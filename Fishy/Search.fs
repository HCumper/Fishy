module Search
// This module is gnarly but it can't be helped

open System
open BoardHelpers
open Types
open GenerateMoves
open MakeMove
open Evaluation
open BoardHelpers.Attacks
open Uci
open TranspositionTable

// scores are always from the SIDE-TO-MOVE viewpoint.
// Negamax relies on this convention. After making a move and switching side,
// the returned child score is negated.

type EvalFn = Position -> int

// Search-wide instrumentation/state used for UCI info output.
// These are reset once per root search in chooseBestMove.
let mutable nodeCount = 0L
let mutable lastInfoTime = 0L
let mutable searchStartTime = 0L
let mutable currentRootScore = 0
let infoIntervalMs = 500L   // send periodic info at most twice per second
let defaultSearchDepth = 60
let mutable iidSearchCount = 0
let mutable aspirationResearchCount = 0

let mutable abortSearch = false
let mutable softTimeUp = false

[<Literal>]
let private MaxKillerPly = 128

[<Literal>]
let private IidMinDepth = 4

[<Literal>]
let private IidReduction = 2

[<Literal>]
let private AspirationInitialWindow = 50

let private killerMoves : int32[,] =
    Array2D.zeroCreate MaxKillerPly 2

let private historyScores : int[,,] =
    Array3D.zeroCreate 2 64 64

type TimeBudget =
    { SoftMs: int64
      HardMs: int64 }

let inline private otherColor (c: Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

/// Mate score convention.
/// Must be safely outside the normal evaluation range so mates always dominate
/// static eval terms and search heuristics.
[<Literal>]
let MateScore = 30000

/// Small offset used in the mate score formula so closer mates score better
/// than more distant mates.
[<Literal>]
let InCheckPenalty = 100

[<Literal>]
let private AspirationMaxWindow = MateScore + 1

let negInf = -MateScore - 1

let inline private elapsedMs (stopwatch: Diagnostics.Stopwatch) =
    stopwatch.ElapsedMilliseconds

let inline private checkTime (budget: TimeBudget) (stopwatch: Diagnostics.Stopwatch) =
    let t = elapsedMs stopwatch
    if t >= budget.HardMs then
        abortSearch <- true
    elif t >= budget.SoftMs then
        softTimeUp <- true

// --------------------
// TT helpers
// --------------------

/// Zobrist key for the current position.
///
/// This module assumes pos.State.HashKey is kept fully synchronized with board
/// and state changes by makeMove/unmakeMove. If that invariant is broken,
/// TT hits, move ordering, and repetition-related behavior all become unreliable.
let inline private keyOfPos (pos: Position) : uint64 =
    uint64 pos.State.HashKey

/// Clamp a search score into the int16 range used by the TT entry.
/// TT storage is intentionally compact; extreme values outside int16 are clipped.
let inline private clamp16 (x:int) : int16 =
    if x < int Int16.MinValue then Int16.MinValue
    elif x > int Int16.MaxValue then Int16.MaxValue
    else int16 x

/// Pack a move into a compact int32 identity for TT storage/order hints.
///
/// Bit layout:
///   from square : 6 bits
///   to square   : 6 bits
///   promotion   : 4 bits
///
/// This is used for move identity and ordering only, not as a full serialized
/// move representation.
let internal packMove (mv: Move) : int32 =
    let fromSq = int mv.From.File + 8 * int mv.From.Rank
    let toSq   = int mv.To.File   + 8 * int mv.To.Rank
    let promo  = (int mv.PromoteTo) &&& 0xF
    int32 (fromSq ||| (toSq <<< 6) ||| (promo <<< 12))

/// True when a TT-stored packed move matches a generated legal move.
let inline private isPackedMoveMatch (packed:int32) (mv:Move) =
    packed <> 0 && packed = packMove mv

let internal clearKillerMoves () =
    for ply = 0 to MaxKillerPly - 1 do
        killerMoves.[ply, 0] <- 0
        killerMoves.[ply, 1] <- 0

let internal clearHistoryScores () =
    Array.Clear(historyScores, 0, historyScores.Length)

let private rememberKiller (ply:int) (mv:Move) =
    if ply >= 0 && ply < MaxKillerPly then
        let packed = packMove mv
        if packed <> 0 && killerMoves.[ply, 0] <> packed then
            killerMoves.[ply, 1] <- killerMoves.[ply, 0]
            killerMoves.[ply, 0] <- packed

let internal rememberKillerForTests ply mv =
    rememberKiller ply mv

let inline private squareIndex (sq: Coordinates) =
    int sq.File + 8 * int sq.Rank

let inline private colorIndex (c: Color) =
    if c = Color.White then 0 else 1

let private historyScore (side: Color) (mv: Move) =
    historyScores.[colorIndex side, squareIndex mv.From, squareIndex mv.To]

let private rememberHistory (side: Color) (depth: int) (mv: Move) =
    let ci = colorIndex side
    let fromSq = squareIndex mv.From
    let toSq = squareIndex mv.To
    let bonus = max 1 (depth * depth)
    historyScores.[ci, fromSq, toSq] <- min 1_000_000 (historyScores.[ci, fromSq, toSq] + bonus)

let internal rememberHistoryForTests side depth mv =
    rememberHistory side depth mv

/// Root-only helper used by iterative deepening.
///
/// Promotes the previous iteration's best move so the next iteration searches
/// the most likely PV move first. This is one of the main benefits of
/// iterative deepening.
let private putMoveFirst (preferred: Move) (moves: Move list) =
    let first, rest = moves |> List.partition (fun m -> m = preferred)
    first @ rest

/// Attempts to use a TT probe result as an immediate search result.
///
/// A TT entry is only usable if its stored depth is at least the current
/// search depth. Bound semantics:
///
///   BoundExact -> exact node score; always reusable
///   BoundLower -> reusable only when it proves a fail-high against beta
///   BoundUpper -> reusable only when it proves a fail-low against alpha
///
/// Returns ValueSome score only when the TT fully proves the node result.
/// Otherwise returns ValueNone and normal search continues.
let inline private tryUseTT (pr:ProbeResult) (depth:int) (alpha:int) (beta:int) : int voption =
    if pr.Hit then
        if int pr.Entry.Depth >= depth then
            let s = int pr.Entry.Score
            match pr.Entry.Bound with
            | BoundExact ->
                TranspositionTable.markUseful()
                ValueSome s
            | BoundLower when s >= beta ->
                TranspositionTable.markUseful()
                ValueSome s
            | BoundUpper when s <= alpha ->
                TranspositionTable.markUseful()
                ValueSome s
            | _ ->
                ValueNone
        else
            // TT hit, but not deep enough to be trusted as a cutoff at this node.
            TranspositionTable.markDepthReject()
            ValueNone
    else
        ValueNone

/// Maximum number of moves to emit in a GUI PV line.
/// This is just a presentation limit, not a search limit.
[<Literal>]
let MaxPvLength = 32

/// Try to find the actual legal move corresponding to a packed TT move.
let private tryFindPackedMove (packed:int32) (moves: Move list) : Move voption =
    if packed = 0 then
        ValueNone
    else
        moves
        |> List.tryFind (fun mv -> packMove mv = packed)
        |> function
           | Some mv -> ValueSome mv
           | None -> ValueNone

/// Extract a principal variation by following TT moves from the given position.
let extractPv (tt: TranspositionTable) (rootPos: Position) (maxLen:int) : Move list =
    let rec loop (pos: Position) (remaining:int) (visited:Set<uint64>) (acc: Move list) =
        if remaining <= 0 then
            List.rev acc
        else
            let key = keyOfPos pos

            if visited.Contains key then
                List.rev acc
            else
                let pr = probe tt key
                if not pr.Hit || pr.Entry.Move = 0 then
                    List.rev acc
                else
                    let legalMoves = generateAllLegalMoves pos inCheck
                    match tryFindPackedMove pr.Entry.Move legalMoves with
                    | ValueNone ->
                        List.rev acc
                    | ValueSome mv ->
                        let mutable p = pos
                        let undo = makeMove &p mv

                        let acc' = mv :: acc
                        let visited' = visited.Add key

                        let result =
                            loop p (remaining - 1) visited' acc'

                        unmakeMove &p mv undo
                        result

    loop rootPos maxLen Set.empty []

// --------------------
// Time management
// --------------------

let private computeTimeBudget (pos: Position) (req: SearchRequest) : TimeBudget =
    match req.MoveTime with
    | ValueSome mt when mt > 0 ->
        let hard = int64 mt
        let soft = max 1L (hard * 9L / 10L)
        { SoftMs = soft; HardMs = hard }

    | _ ->
        let myTimeOpt, myIncOpt =
            match pos.State.ToPlay with
            | Color.White -> req.WTime, req.WInc
            | Color.Black -> req.BTime, req.BInc
            | _ -> req.WTime, req.WInc

        match myTimeOpt with
        | ValueSome myTime when myTime > 0 ->
            let inc =
                match myIncOpt with
                | ValueSome x when x > 0 -> x
                | _ -> 0

            let movesToGo =
                match req.MovesToGo with
                | ValueSome n when n > 0 -> n
                | _ -> 30

            let baseTime =
                (myTime / movesToGo) + (inc * 3 / 4)

            let hard =
                baseTime
                |> max 10
                |> min (myTime / 2)
                |> int64

            let soft = max 1L (hard * 7L / 10L)
            { SoftMs = soft; HardMs = hard }

        | _ ->
            // No usable clock info: fall back to a fixed small budget.
            { SoftMs = 1000L; HardMs = 5000L }

// --------------------
// Move ordering helpers
// --------------------

/// Material scale used only for MVV-LVA capture ordering.
/// This does not need to match evaluation exactly; it is only a move-ordering
/// heuristic.
let inline private pieceValueFromCode (p:sbyte) : int =
    match PieceCode.absKind p with
    | PieceCode.Pawn   -> 100
    | PieceCode.Knight -> 320
    | PieceCode.Bishop -> 330
    | PieceCode.Rook   -> 500
    | PieceCode.Queen  -> 900
    | PieceCode.King   -> 20000
    | _ -> 0

/// Returns the moving piece from the current board position.
let inline private movingPieceAt (pos: Position) (mv: Move) : sbyte =
    Board.getSq pos.Board mv.From

/// Best-effort captured-piece detection for move ordering.
let inline private capturedPieceOf (pos: Position) (mv: Move) : sbyte =
    let dst = Board.getSq pos.Board mv.To
    if dst <> PieceCode.Empty then dst else PieceCode.Empty

let inline private isEnPassantCapture (pos: Position) (mv: Move) : bool =
    PieceCode.absKind mv.Piece = PieceCode.Pawn
    && int mv.From.File <> int mv.To.File
    && capturedPieceOf pos mv = PieceCode.Empty
    && match pos.State.EPSquare with
       | ValueSome ep -> ep.File = mv.To.File && ep.Rank = mv.To.Rank
       | ValueNone -> false

/// True for ordinary captures detectable from the current destination square.
let inline private isCapture (pos: Position) (mv: Move) : bool =
    capturedPieceOf pos mv <> PieceCode.Empty || isEnPassantCapture pos mv

let inline private isQuietKillerCandidate (pos: Position) (mv: Move) : bool =
    not (isCapture pos mv) && mv.PromoteTo = PieceCode.Empty

/// MVV-LVA score used to sort captures:
/// Most Valuable Victim, Least Valuable Attacker.
let inline private mvvLvaScore (pos: Position) (mv: Move) : int =
    let victim = capturedPieceOf pos mv
    if victim = PieceCode.Empty then Int32.MinValue
    else
        let attacker = movingPieceAt pos mv
        pieceValueFromCode victim * 100 - pieceValueFromCode attacker

let inline private onBoard (file:int) (rank:int) =
    file >= MinFileRank && file <= MaxFileRank &&
    rank >= MinFileRank && rank <= MaxFileRank

let inline private isColorPiece (side: Color) (p: sbyte) =
    p <> PieceCode.Empty &&
    ((side = Color.White && PieceCode.isWhite p) ||
     (side = Color.Black && PieceCode.isBlack p))

let private leastValuableAttacker (board: Board) (target: Coordinates) (side: Color) : (Coordinates * sbyte) voption =
    let tf = int target.File
    let tr = int target.Rank

    let mutable bestSq = Unchecked.defaultof<Coordinates>
    let mutable bestPiece = PieceCode.Empty
    let mutable bestValue = Int32.MaxValue

    let inline consider (sq: Coordinates) =
        let p = Board.getSq board sq
        if isColorPiece side p then
            let value = pieceValueFromCode p
            if value < bestValue then
                bestSq <- sq
                bestPiece <- p
                bestValue <- value

    let inline considerKind (sq: Coordinates) (kind: sbyte) =
        let p = Board.getSq board sq
        if isColorPiece side p && PieceCode.absKind p = kind then
            let value = pieceValueFromCode p
            if value < bestValue then
                bestSq <- sq
                bestPiece <- p
                bestValue <- value

    let pawnSourceRank = if side = Color.White then tr - 1 else tr + 1
    for df in [| -1; 1 |] do
        let f = tf + df
        if onBoard f pawnSourceRank then
            considerKind { File = byte f; Rank = byte pawnSourceRank } PieceCode.Pawn

    for df, dr in [| (1, 2); (2, 1); (-1, 2); (-2, 1); (1, -2); (2, -1); (-1, -2); (-2, -1) |] do
        let f = tf + df
        let r = tr + dr
        if onBoard f r then
            considerKind { File = byte f; Rank = byte r } PieceCode.Knight

    for df, dr in [| (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) |] do
        let f = tf + df
        let r = tr + dr
        if onBoard f r then
            considerKind { File = byte f; Rank = byte r } PieceCode.King

    let inline scan (df:int) (dr:int) (kind1:sbyte) (kind2:sbyte) =
        let mutable f = tf + df
        let mutable r = tr + dr
        let mutable blocked = false
        while not blocked && onBoard f r do
            let sq = { File = byte f; Rank = byte r }
            let p = Board.getSq board sq
            if p = PieceCode.Empty then
                f <- f + df
                r <- r + dr
            else
                if isColorPiece side p then
                    let k = PieceCode.absKind p
                    if k = kind1 || k = kind2 then
                        consider sq
                blocked <- true

    for df, dr in [| (1, 1); (1, -1); (-1, 1); (-1, -1) |] do
        scan df dr PieceCode.Bishop PieceCode.Queen

    for df, dr in [| (1, 0); (-1, 0); (0, 1); (0, -1) |] do
        scan df dr PieceCode.Rook PieceCode.Queen

    if bestPiece = PieceCode.Empty then ValueNone
    else ValueSome (bestSq, bestPiece)

let internal see (pos: Position) (mv: Move) : int =
    if not (isCapture pos mv) then
        0
    else
        let board = pos.Board.Clone() :?> Board
        let target = mv.To
        let movingPiece = Board.getSq board mv.From
        let capturedPiece =
            if isEnPassantCapture pos mv then
                Board.getFR board (int mv.To.File) (int mv.From.Rank)
            else
                Board.getSq board target

        let promotedPiece =
            if mv.PromoteTo <> PieceCode.Empty then mv.PromoteTo else movingPiece

        let promotionGain =
            if mv.PromoteTo <> PieceCode.Empty then
                pieceValueFromCode promotedPiece - pieceValueFromCode movingPiece
            else
                0

        let gains = ResizeArray<int>(8)
        gains.Add(pieceValueFromCode capturedPiece + promotionGain)

        Board.setSq board mv.From PieceCode.Empty
        if isEnPassantCapture pos mv then
            Board.setFR board (int mv.To.File) (int mv.From.Rank) PieceCode.Empty
        Board.setSq board target promotedPiece

        let mutable side = otherColor pos.State.ToPlay
        let mutable pieceOnTarget = promotedPiece
        let mutable done_ = false

        while not done_ do
            match leastValuableAttacker board target side with
            | ValueNone ->
                done_ <- true
            | ValueSome (fromSq, attacker) ->
                gains.Add(pieceValueFromCode pieceOnTarget - gains.[gains.Count - 1])
                Board.setSq board fromSq PieceCode.Empty
                Board.setSq board target attacker
                pieceOnTarget <- attacker
                side <- otherColor side

        for i = gains.Count - 2 downto 0 do
            gains.[i] <- -max (-gains.[i]) gains.[i + 1]

        gains.[0]

let private captureOrderScore (pos: Position) (mv: Move) =
    see pos mv * 100000 + mvvLvaScore pos mv

/// Non-root move ordering policy:
///
///   1. TT move first, if present in the generated move list
///   2. Remaining captures sorted by MVV-LVA
///   3. Remaining quiet moves in generator order
let internal orderMovesWithKillers
    (pos: Position)
    (ttMovePacked:int32)
    (killer1Packed:int32)
    (killer2Packed:int32)
    (moves0: Move list)
    : Move list =

    let ttFirst, rest =
        if ttMovePacked = 0 then [], moves0
        else moves0 |> List.partition (isPackedMoveMatch ttMovePacked)

    let captures, quiets =
        rest |> List.partition (isCapture pos)

    let capturesSorted =
        captures |> List.sortByDescending (captureOrderScore pos)

    let killer1, restAfterK1 =
        if killer1Packed = 0 then [], quiets
        else quiets |> List.partition (isPackedMoveMatch killer1Packed)

    let killer2, quietRest =
        if killer2Packed = 0 || killer2Packed = killer1Packed then [], restAfterK1
        else restAfterK1 |> List.partition (isPackedMoveMatch killer2Packed)

    let quietsByHistory =
        quietRest |> List.sortByDescending (historyScore pos.State.ToPlay)

    ttFirst @ capturesSorted @ killer1 @ killer2 @ quietsByHistory

let internal orderMoves (pos: Position) (ttMovePacked:int32) (moves0: Move list) : Move list =
    orderMovesWithKillers pos ttMovePacked 0 0 moves0

/// Capture-only ordering for quiescence.
let internal orderQMoves (pos: Position) (ttMovePacked:int32) (moves0: Move list) : Move list =
    let ttFirst, rest =
        if ttMovePacked = 0 then [], moves0
        else moves0 |> List.partition (isPackedMoveMatch ttMovePacked)

    let capturesSorted =
        rest |> List.sortByDescending (captureOrderScore pos)

    ttFirst @ capturesSorted

/// Root-only move ordering for iterative deepening.
let internal orderRootMoves
    (pos: Position)
    (pvMove: Move voption)
    (ttMovePacked: int32)
    (moves0: Move list)
    : Move list =

    let pvFirst, rest1 =
        match pvMove with
        | ValueSome pv ->
            moves0 |> List.partition (fun m -> m = pv)
        | ValueNone ->
            [], moves0

    let ttFirst, rest2 =
        if ttMovePacked = 0 then
            [], rest1
        else
            rest1 |> List.partition (isPackedMoveMatch ttMovePacked)

    let captures, quiets =
        rest2 |> List.partition (isCapture pos)

    let capturesSorted =
        captures |> List.sortByDescending (captureOrderScore pos)

    pvFirst @ ttFirst @ capturesSorted @ quiets

// =============================
// Quiescence
// =============================

let rec quiescence
    (tt: TranspositionTable)
    (pos: Position)
    (alpha: int)
    (beta: int)
    (stopwatch: Diagnostics.Stopwatch)
    (budget: TimeBudget)
    : int =

    nodeCount <- nodeCount + 1L

    if (nodeCount &&& 1023L) = 0L then
        checkTime budget stopwatch

    if abortSearch then
        evaluate pos
    else
        let alphaOrig = alpha
        let key = keyOfPos pos
        let pr = probe tt key
        let side = pos.State.ToPlay
        let isInCheck = inCheck pos side

        match tryUseTT pr 0 alpha beta with
        | ValueSome score ->
            score
        | ValueNone ->
            let mutable a = alpha
            let mutable bestMovePacked = 0
            let mutable best = negInf

            if not isInCheck then
                let standPat = evaluate pos

                if standPat >= beta then
                    store tt key 0 (clamp16 standPat) (clamp16 standPat) 0 BoundLower 0uy
                    standPat
                else
                    if standPat > a then
                        a <- standPat
                    best <- standPat

                    let ttMovePacked =
                        if pr.Hit then pr.Entry.Move else 0

                    let captures =
                        generateAllLegalCaptures pos inCheck
                        |> orderQMoves pos ttMovePacked

                    let mutable cutoff = false

                    for mv in captures do
                        if not cutoff && not abortSearch then
                            let mutable p = pos
                            let undo = makeMove &p mv

                            let score =
                                -(quiescence tt p (-beta) (-a) stopwatch budget)

                            unmakeMove &p mv undo

                            if not abortSearch then
                                if score > best then
                                    best <- score
                                    bestMovePacked <- packMove mv

                                if score > a then
                                    a <- score

                                if a >= beta then
                                    cutoff <- true

                    if abortSearch then
                        best
                    else
                        let bound =
                            if best <= alphaOrig then BoundUpper
                            elif best >= beta then BoundLower
                            else BoundExact

                        store tt key bestMovePacked (clamp16 best) (clamp16 best) 0 bound 0uy
                        best

            else
                let moves0 = generateAllLegalMoves pos inCheck

                if List.isEmpty moves0 then
                    let sc = -MateScore + InCheckPenalty
                    store tt key 0 (clamp16 sc) (clamp16 sc) 0 BoundExact 0uy
                    sc
                else
                    let ttMovePacked =
                        if pr.Hit then pr.Entry.Move else 0

                    let moves = orderMoves pos ttMovePacked moves0
                    let mutable cutoff = false

                    for mv in moves do
                        if not cutoff && not abortSearch then
                            let mutable p = pos
                            let undo = makeMove &p mv

                            let score =
                                -(quiescence tt p (-beta) (-a) stopwatch budget)

                            unmakeMove &p mv undo

                            if not abortSearch then
                                if score > best then
                                    best <- score
                                    bestMovePacked <- packMove mv

                                if score > a then
                                    a <- score

                                if a >= beta then
                                    cutoff <- true

                    if abortSearch then
                        best
                    else
                        let bound =
                            if best <= alphaOrig then BoundUpper
                            elif best >= beta then BoundLower
                            else BoundExact

                        store tt key bestMovePacked (clamp16 best) (clamp16 best) 0 bound 0uy
                        best

// =============================
// Search (Negamax + TT)
// =============================
// Fishy's heart
let rec private negamaxAtPly
    (tt: TranspositionTable)
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    (stopwatch: Diagnostics.Stopwatch)
    (budget: TimeBudget)
    (ply: int)
    : int =

    nodeCount <- nodeCount + 1L

    if (nodeCount &&& 1023L) = 0L then
        checkTime budget stopwatch

    if abortSearch then
        evaluate pos
    else
        let alphaOrig = alpha
        let key = keyOfPos pos

        // Avoid expanding the node if it is already in the TT.
        let pr = probe tt key

        match tryUseTT pr depth alpha beta with
        | ValueSome score ->
            score

        | ValueNone ->
            if depth <= 0 then
                quiescence tt pos alpha beta stopwatch budget
            else
                let side = pos.State.ToPlay
                let moves0 = generateAllLegalMoves pos inCheck

                if List.isEmpty moves0 then
                    let sc =
                        if inCheck pos side then
                            // Checkmate: prefer shorter mates.
                            -MateScore + (InCheckPenalty - depth)
                        else
                            // Stalemate/draw.
                            0

                    store tt key 0 (clamp16 sc) (clamp16 sc) depth BoundExact 0uy
                    sc
                else
                    let mutable ttMovePacked =
                        if pr.Hit then pr.Entry.Move else 0

                    if ttMovePacked = 0
                       && depth >= IidMinDepth
                       && not abortSearch
                       && not (inCheck pos side) then
                        iidSearchCount <- iidSearchCount + 1
                        let iidDepth = max 1 (depth - IidReduction)
                        let _ = negamaxAtPly tt pos iidDepth alpha beta stopwatch budget ply

                        if not abortSearch then
                            let iidProbe = probe tt key
                            if iidProbe.Hit then
                                ttMovePacked <- iidProbe.Entry.Move

                    let mutable a = alpha
                    let mutable best = negInf
                    let mutable bestMovePacked = 0
                    let mutable cutoff = false

                    let killer1Packed =
                        if ply >= 0 && ply < MaxKillerPly then killerMoves.[ply, 0] else 0
                    let killer2Packed =
                        if ply >= 0 && ply < MaxKillerPly then killerMoves.[ply, 1] else 0

                    let moves = orderMovesWithKillers pos ttMovePacked killer1Packed killer2Packed moves0

                    for mv in moves do
                        if not cutoff && not abortSearch then
                            let mutable p = pos
                            let undo = makeMove &p mv

                            let score =
                                -(negamaxAtPly tt p (depth - 1) (-beta) (-a) stopwatch budget (ply + 1))

                            unmakeMove &p mv undo

                            if not abortSearch then
                                if score > best then
                                    best <- score
                                    bestMovePacked <- packMove mv

                                if score > a then
                                    a <- score

                                if a >= beta then
                                    if isQuietKillerCandidate pos mv then
                                        rememberKiller ply mv
                                        rememberHistory pos.State.ToPlay depth mv
                                    cutoff <- true

                    if abortSearch then
                        best
                    else
                        let bound =
                            if best <= alphaOrig then BoundUpper
                            elif best >= beta then BoundLower
                            else BoundExact

                        store tt key bestMovePacked (clamp16 best) (clamp16 best) depth bound 0uy
                        best

let negamax
    (tt: TranspositionTable)
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    (stopwatch: Diagnostics.Stopwatch)
    (budget: TimeBudget)
    : int =

    abortSearch <- false
    softTimeUp <- false
    negamaxAtPly tt pos depth alpha beta stopwatch budget 0

/// Extracts search depth from the UCI request.
/// Defaults to a small fixed depth when no explicit depth is provided.
let private depthFromRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> d
    | _ -> defaultSearchDepth

let private isFixedDepthRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> true
    | _ -> false

let private shouldStartNextIteration
    (budget: TimeBudget)
    (elapsedNow: int64)
    (prevCompletedElapsed: int64)
    (lastCompletedElapsed: int64)
    : bool =

    let remainingSoft = budget.SoftMs - elapsedNow

    if remainingSoft <= 0L then
        false
    else
        let lastIterTime = elapsedNow - lastCompletedElapsed

        if lastIterTime <= 0L then
            true
        else
            let predictedNext =
                if prevCompletedElapsed > 0L && lastCompletedElapsed > prevCompletedElapsed then
                    let prevIterTime = lastCompletedElapsed - prevCompletedElapsed
                    if prevIterTime > 0L then
                        let ratioBased = lastIterTime * lastIterTime / prevIterTime
                        max (lastIterTime * 2L) ratioBased
                    else
                        lastIterTime * 2L
                else
                    lastIterTime * 2L

            predictedNext <= remainingSoft
            
/// Root search entry point.
let chooseBestMove (tt: TranspositionTable) (pos: Position) (req: SearchRequest) : Move voption =
    let targetDepth = depthFromRequest req
    let fixedDepth = isFixedDepthRequest req
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let budget = computeTimeBudget pos req

    searchStartTime <- 0L
    lastInfoTime <- 0L
    nodeCount <- 0L
    currentRootScore <- 0
    iidSearchCount <- 0
    aspirationResearchCount <- 0
    abortSearch <- false
    softTimeUp <- false
    clearKillerMoves()
    clearHistoryScores()

    // One new TT generation per root search.
    newSearch tt

    let rootMoves0 = generateAllLegalMoves pos inCheck

    match rootMoves0 with
    | [] -> ValueNone
    | _ ->
        let rec iterate
            (depth:int)
            (bestMoveOverall: Move voption)
            (bestScoreOverall:int)
            (rootMoves: Move list)
            (prevCompletedElapsed:int64)
            (lastCompletedElapsed:int64)
            : Move voption =

            if depth > targetDepth || abortSearch || (softTimeUp && not fixedDepth) then
                bestMoveOverall
            else
                let fullAlpha = -MateScore - 1
                let fullBeta = MateScore + 1

                let rootKey = keyOfPos pos

                let searchRootWindow (alphaStart:int) (beta:int) =
                    let rootProbe = probe tt rootKey
                    let ttMovePacked =
                        if rootProbe.Hit then rootProbe.Entry.Move else 0

                    let orderedMoves =
                        orderRootMoves pos bestMoveOverall ttMovePacked rootMoves

                    let mutable bestMoveThisIter = ValueNone
                    let mutable bestScoreThisIter = negInf
                    let mutable alpha = alphaStart
                    let mutable cutoff = false

                    for mv in orderedMoves do
                        if not abortSearch && not cutoff then
                            let mutable p = pos
                            let undo = makeMove &p mv

                            let score =
                                -(negamaxAtPly tt p (depth - 1) (-beta) (-alpha) stopwatch budget 1)

                            unmakeMove &p mv undo

                            if not abortSearch then
                                if score > bestScoreThisIter then
                                    bestScoreThisIter <- score
                                    bestMoveThisIter <- ValueSome mv

                                if score > alpha then
                                    alpha <- score

                                if alpha >= beta then
                                    cutoff <- true

                    bestMoveThisIter, bestScoreThisIter

                let rec searchWithAspiration (alpha:int) (beta:int) (window:int) =
                    let bestMoveThisIter, bestScoreThisIter = searchRootWindow alpha beta

                    if abortSearch then
                        bestMoveThisIter, bestScoreThisIter
                    else
                        match bestMoveThisIter with
                        | ValueNone ->
                            bestMoveThisIter, bestScoreThisIter
                        | ValueSome _ when bestScoreThisIter <= alpha && alpha > fullAlpha ->
                            aspirationResearchCount <- aspirationResearchCount + 1
                            let nextWindow = min AspirationMaxWindow (window * 2)
                            let nextAlpha = max fullAlpha (bestScoreThisIter - nextWindow)
                            searchWithAspiration nextAlpha beta nextWindow
                        | ValueSome _ when bestScoreThisIter >= beta && beta < fullBeta ->
                            aspirationResearchCount <- aspirationResearchCount + 1
                            let nextWindow = min AspirationMaxWindow (window * 2)
                            let nextBeta = min fullBeta (bestScoreThisIter + nextWindow)
                            searchWithAspiration alpha nextBeta nextWindow
                        | _ ->
                            bestMoveThisIter, bestScoreThisIter

                let initialAlpha, initialBeta, initialWindow =
                    if depth <= 1 || bestMoveOverall = ValueNone then
                        fullAlpha, fullBeta, AspirationMaxWindow
                    else
                        max fullAlpha (bestScoreOverall - AspirationInitialWindow),
                        min fullBeta (bestScoreOverall + AspirationInitialWindow),
                        AspirationInitialWindow

                let bestMoveThisIter, bestScoreThisIter =
                    searchWithAspiration initialAlpha initialBeta initialWindow

                match bestMoveThisIter with
                | ValueSome bm when not abortSearch ->
                    let bestMoveOverall' = ValueSome bm
                    let bestScoreOverall' = bestScoreThisIter
                    currentRootScore <- bestScoreOverall'

                    store tt rootKey (packMove bm) (clamp16 bestScoreThisIter) (clamp16 bestScoreThisIter) depth BoundExact 0uy

                    let rootMoves' = putMoveFirst bm rootMoves

                    let now = stopwatch.ElapsedMilliseconds
                    let nps = if now > 0L then nodeCount * 1000L / now else 0L

                    let pvMoves = extractPv tt pos MaxPvLength
                    let pvText =
                        match pvMoves with
                        | [] -> moveToUci bm
                        | xs -> pvToUciString xs

                    writeInfo depth nodeCount nps now bestScoreOverall' pvText

                    // Existing soft-stop rule: if soft limit already reached, stop.
                    if now >= budget.SoftMs && not fixedDepth then
                        softTimeUp <- true

                    // Predict whether starting the next iteration is worthwhile.
                    let startNext =
                        if fixedDepth then
                            depth < targetDepth
                        else
                            not softTimeUp
                            && shouldStartNextIteration budget now prevCompletedElapsed lastCompletedElapsed

                    if startNext then
                        iterate
                            (depth + 1)
                            bestMoveOverall'
                            bestScoreOverall'
                            rootMoves'
                            lastCompletedElapsed
                            now
                    else
                        bestMoveOverall'

                | _ ->
                    bestMoveOverall

        iterate 1 ValueNone negInf rootMoves0 0L 0L

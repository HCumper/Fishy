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
let defaultSearchDepth = 6

let mutable abortSearch = false
let mutable softTimeUp = false

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

/// True for ordinary captures detectable from the current destination square.
let inline private isCapture (pos: Position) (mv: Move) : bool =
    capturedPieceOf pos mv <> PieceCode.Empty

/// MVV-LVA score used to sort captures:
/// Most Valuable Victim, Least Valuable Attacker.
let inline private mvvLvaScore (pos: Position) (mv: Move) : int =
    let victim = capturedPieceOf pos mv
    if victim = PieceCode.Empty then Int32.MinValue
    else
        let attacker = movingPieceAt pos mv
        pieceValueFromCode victim * 100 - pieceValueFromCode attacker

/// Non-root move ordering policy:
///
///   1. TT move first, if present in the generated move list
///   2. Remaining captures sorted by MVV-LVA
///   3. Remaining quiet moves in generator order
let internal orderMoves (pos: Position) (ttMovePacked:int32) (moves0: Move list) : Move list =
    let ttFirst, rest =
        if ttMovePacked = 0 then [], moves0
        else moves0 |> List.partition (isPackedMoveMatch ttMovePacked)

    let captures, quiets =
        rest |> List.partition (isCapture pos)

    let capturesSorted =
        captures |> List.sortByDescending (mvvLvaScore pos)

    ttFirst @ capturesSorted @ quiets

/// Capture-only ordering for quiescence.
let internal orderQMoves (pos: Position) (ttMovePacked:int32) (moves0: Move list) : Move list =
    let ttFirst, rest =
        if ttMovePacked = 0 then [], moves0
        else moves0 |> List.partition (isPackedMoveMatch ttMovePacked)

    let capturesSorted =
        rest |> List.sortByDescending (mvvLvaScore pos)

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
        captures |> List.sortByDescending (mvvLvaScore pos)

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
let rec negamax
    (tt: TranspositionTable)
    (pos: Position)
    (depth: int)
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
                    let ttMovePacked =
                        if pr.Hit then pr.Entry.Move else 0

                    let mutable a = alpha
                    let mutable best = negInf
                    let mutable bestMovePacked = 0
                    let mutable cutoff = false

                    let moves = orderMoves pos ttMovePacked moves0

                    for mv in moves do
                        if not cutoff && not abortSearch then
                            let mutable p = pos
                            let undo = makeMove &p mv

                            let score =
                                -(negamax tt p (depth - 1) (-beta) (-a) stopwatch budget)

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

                        store tt key bestMovePacked (clamp16 best) (clamp16 best) depth bound 0uy
                        best

/// Extracts search depth from the UCI request.
/// Defaults to a small fixed depth when no explicit depth is provided.
let private depthFromRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> d
    | _ -> defaultSearchDepth

/// Root search entry point.
let chooseBestMove (tt: TranspositionTable) (pos: Position) (req: SearchRequest) : Move voption =
    let targetDepth = depthFromRequest req
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let budget = computeTimeBudget pos req

    searchStartTime <- 0L
    lastInfoTime <- 0L
    nodeCount <- 0L
    currentRootScore <- 0
    abortSearch <- false
    softTimeUp <- false

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
            : Move voption =

            if depth > targetDepth || abortSearch || softTimeUp then
                bestMoveOverall
            else
                let mutable bestMoveThisIter = ValueNone
                let mutable bestScoreThisIter = negInf
                let mutable alpha = -MateScore - 1
                let beta = MateScore + 1

                let rootKey = keyOfPos pos
                let rootProbe = probe tt rootKey
                let ttMovePacked =
                    if rootProbe.Hit then rootProbe.Entry.Move else 0

                let orderedMoves =
                    orderRootMoves pos bestMoveOverall ttMovePacked rootMoves

                for mv in orderedMoves do
                    if not abortSearch then
                        let mutable p = pos
                        let undo = makeMove &p mv

                        let score =
                            -(negamax tt p (depth - 1) (-beta) (-alpha) stopwatch budget)

                        unmakeMove &p mv undo

                        if not abortSearch then
                            if score > bestScoreThisIter then
                                bestScoreThisIter <- score
                                bestMoveThisIter <- ValueSome mv

                            if score > alpha then
                                alpha <- score

                match bestMoveThisIter with
                | ValueSome bm when not abortSearch ->
                    let bestMoveOverall' = ValueSome bm
                    let bestScoreOverall' = bestScoreThisIter
                    currentRootScore <- bestScoreOverall'

                    let rootMoves' = putMoveFirst bm rootMoves

                    let now = stopwatch.ElapsedMilliseconds
                    let nps = if now > 0L then nodeCount * 1000L / now else 0L

                    let pvMoves = extractPv tt pos MaxPvLength
                    let pvText =
                        match pvMoves with
                        | [] -> moveToUci bm
                        | xs -> pvToUciString xs

                    writeInfo depth nodeCount nps now bestScoreOverall' pvText

                    if now >= budget.SoftMs then
                        softTimeUp <- true

                    iterate (depth + 1) bestMoveOverall' bestScoreOverall' rootMoves'

                | _ ->
                    bestMoveOverall

        iterate 1 ValueNone negInf rootMoves0
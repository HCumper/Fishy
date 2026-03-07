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

// Evaluation convention used throughout this module:
// scores are always from the SIDE-TO-MOVE viewpoint.
//
// That means:
//   + score => good for pos.State.ToPlay
//   - score => bad for pos.State.ToPlay
//
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
///
/// This currently infers captures from destination occupancy, so en passant is
/// not recognized as a capture unless the Move type carries explicit capture
/// information elsewhere. That does not affect correctness, only ordering.
let inline private capturedPieceOf (pos: Position) (mv: Move) : sbyte =
    let dst = Board.getSq pos.Board mv.To
    if dst <> PieceCode.Empty then dst else PieceCode.Empty

/// True for ordinary captures detectable from the current destination square.
let inline private isCapture (pos: Position) (mv: Move) : bool =
    capturedPieceOf pos mv <> PieceCode.Empty

/// MVV-LVA score used to sort captures:
/// Most Valuable Victim, Least Valuable Attacker.
///
/// Higher score = search earlier.
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
///
/// This is intentionally simple and cheap. Killer/history heuristics are not
/// yet applied.
let internal orderMoves (pos: Position) (ttMovePacked:int32) (moves0: Move list) : Move list =
    let ttFirst, rest =
        if ttMovePacked = 0 then [], moves0
        else moves0 |> List.partition (isPackedMoveMatch ttMovePacked)

    let captures, quiets =
        rest |> List.partition (isCapture pos)

    let capturesSorted =
        captures |> List.sortByDescending (mvvLvaScore pos)

    ttFirst @ capturesSorted @ quiets

/// Root-only move ordering for iterative deepening:
///
///   1. Previous iteration PV/best move first
///   2. TT move next, if different
///   3. Remaining captures by MVV-LVA
///   4. Remaining quiet moves in generator order
///
/// Keeping the previous root best move first is usually the strongest ordering
/// signal available at the next iteration.
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
// Search (Negamax + TT)
// =============================

/// Fishy's heart
/// Principal recursive search.
///
/// Negamax alpha-beta with TT probe/store and heuristic move ordering.
///
/// Invariants and conventions:
/// - Returned score is always from the side-to-move viewpoint of `pos`.
/// - Child scores are negated on return.
/// - `depth` is remaining search depth in plies.
/// - `alpha`/`beta` are the current node's search window in side-to-move units.
/// - TT entries are used only when the stored depth is sufficient and the bound
///   proves a result.
///
/// Terminal handling:
/// - No legal moves + in check     => mate score
/// - No legal moves + not in check => draw score 0
///
/// Correctness depends on makeMove/unmakeMove fully restoring board, side to
/// move, castling state, EP state, king locations, and hash key.
let rec negamax
    (tt: TranspositionTable)
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    (stopwatch: Diagnostics.Stopwatch)
    : int =

    nodeCount <- nodeCount + 1L

    // Periodic UCI info while searching. This is intentionally throttled.
    if (nodeCount &&& 1023L) = 0L then
        let now = stopwatch.ElapsedMilliseconds
        if now - lastInfoTime >= infoIntervalMs then
            lastInfoTime <- now
            let nps = if now > 0L then nodeCount * 1000L / now else 0L
            writeInfo depth nodeCount nps now currentRootScore

    let alphaOrig = alpha
    let key = keyOfPos pos

    // Probe TT before expanding the node.
    let pr = probe tt key

    match tryUseTT pr depth alpha beta with
    | ValueSome score ->
        score

    | ValueNone ->
        if depth <= 0 then
            // Leaf: static evaluation only.
            // Stored as exact because no further tree search was performed here.
            let sc = evaluate pos
            store tt key 0 (clamp16 sc) (clamp16 sc) 0 BoundExact 0uy
            sc
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
                    if not cutoff then
                        // Search child by make/unmake on a mutable position snapshot.
                        let mutable p = pos
                        let undo = makeMove &p mv

                        let score =
                            -(negamax tt p (depth - 1) (-beta) (-a) stopwatch)

                        unmakeMove &p mv undo

                        if score > best then
                            best <- score
                            bestMovePacked <- packMove mv

                        if score > a then
                            a <- score

                        if a >= beta then
                            cutoff <- true

                // Store node result in TT using the standard alpha/beta bound logic.
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
    | _ -> 6

/// Root search entry point.
///
/// Uses iterative deepening from depth 1 up to the requested depth and returns
/// the best move from the last completed iteration.
///
/// Benefits of iterative deepening here:
/// - previous iteration best move can be searched first at the next depth,
/// - TT entries from shallow searches improve deeper move ordering,
/// - UCI info can be reported after each completed iteration.
///
/// TT generation is advanced once per root search, not once per node.
let chooseBestMove (tt: TranspositionTable) (pos: Position) (req: SearchRequest) : Move voption =
    let targetDepth = depthFromRequest req
    let stopwatch = Diagnostics.Stopwatch.StartNew()

    searchStartTime <- 0L
    lastInfoTime <- 0L
    nodeCount <- 0L
    currentRootScore <- 0

    // One new TT generation per root search.
    newSearch tt

    let rootMoves0 = generateAllLegalMoves pos inCheck

    match rootMoves0 with
    | [] -> ValueNone
    | _ ->
        let mutable bestMoveOverall = ValueNone
        let mutable bestScoreOverall = negInf
        let mutable rootMoves = rootMoves0

        for depth = 1 to targetDepth do
            let mutable bestMoveThisIter = ValueNone
            let mutable bestScoreThisIter = negInf
            let mutable alpha = -MateScore - 1
            let beta = MateScore + 1

            // Root TT probe used only for ordering here.
            let rootKey = keyOfPos pos
            let rootProbe = probe tt rootKey
            let ttMovePacked =
                if rootProbe.Hit then rootProbe.Entry.Move else 0

            let orderedMoves =
                orderRootMoves pos bestMoveOverall ttMovePacked rootMoves

            for mv in orderedMoves do
                let mutable p = pos
                let undo = makeMove &p mv

                let score =
                    -(negamax tt p (depth - 1) (-beta) (-alpha) stopwatch)

                unmakeMove &p mv undo

                if score > bestScoreThisIter then
                    bestScoreThisIter <- score
                    bestMoveThisIter <- ValueSome mv

                if score > alpha then
                    alpha <- score

            match bestMoveThisIter with
            | ValueSome bm ->
                bestMoveOverall <- ValueSome bm
                bestScoreOverall <- bestScoreThisIter
                currentRootScore <- bestScoreOverall

                // Keep the completed-iteration best move near the front for the
                // next iteration, even before fresh root ordering is applied.
                rootMoves <- putMoveFirst bm rootMoves

                // Emit one info line per completed iteration.
                let now = stopwatch.ElapsedMilliseconds
                let nps = if now > 0L then nodeCount * 1000L / now else 0L
                writeInfo depth nodeCount nps now bestScoreOverall
            | ValueNone ->
                ()

        bestMoveOverall
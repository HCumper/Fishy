module Search

open System
open BoardHelpers
open Types
open GenerateMoves
open MakeMove
open Evaluation
open BoardHelpers.Attacks
open Uci
open TranspositionTable

// Eval must be from SIDE-TO-MOVE viewpoint:
// + => good for pos.State.ToPlay
// - => bad for pos.State.ToPlay
type EvalFn = Position -> int

let mutable nodeCount = 0L
let mutable lastInfoTime = 0L
let mutable searchStartTime = 0L
let mutable currentRootScore = 0
let infoIntervalMs = 500L   // update twice per second (typical)

let inline private otherColor (c: Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

/// Mate score convention (in centipawns). Larger than any plausible eval.
[<Literal>]
let MateScore = 30000

[<Literal>]
let InCheckPenalty = 100

let negInf = -MateScore - 1

// --------------------
// TT helpers
// --------------------

let inline private keyOfPos (pos: Position) : uint64 =
    // Assuming pos.State.HashKey is int64 Zobrist (as in your tests)
    uint64 pos.State.HashKey

let inline private clamp16 (x:int) : int16 =
    if x < int Int16.MinValue then Int16.MinValue
    elif x > int Int16.MaxValue then Int16.MaxValue
    else int16 x

/// Pack Move into int32 for TT storage.
/// Layout (bits): from(0..63)=6, to(0..63)=6, promo=4  -> 16 bits used.
let inline private packMove (mv: Move) : int32 =
    let fromSq = int mv.From.File + 8 * int mv.From.Rank
    let toSq   = int mv.To.File   + 8 * int mv.To.Rank
    // PromoteTo is an sbyte piececode in your engine; keep low 4 bits
    let promo  = (int mv.PromoteTo) &&& 0xF
    int32 (fromSq ||| (toSq <<< 6) ||| (promo <<< 12))

let inline private isPackedMoveMatch (packed:int32) (mv:Move) =
    packed <> 0 && packed = packMove mv

/// Use TT entry as a cutoff if entry depth is sufficient and bound proves alpha/beta.
/// Instrumentation:
///  - markUseful() when TT actually returns a score
///  - markDepthReject() when TT hit but entry depth insufficient
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
            // TT hit but entry not deep enough
            TranspositionTable.markDepthReject()
            ValueNone
    else
        ValueNone

// =============================
// Search (Negamax + TT)
// =============================

/// Fishy's heart
/// Negamax with alpha-beta + transposition table.
/// Returns score from the SIDE-TO-MOVE viewpoint at the root `pos`.
let rec negamax
    (tt: TranspositionTable)
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    (stopwatch: Diagnostics.Stopwatch)
    : int =

    nodeCount <- nodeCount + 1L
    if (nodeCount &&& 1023L) = 0L then
        let now = stopwatch.ElapsedMilliseconds
        if now - lastInfoTime >= infoIntervalMs then
            lastInfoTime <- now
            let nps = if now > 0L then nodeCount * 1000L / now else 0L
            writeInfo depth nodeCount nps now currentRootScore

    let alphaOrig = alpha
    let key = keyOfPos pos

    // ---- TT PROBE ----
    let pr = probe tt key

    match tryUseTT pr depth alpha beta with
    | ValueSome score ->
        score

    | ValueNone ->

        // Leaf
        if depth <= 0 then
            // Store leaf as exact (cheap, helps repetition / move ordering)
            let sc = evaluate pos
            store tt key 0 (clamp16 sc) (clamp16 sc) 0 BoundExact 0uy
            sc
        else
            let side = pos.State.ToPlay
            let moves0 = generateAllLegalMoves pos inCheck

            if List.isEmpty moves0 then
                let sc =
                    if inCheck pos side then
                        -MateScore + (InCheckPenalty - depth)
                    else
                        0
                store tt key 0 (clamp16 sc) (clamp16 sc) depth BoundExact 0uy
                sc
            else
                // ---- TT MOVE ORDERING ----
                let ttMovePacked =
                    if pr.Hit then pr.Entry.Move else 0

                let moves =
                    if ttMovePacked = 0 then moves0
                    else
                        // TT move first, rest after
                        let ttFirst, rest =
                            moves0 |> List.partition (isPackedMoveMatch ttMovePacked)
                        ttFirst @ rest

                let mutable a = alpha
                let mutable best = negInf
                let mutable bestMovePacked = 0
                let mutable cutoff = false

                for mv in moves do
                    if not cutoff then
                        let mutable p = pos
                        let undo = makeMove &p mv

                        let score = - (negamax tt p (depth - 1) (-beta) (-a) stopwatch)

                        unmakeMove &p mv undo

                        if score > best then
                            best <- score
                            bestMovePacked <- packMove mv

                        if score > a then a <- score
                        if a >= beta then cutoff <- true

                // ---- TT STORE ----
                // Bound based on alphaOrig/beta and final best
                let bound =
                    if best <= alphaOrig then BoundUpper
                    elif best >= beta then BoundLower
                    else BoundExact

                store tt key bestMovePacked (clamp16 best) (clamp16 best) depth bound 0uy
                best
// set the search depth
let private depthFromRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> d
    | _ -> 6

/// Picks best move using negamax and UCI depth (default 5 if not provided).
let chooseBestMove (tt: TranspositionTable) (pos: Position) (req: SearchRequest) : Move voption =
    let depth = depthFromRequest req
    let moves = generateAllLegalMoves pos inCheck
    let stopwatch = Diagnostics.Stopwatch.StartNew()

    searchStartTime <- 0L
    lastInfoTime <- 0L
    nodeCount <- 0L
    currentRootScore <- 0

    // Advance TT generation once per root search
    newSearch tt

    match moves with
    | [] -> ValueNone
    | _ ->
        let mutable bestMove = ValueNone
        let mutable bestScore = negInf
        let mutable alpha = -MateScore - 1
        let beta = MateScore + 1

        for mv in moves do
            let mutable p = pos
            let undo = makeMove &p mv

            let score = - (negamax tt p (depth - 1) (-beta) (-alpha) stopwatch)

            unmakeMove &p mv undo

            if score > bestScore then
                bestScore <- score
                bestMove <- ValueSome mv
                currentRootScore <- bestScore

            if score > alpha then
                alpha <- score

        bestMove
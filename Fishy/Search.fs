module Search

open System
open BoardHelpers
open Types
open GenerateMoves
open MakeMove
open Evaluation
open BoardHelpers.Attacks
open Uci

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

/// Fishy's heart
/// Negamax with alpha-beta.
/// Returns score from the SIDE-TO-MOVE viewpoint at the root `pos`.
let rec negamax
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    (stopwatch: System.Diagnostics.Stopwatch)
    : int =

    nodeCount <- nodeCount + 1L
    if (nodeCount &&& 1023L) = 0L then
        let now = stopwatch.ElapsedMilliseconds
        if now - lastInfoTime >= infoIntervalMs then
            lastInfoTime <- now

            let nps =
                if now > 0L then nodeCount * 1000L / now else 0L

            writeInfo depth nodeCount nps now currentRootScore
            
    if depth <= 0 then
        evaluate pos
    else
        let side = pos.State.ToPlay
        let moves = generateAllLegalMoves pos inCheck

        if List.isEmpty moves then
            if inCheck pos side then
                -MateScore + (InCheckPenalty - depth)
            else
                0
        else
            let mutable a = alpha
            let mutable best = negInf
            let mutable cutoff = false

            for mv in moves do
                if not cutoff then
                    let mutable p = pos
                    let undo = makeMove &p mv

                    let score = - (negamax p (depth - 1) (-beta) (-a) stopwatch)

                    unmakeMove &p mv undo

                    if score > best then best <- score
                    if score > a then a <- score
                    if a >= beta then cutoff <- true

            best

let private depthFromRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> d
    | _ -> 8

/// Picks best move using negamax and UCI depth (default 4 if not provided).
let chooseBestMove (pos: Position) (req: SearchRequest) : Move voption =
    let depth = depthFromRequest req
    let moves = generateAllLegalMoves pos inCheck
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    searchStartTime <- 0L
    lastInfoTime <- 0L
    nodeCount <- 0L
    currentRootScore <- 0
    
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

            let score = - (negamax p (depth - 1) (-beta) (-alpha) stopwatch)

            unmakeMove &p mv undo

            if score > bestScore then
                bestScore <- score
                bestMove <- ValueSome mv
                currentRootScore <- bestScore

            if score > alpha then
                alpha <- score

        bestMove
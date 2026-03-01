module Search

open System
open BoardHelpers
open Types
open GenerateMoves
open MakeMove
open Evaluation
open BoardHelpers.Attacks
open UCILogger.Uci

// Eval must be from SIDE-TO-MOVE viewpoint:
// + => good for pos.State.ToPlay
// - => bad for pos.State.ToPlay
type EvalFn = Position -> int

let inline private otherColor (c: Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

/// Mate score convention (in centipawns). Larger than any plausible eval.
[<Literal>]
let MateScore = 30000

/// Negamax with alpha-beta.
/// Returns score from the SIDE-TO-MOVE viewpoint at the root `pos`.
let rec negamax
    (pos: Position)
    (depth: int)
    (alpha: int)
    (beta: int)
    : int =

    if depth <= 0 then
        evaluate pos
    else
        let side = pos.State.ToPlay
        let moves = generateAllLegalMoves pos inCheck

        if List.isEmpty moves then
            if inCheck pos side then
                -MateScore + (100 - depth)
            else
                0
        else
            let mutable a = alpha
            let mutable best = Int32.MinValue
            let mutable cutoff = false

            for mv in moves do
                if not cutoff then
                    let mutable p = pos
                    let undo = makeMove &p mv

                    let score = - (negamax p (depth - 1) (-beta) (-a))

                    unmakeMove &p mv undo

                    if score > best then best <- score
                    if score > a then a <- score
                    if a >= beta then cutoff <- true

            best

let private depthFromRequest (req: SearchRequest) =
    match req.Depth with
    | ValueSome d when d > 0 -> d
    | _ -> 5

/// Picks best move using negamax and UCI depth (default 4 if not provided).
let chooseBestMove (pos: Position) (req: SearchRequest) : Move voption =
    let depth = depthFromRequest req
    let moves = generateAllLegalMoves pos inCheck

    match moves with
    | [] -> ValueNone
    | _ ->
        let mutable bestMove = ValueNone
        let mutable bestScore = Int32.MinValue
        let mutable alpha = Int32.MinValue + 1
        let beta = Int32.MaxValue

        for mv in moves do
            let mutable p = pos
            let undo = makeMove &p mv

            let score = - (negamax p (depth - 1) (-beta) (-alpha))

            unmakeMove &p mv undo

            if score > bestScore then
                bestScore <- score
                bestMove <- ValueSome mv

            if score > alpha then
                alpha <- score

        bestMove
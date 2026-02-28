module Search

open BoardHelpers
open Types
open GenerateMoves
open MakeMove
open Evaluation
open BoardHelpers.Attacks 

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
            let mutable best = System.Int32.MinValue
            let mutable cutoff = false

            for mv in moves do
                if not cutoff then
                    let mutable p = pos
                    let undo = makeMove &p mv

                    let score = - (negamax p (depth - 1) (-beta) (-a) )

                    unmakeMove &p mv undo

                    if score > best then best <- score
                    if score > a then a <- score
                    if a >= beta then cutoff <- true

            best
module LookAhead

open Types
open System.Diagnostics
open GenerateMoves
open Evaluation
open MakeMove
open TranspositionTable
open Fishy
open System.Collections.Generic

let moveAndEvaluate board otherState move =
    let newBoard, newState = makeMove (Array2D.copy board) otherState move
    let score = evaluate newBoard newState
    Leaf(score, move)

let rec addOneLevel board otherState (value: int) (tree: Tree) : Tree =
    match tree with
    | Leaf(_, move) ->
        let evaluatedChildren = generateMoves board otherState |> List.map (moveAndEvaluate board otherState) 
        let orderedChildren = Array.sortDescending (Array.ofList evaluatedChildren)
        let Tree(topScore, _, _) = orderedChildren[0]
        let topScore =
            match tree with
            | Node (topScore, _, _) -> topScore
            | Leaf (topScore, _) -> topScore
        Node(topScore, move, orderedChildren)
    | Node(value, move, children) ->
        let newChildren = [ for child in children -> addOneLevel board otherState value child ]
        let orderedChildren = Array.sortDescending (Array.ofList newChildren)
        let Tree(topScore, bestMove, _) = orderedChildren[0]
        let topScore, bestMove =
            match tree with
            | Node (topScore, bestMove, _) -> topScore, bestMove
            | Leaf (topScore, bestMove) -> topScore, bestMove
        Node(topScore, bestMove, orderedChildren)

let buildInitialTree : Tree =
    Leaf(0, defaultMove)

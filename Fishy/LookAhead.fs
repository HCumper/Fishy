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
    Leaf (score, move)

let extractFirst2Field (tree: Tree): int * Move =
    match tree with
    | Node (firstField, secondField, _) -> firstField, secondField
    | Leaf (firstField, secondField) -> firstField, secondField

let rec makeMoveAndAddLevel board state newNode =
    let newMove =
        match newNode with
        | Node (_, secondField, _) | Leaf (_, secondField) -> secondField
    let newBoard, newState = makeMove (Array2D.copy board) state newMove
    addOneLevel newBoard newState newNode

and addOneLevel board otherState (tree: Tree) =
    match tree with
    | Leaf (_, move) ->
        let evaluatedChildren = generateMoves board otherState |> List.map (moveAndEvaluate board otherState)
        let orderedChildren =
            match otherState.ToPlay with
            | White -> Array.sortDescending (Array.ofList evaluatedChildren)
            | Black -> Array.sort (Array.ofList evaluatedChildren)
        let topScore, _ = extractFirst2Field (Array.head orderedChildren)
        Node (topScore, move, orderedChildren)

    | Node (_, move, children) ->
        let newChildren = Array.map (fun newNode -> makeMoveAndAddLevel (Array2D.copy board) otherState newNode) children
        let orderedChildren =
            match otherState.ToPlay with
            | White -> Array.sortDescending newChildren
            | Black -> Array.sort newChildren
        let topScore, _ = extractFirst2Field (Array.head orderedChildren)
        Node (topScore, move, orderedChildren)

let rec addNLevels board otherState levels tree =
    match levels with
    | 0 -> tree
    | numberLeft ->
        let newTree = addOneLevel board otherState tree
        addNLevels board otherState (numberLeft - 1) newTree

let buildInitialTree : Tree =
    Leaf (0, defaultMove)

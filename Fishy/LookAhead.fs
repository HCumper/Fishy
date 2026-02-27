module LookAhead

open Types
open GenerateMoves
open Evaluation
open MakeMove
open Fishy

let moveAndEvaluate (board: Board) (otherState: GameState) (move: Move): Tree =
    let newBoard, newState = makeMove (Array2D.copy board) otherState move
    let score = evaluate newBoard newState
    Leaf (score, move)

let extractFirst2Field (tree: Tree): int * Move =
    match tree with
    | Node (firstField, secondField, _) -> firstField, secondField
    | Leaf (firstField, secondField) -> firstField, secondField

let rec makeMoveAndAddLevel (board: Board) (state: GameState) (newNode: Tree): Tree =
    let newMove = match newNode with
                  | Node (_, move, _) | Leaf (_, move) -> move
    let newBoard, newState = makeMove (Array2D.copy board) state newMove
    addOneLevel newBoard newState newNode

and addOneLevel (board: Board) (otherState: GameState) (tree: Tree): Tree =
    let evaluatedChildren = generateMoves board otherState |> List.map (moveAndEvaluate board otherState)
    let orderedChildren = match otherState.ToPlay with
                          | White -> Array.sortDescending (Array.ofList evaluatedChildren)
                          | Black -> Array.sort (Array.ofList evaluatedChildren)
    let topScore, _ = extractFirst2Field (Array.head orderedChildren)
    match tree with
    | Leaf (_, move) -> Node (topScore, move, orderedChildren)
    | Node (_, move, children) ->
        let newChildren = Array.map (makeMoveAndAddLevel (Array2D.copy board) otherState) children
        let orderedChildren = match otherState.ToPlay with
                              | White -> Array.sortDescending newChildren
                              | Black -> Array.sort newChildren
        let topScore, _ = extractFirst2Field (Array.head orderedChildren)
        Node (topScore, move, orderedChildren)

let rec addNLevels (board: Board) (otherState: GameState) (levels: int) (tree: Tree): Tree =
    if levels = 0 then tree
    else addNLevels board otherState (levels - 1) (addOneLevel board otherState tree)

let buildInitialTree: Tree = Leaf (0, defaultMove)
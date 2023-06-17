﻿module Tests
open Fishy
open Types
open System.Collections.Generic
open Xunit
open LookAhead
open FENParser
open Evaluation
open LookAhead

let rec printTree (tree: Tree) =
    match tree with
    | Leaf(value, _) ->
        printfn "Leaf: %d" value
    | Node(value, _, children) ->
        printfn "Node: %d" value
        for child in children do
            printTree child

let rec countNodes (tree: Tree) : int =
    match tree with
    | Leaf(_, _) -> 1
    | Node(_, _, children) -> (children |> (Seq.sumBy countNodes)) + 1

[<Fact>]
let ``Add Three Levels to Leaf Tree``() =
    let sessionBoard, sessionState = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
 //   myColor <- White
    initializePlacementValues () |> ignore
    let tree = Leaf(0, defaultMove)
    let finalTree = addNLevels sessionBoard sessionState 4 tree
//    let nodeCount = countNodes newTree
//    printTree newTree
    Assert.Equal(3, 3)

// [<Fact>]
// let ``Add One Level to Node Tree``() =
//     let tree =
//         Node(0, defaultMove,
//             SortedSet([
//                 Leaf(1, defaultMove)
//                 Leaf(2, defaultMove)
//             ]))
//     let newTree = addOneLevel tree 3
//     let nodeCount = countNodes newTree
//     Assert.Equal(7, nodeCount)
//
// [<Fact>]
// let ``Add One Level to Empty Tree``() =
//     let tree = Leaf(0, defaultMove)
//     let newTree = addOneLevel tree 0
//     let nodeCount = countNodes newTree
//     Assert.Equal(1, nodeCount)
//
// [<Fact>]
// let ``Build N-Tree Iterative``() =
//     let tree = buildNTreeIterative
//     let nodeCount = countNodes tree
//     Assert.Equal(1, nodeCount)
//
// [<Fact>]
// let ``Add One Level to N-Tree Iterative``() =
//     let tree = buildNTreeIterative
//     let newTree = addOneLevel tree 4
//     let nodeCount = countNodes newTree
//     Assert.Equal(5, nodeCount)

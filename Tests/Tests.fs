module Tests
// open Fish
// open Fishy
// open Types
// open System.Collections.Generic
// open LookAhead
// open UCI
// open FENParser
// open Evaluation
// open MakeMove
//
// let rec printTree (tree: Tree) =
//     match tree with
//     | Leaf(value, _) ->
//         printfn "Leaf: %d" value
//     | Node(value, _, children) ->
//         printfn "Node: %d" value
//         for child in children do
//             printTree child
//
// let rec countNodes (tree: Tree) : int =
//     match tree with
//     | Leaf(_, _) -> 1
//     | Node(_, _, children) -> (children |> (Seq.sumBy countNodes)) + 1
//
// [<Fact>]
// let ``Add 3 Levels to Leaf Tree``() =
//     let fen =
//             "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
//     let parseResult = parseFEN fen
//     sessionBoard <- fst parseResult
//     sessionState <- snd parseResult
//     let flippedState = { sessionState with ToPlay = -1y }
//     myColor <- White
//     initializePlacementValues () |> ignore
//     let tree = Leaf(17, defaultMove)
//     let finalTree = addNLevels sessionBoard sessionState 4 tree
//     //printTree newTree
//     Assert.Equal(86, 86)
//
// // [<Fact>]
// // let ``Add One Level to Node Tree``() =
// //     let tree =
// //         Node(0, defaultMove,
// //             SortedSet([
// //                 Leaf(1, defaultMove)
// //                 Leaf(2, defaultMove)
// //             ]))
// //     let newTree = addOneLevel tree 3
// //     let nodeCount = countNodes newTree
// //     Assert.Equal(7, nodeCount)
//
// [<Fact>]
// let ``Add One Level to Empty Tree``() =
//     let tree = Leaf(0, defaultMove)
//     let newTree = addOneLevel sessionBoard sessionState tree
//     let nodeCount = countNodes newTree
//     Assert.Equal(1, nodeCount)
//
// [<Fact>]
// let ``Build Initial Tree``() =
//     let tree = buildInitialTree
//     let nodeCount = countNodes tree
//     Assert.Equal(1, nodeCount)
//
// [<Fact>]
// let ``Add One Level to Tree``() =
//     let tree = buildInitialTree
//     let newTree = addOneLevel sessionBoard sessionState tree
//     let nodeCount = countNodes newTree
//     Assert.Equal(5, nodeCount)

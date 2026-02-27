module TranspositionTableTests

open NUnit.Framework
open TranspositionTable
open Types
open Fishy
    
let createMove () =
    { FromFile = 4
      FromRank = 1
      ToFile = 4
      ToRank = 3
      CapturedPiece = 0y
      PromoteTo = 0y }
    
[<Test>]
let ``Insert into transposition table with higher confidence updates entry`` () =
    resetTranspositionTable()
    let board = createInitialBoard()
    let state = createInitialState()
    let move = createMove()
    insertIntoTranspositionTable board state 10 5 move
    insertIntoTranspositionTable board state 20 10 move
    let result = transpositionTableLookupByBoard board state 0
    Assert.That(Some (20, 10, move), Is.EqualTo(result))
    ()

[<Test>]
let ``Insert into transposition table with lower confidence does not update entry`` () =
    resetTranspositionTable()
    let board = createInitialBoard()
    let state = createInitialState()
    let move = createMove()
    insertIntoTranspositionTable board state 20 10 move
    insertIntoTranspositionTable board state 10 5 move
    let result = transpositionTableLookupByBoard board state 0
    Assert.That(Some (20, 10, move), Is.EqualTo(result))

[<Test>]
let ``Does not retrieve if state is different`` () =
    resetTranspositionTable()
    let board = createInitialBoard()
    let state = createInitialState()
    let move = createMove()
    insertIntoTranspositionTable board state 20 10 move
    let differentState = { state with ToPlay = Black }
    let result = transpositionTableLookupByBoard board differentState 0
    Assert.That(None, Is.EqualTo(result))

[<Test>]
let ``Lookup by board and state returns None if not present`` () =
    resetTranspositionTable()
    let board = createInitialBoard()
    let state = createInitialState()
    let result = transpositionTableLookupByBoard board state 0
    Assert.That(None, Is.EqualTo(result))

[<Test>]
let ``Reset transposition table clears all entries`` () =
    resetTranspositionTable()
    let board = createInitialBoard()
    let state = createInitialState()
    let move = createMove()
    insertIntoTranspositionTable board state 20 10 move
    resetTranspositionTable()
    let result = transpositionTableLookupByBoard board state 0
    Assert.That(None, Is.EqualTo(result))

// [<Test>]
// let ``Lookup by hash returns correct entry`` () =
//     resetTranspositionTable()
//     let board = createInitialBoard()
//     let state = createInitialState()
//     let move = createMove()
//     let hashKey = hashAPosition board state
//     insertIntoTranspositionTable board state 20 10 move
//     let result = transpositionTableLookupByHash hashKey
//     Assert.That(Some (20, 10, move), Is.EqualTo(result))


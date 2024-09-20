module MoveGenerationTests

open NUnit.Framework
open Fishy
open Types
open GenerateMoves
open MakeMove

let gameState = { ToPlay = White; WhiteCanCastleKingside = false; WhiteCanCastleQueenside = false; BlackCanCastleKingside = false; BlackCanCastleQueenside = false; EPSquare = None; HalfMoveClock = 26; FullMoveNumber = 7; HashKey = 0 }

[<Test>]
let ``generateMoves returns empty list for empty board`` () =
    let board = Array2D.createBased 1 1 8 8 0y
    let moves = generateMoves board gameState
    Assert.That(moves.Length, Is.EqualTo(0))
    ()

[<Test>]
let ``generateMoves returns correct moves for initial position`` () =
    let board = createInitialBoard()
    let state = createInitialState()
    let moves = generateMoves board state
    Assert.That(20, Is.EqualTo(List.length moves))
    let newBoard, newState = makeMove board state moves.Head
    Assert.That(20, Is.EqualTo(List.length (generateMoves newBoard newState)))
    ()
    
//
// [<Test>]
// let ``generateMoves returns correct moves for black pawn initial position`` () =
//     let board = Array2D.create 9 9 Empty
//     board.[2, 7] <- BlackPawn
//     let gameState = { ToPlay = Black; WhiteCanCastleKingside = false; WhiteCanCastleQueenside = false; BlackCanCastleKingside = false; BlackCanCastleQueenside = false; EPSquare = None }
//     let moves = generateMoves board gameState
//     Assert.AreEqual(2, List.length moves)
//
// [<Test>]
// let ``generateMoves returns correct moves for white king castling`` () =
//     let board = Array2D.create 9 9 Empty
//     board.[5, 1] <- WhiteKing
//     board.[8, 1] <- WhiteRook
//     let gameState = { ToPlay = White; WhiteCanCastleKingside = true; WhiteCanCastleQueenside = false; BlackCanCastleKingside = false; BlackCanCastleQueenside = false; EPSquare = None }
//     let moves = generateMoves board gameState
//     Assert.IsTrue(moves |> List.exists (fun move -> move.fromFile = 5 && move.fromRank = 1 && move.toFile = 7 && move.toRank = 1))
//
// [<Test>]
// let ``generateMoves returns correct moves for black king castling`` () =
//     let board = Array2D.create 9 9 Empty
//     board.[5, 8] <- BlackKing
//     board.[8, 8] <- BlackRook
//     let gameState = { ToPlay = Black; WhiteCanCastleKingside = false; WhiteCanCastleQueenside = false; BlackCanCastleKingside = true; BlackCanCastleQueenside = false; EPSquare = None }
//     let moves = generateMoves board gameState
//     Assert.IsTrue(moves |> List.exists (fun move -> move.fromFile = 5 && move.fromRank = 8 && move.toFile = 7 && move.toRank = 8))
//
// [<Test>]
// let ``generateMoves returns correct moves for knight`` () =
//     let board = Array2D.create 9 9 Empty
//     board.[2, 2] <- WhiteKnight
//     let gameState = { ToPlay = White; WhiteCanCastleKingside = false; WhiteCanCastleQueenside = false; BlackCanCastleKingside = false; BlackCanCastleQueenside = false; EPSquare = None }
//     let moves = generateMoves board gameState
//     Assert.AreEqual(8, List.length moves)
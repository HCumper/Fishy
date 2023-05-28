module Fishy

open System.Collections.Generic
open Types

[<Literal>]
let White = 1y

[<Literal>]
let Black = -1y

[<Literal>]
let WhitePawn = 1y

[<Literal>]
let WhiteRook = 2y

[<Literal>]
let WhiteKnight = 3y

[<Literal>]
let WhiteBishop = 4y

[<Literal>]
let WhiteQueen = 5y

[<Literal>]
let WhiteKing = 6y

[<Literal>]
let BlackPawn = -1y

[<Literal>]
let BlackRook = -2y

[<Literal>]
let BlackKnight = -3y

[<Literal>]
let BlackBishop = -4y

[<Literal>]
let BlackQueen = -5y

[<Literal>]
let BlackKing = -6y

[<Literal>]
let Empty = 0y

[<Literal>]
let offBoard = 100y

// // This is a global variable on which actual game moves are made and unmade
// // board and state have to be global mutables as communication via UCI ia stateful
// let mutable (globalBoard: sbyte[,]) = Array2D.createBased -1 -1 12 12 offBoard
//
// // match from * to with
// //    | x when x = 0 move to empty Square
// //    | x when x = 100 move off Board
// //    | x when x*y < 0 Capture
// //    | x when x*y > 0 attempt to capture own piece
//
// // current state is passed around and coninually copied and destroyed
// let mutable globalState: GameState =
//     { WhiteCanCastleKingside = true
//       WhiteCanCastleQueenside = true
//       BlackCanCastleKingside = true
//       BlackCanCastleQueenside = true
//       ToPlay = White
//       EPSquare = None
//       HalfMoveClock = 0
//       FullMoveNumber = 0
//     }

let defaultMove =
    { fromFile = 0
      fromRank = 0
      toFile = 0
      toRank = 0
      capturedPiece = 0y
      promoteTo = 0y }

let mutable (transpositionTable: HashTable) = Dictionary<int64, int>()

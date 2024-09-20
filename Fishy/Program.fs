module Fishy

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

let createInitialBoard () =
    let board = Array2D.createBased 1 1 8 8 0y
    for i in 1..8 do
        board.[i, 2] <- 1y  // White pawns
        board.[i, 7] <- -1y // Black pawns
    // Place rooks
    board.[1, 1] <- 2y; board.[8, 1] <- 2y  // White rooks
    board.[1, 8] <- -2y; board.[8, 8] <- -2y // Black rooks
    // Place knights
    board.[2, 1] <- 3y; board.[7, 1] <- 3y  // White knights
    board.[2, 8] <- -3y; board.[7, 8] <- -3y // Black knights
    // Place bishops
    board.[3, 1] <- 4y; board.[6, 1] <- 4y  // White bishops
    board.[3, 8] <- -4y; board.[6, 8] <- -4y // Black bishops
    // Place queens
    board.[4, 1] <- 5y  // White queen
    board.[4, 8] <- -5y // Black queen
    // Place kings
    board.[5, 1] <- 6y  // White king
    board.[5, 8] <- -6y // Black king
    board

let createInitialState () =
    { WhiteCanCastleKingside = true
      WhiteCanCastleQueenside = true
      BlackCanCastleKingside = true
      BlackCanCastleQueenside = true
      ToPlay = White
      EPSquare = None
      HalfMoveClock = 0
      FullMoveNumber = 1
      HashKey = 0 }

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
    { fromFile = -1
      fromRank = -1
      toFile = -1
      toRank = -1
      capturedPiece = 0y
      promoteTo = 0y }

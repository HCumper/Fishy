module Types

type Player =
    | Human
    | Engine

type Piece =
    | King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn
//
// type Color =
//     | White
//     | Black

//type Square = (Piece * Color) option

type Board = sbyte[,]

// board is laid out column, row i.e. file, rank matching chess notation e4
type Coordinates = int * int

type Move =
    { fromFile : int
      fromRank : int
      toFile : int
      toRank : int
      capturedPiece : sbyte
      promoteTo : sbyte
    }

type OtherState =
    { WhiteKingMoved: bool
      WhiteQRMoved: bool
      WhiteKRMoved: bool
      BlackKingMoved: bool
      BlackQRMoved: bool
      BlackKRMoved: bool
      EPSquare: Coordinates option
      ToPlay: sbyte
    }

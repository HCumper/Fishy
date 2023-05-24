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

type Color =
    | White
    | Black

type Square = (Piece * Color) option

type Board = Square[,]

// board is laid out column, row i.e. file, rank matching chess notation e4
type Coordinates = int * int

type Move = Coordinates * Coordinates * Piece option

type OtherState =
    { WhiteKingMoved: bool
      WhiteQRMoved: bool
      WhiteKRMoved: bool
      BlackKingMoved: bool
      BlackQRMoved: bool
      BlackKRMoved: bool
      EPSquare: Coordinates option
      JustCaptured: bool
      PromoteTo: Piece option
      ToPlay: Color }

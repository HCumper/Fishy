﻿module Types

open System
open System.Collections.Generic

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

type HashTable = Dictionary<int64, (int * int)>

type GameState =
    { WhiteCanCastleKingside: bool
      WhiteCanCastleQueenside: bool
      BlackCanCastleKingside: bool
      BlackCanCastleQueenside: bool
      ToPlay: sbyte
      EPSquare: Coordinates option
      HalfMoveClock: int
      FullMoveNumber: int
      HashKey: Int64
    }

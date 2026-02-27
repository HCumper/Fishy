module Types

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

type Player = Human | Engine  // UI only

type Color = White = 0uy | Black = 1uy

type Piece = King | Queen | Rook | Bishop | Knight | Pawn

type Board = sbyte[,]

[<Literal>]
let BoardSize = 8

[<Literal>]
let MinFileRank = 1

[<Literal>]
let MaxFileRank = 8

// board is laid out column, row i.e. file, rank matching chess notation e4
[<Struct; IsReadOnly>]
type Coordinates = { File: byte; Rank: byte }

// Everything needed to evaluate a position including the board disregarding repetitions
[<Struct>]
type GameState =
    {
      HashKey: Int64
      EPSquare: ValueOption<Coordinates>
      FullMoveNumber: uint16
      CastlingRights : byte
      HalfMoveClock: byte
      ToPlay: Color
    }

/// Keep king squares outside the array for speed
[<Struct>]
type KingSquares =
    { WhiteKingSq : Coordinates
      BlackKingSq : Coordinates }
    
// Lightweight representation of a move used for move generation    
[<Struct>]
type Move =
    { From: Coordinates
      To: Coordinates
      Piece: sbyte
      PromoteTo: sbyte
    }
    
[<Struct>]
type MoveUndo =
      {
          CapturedPiece: sbyte
          PreviousCastlingRights: byte
          PreviousEPSquare: ValueOption<Coordinates> 
          PreviousHalfMoveClock: byte
          PreviousFullMoveNumber: uint16
          PreviousKingSquares: KingSquares
          PreviousHashKey: Int64
       }
      
[<Struct>]
type Position =
    {
        Board: Board
        State: GameState
        Kings: KingSquares
    }

type ScoreType =
    | Exact      // PV node - exact score
    | LowerBound // Beta cutoff - score >= this
    | UpperBound // All nodes searched - score <= this
    
[<Struct>]
type TTEntry =
    { Depth: int
      Score: int
      ScoreType: ScoreType
      BestMove: ValueOption<Move>
      Age: byte }
    
type HashTable = Dictionary<int64, TTEntry>
    
type SearchTree =
    { State: GameState
      Move: ValueOption<Move>      // Move that led to this position
      Eval: int
      Depth: int
      Alpha: int
      Beta: int
      BestMove: ValueOption<Move>
      Children: SearchTree list }

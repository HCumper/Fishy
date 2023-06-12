module Types

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
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

type HashTable = Dictionary<int64, int>

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
type Tree =
    | Node of int * Move * Tree[]
    | Leaf of int * Move
    // interface IComparable<Tree> with
    //     member this.CompareTo(other : Tree) =
    //         match this, other with
    //         | Leaf(v1, _), Leaf(v2, _) -> compare v1 v2
    //         | Node(v1, _, _), Node(v2, _, _) -> compare v1 v2
    //         | Leaf _, Node _ -> -1
    //         | Node _, Leaf _ -> 1
    //         | _ -> 0

// let rec compareTrees (tree1: Tree) (tree2: Tree) : int =
//     match tree1, tree2 with
//     | Leaf(v1, _), Leaf(v2, _) -> compare v1 v2
//     | Node(v1, _, _), Node(v2, _, _) -> compare v1 v2
//     | Leaf _, Node _ -> -1
//     | Node _, Leaf _ -> 1
//     | _ -> 0
//
// let comparer : Comparer<Tree> = Comparer<Tree>.Create(compareTrees)

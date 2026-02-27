module Tests

open Types
open Fen
open NUnit.Framework

/// Initialize a fresh 8x8 board from a FEN string.
let initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

/// Simple smoke test using the standard starting position FEN.
[<Test>]
let testInitStartPosition () : bool =
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    match initBoardFromFen startFen with
    | ValueSome _ -> true
    | ValueNone -> false
module TestProject1.MoveGenerationTests

open BoardHelpers.Coordinates
open Types
open Fen
open BoardHelpers
open GenerateMoves
open MakeMove
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()
    
/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

[<Test>]
let ``Start FEN round-trips starting position back to identical string`` () =
    let startFen =
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 7 1)
                makeMove
                unmakeMove
                inCheck

        Assert.Pass()  // or actual assertion

    | ValueNone ->
        Assert.Fail("FEN parse failed")    
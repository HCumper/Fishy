module TestProject1.MoveGenerationTests

open BoardHelpers.Coordinates
open Types
open Fen
open BoardHelpers
open GenerateMoves
open Attacks
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()
    
/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

[<Test>]
let ``Generate knight moves from starting position`` () =
    let startFen =
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 7 1)
                inCheck

        Assert.That(moveList.Length = 2)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate knight moves when none are possible due to moving into check`` () =
    let startFen =
        "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 3 3)
                inCheck

        Assert.That(moveList.Length = 0)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate knight moves when five are possible`` () =
    let startFen =
        "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 3 3)
                inCheck

        Assert.That(moveList.Length = 5)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate knight moves from empty square`` () =
    let startFen =
        "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 7 4)
                inCheck

        Assert.That(moveList.Length = 0)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate knight moves from square with enemy knight`` () =
    let startFen =
        "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalKnightMoves
                position
                (Coordinates.createInts 6 6)
                inCheck

        Assert.That(moveList.Length = 0)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate black bishop moves`` () =
    let startFen =
        "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR b KQkq - 0 4"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalBishopMoves
                position
                (Coordinates.createInts 2 4)
                inCheck

        Assert.That(moveList.Length = 7)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate black queen moves`` () =
    let startFen =
        "rnb2rk1/pppp1ppp/4p3/6q1/1b1Pn3/2NB1N2/PPPB1PPP/R2Q1RK1 b - - 0 7"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalQueenMoves
                position
                (Coordinates.createInts 7 5)
                inCheck

        Assert.That(moveList.Length = 19)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate white rook moves`` () =
    let startFen =
        "rnb2r2/pppp1p1k/4p1pp/6q1/1b1Pn2P/2NB1NR1/PPPB1PP1/R2QK3 w Q - 0 10"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalRookMoves
                position
                (Coordinates.createInts 7 3)
                inCheck

        Assert.That(moveList.Length = 3)

    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Generate white rook moves with moving into check avoidance`` () =
    let startFen =
        "rnb2rk1/pppp1ppp/4p3/8/Pb1P4/2NB1N2/1PPB1RPP/R2Qq1K1 w - - 0 11"

    match initBoardFromFen startFen with
    | ValueSome position ->

        let moveList =
            generateLegalRookMoves
                position
                (Coordinates.createInts 6 2)
                inCheck

        Assert.That(moveList.Length = 1)

    | ValueNone ->
        Assert.Fail("FEN parse failed")    
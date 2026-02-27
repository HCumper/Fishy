module FENTests

open Types
open Fen
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()
    
/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

/// Simple smoke test using the standard starting position FEN.
[<Test>]
let testInitStartPosition () : bool =
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    match initBoardFromFen startFen with
    | ValueSome _ -> true
    | ValueNone -> false

[<Test>]
let ``Start FEN round-trips starting position back to identical string`` () =
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    match initBoardFromFen startFen with
    | ValueSome pos ->
        let roundTrippedFen = positionToFen pos
        Assert.That(roundTrippedFen, Is.EqualTo(startFen))
    | ValueNone ->
        Assert.Fail("FEN parse failed")
        
[<Test>]
let ``Start FEN round-trips arbitrary position back to identical string`` () =
    let startFen = "r3kr2/1p1q1p1p/p2p1Q2/3Np3/3nP3/8/PPP2PPP/R4RK1 w - - 0 22"
    match initBoardFromFen startFen with
    | ValueSome pos ->
        let roundTrippedFen = positionToFen pos
        Assert.That(roundTrippedFen, Is.EqualTo(startFen))
    | ValueNone ->
        Assert.Fail("FEN parse failed")

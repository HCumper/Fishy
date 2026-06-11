module FENTests

open Types
open Fen
open Zobrist
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

/// Initialize a fresh 1D 64-square board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array.zeroCreate 64
    tryLoadPositionFromFen board fen

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

[<Test>]
let ``Loaded FEN initializes hash key`` () =
    let fenAfterE4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

    match initBoardFromFen fenAfterE4 with
    | ValueSome pos ->
        let recomputed = hashPosition pos.Board pos.State
        Assert.That(pos.State.HashKey, Is.EqualTo(recomputed))
        Assert.That(pos.State.HashKey, Is.Not.EqualTo(0L))
    | ValueNone ->
        Assert.Fail("FEN parse failed")

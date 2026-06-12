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

let private assertFenRejected (fen: string) =
    match initBoardFromFen fen with
    | ValueNone -> ()
    | ValueSome _ -> Assert.Fail($"Expected FEN to be rejected: {fen}")

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

[<Test>]
let ``FEN preserves castling rights en passant clocks and hash`` () =
    let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq e3 12 34"

    match initBoardFromFen fen with
    | ValueSome pos ->
        Assert.That(positionToFen pos, Is.EqualTo(fen))
        Assert.That(pos.State.CastlingRights, Is.EqualTo(15uy))
        Assert.That(pos.State.HalfMoveClock, Is.EqualTo(12uy))
        Assert.That(pos.State.FullMoveNumber, Is.EqualTo(34us))

        match pos.State.EPSquare with
        | ValueSome ep ->
            Assert.That(ep.File, Is.EqualTo(4uy))
            Assert.That(ep.Rank, Is.EqualTo(2uy))
        | ValueNone ->
            Assert.Fail("Expected EP square")

        Assert.That(pos.State.HashKey, Is.EqualTo(hashPosition pos.Board pos.State))
    | ValueNone ->
        Assert.Fail("FEN parse failed")

[<Test>]
let ``FEN rejects invalid en passant target ranks`` () =
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 w - e4 0 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 b - a1 0 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 w - z9 0 1"

[<Test>]
let ``FEN rejects missing or duplicate kings`` () =
    assertFenRejected "8/8/8/8/8/8/8/4K3 w - - 0 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K2K w - - 0 1"
    assertFenRejected "4k2k/8/8/8/8/8/8/4K3 b - - 0 1"

[<Test>]
let ``FEN rejects malformed side castling and move counters`` () =
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 x - - 0 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 w KX - 0 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 w - - -1 1"
    assertFenRejected "4k3/8/8/8/8/8/8/4K3 w - - 0 -1"

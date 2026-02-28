module EvaluationTests

open NUnit.Framework
open Types
open Fen
open Evaluation

/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

let private mustLoad (fen: string) : Position =
    match initBoardFromFen fen with
    | ValueSome p -> p
    | ValueNone -> Assert.Fail($"FEN parse failed: {fen}"); Unchecked.defaultof<Position>

[<Test>]
let ``Evaluate start position is zero`` () =
    let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let pos = mustLoad fen
    Assert.That(evaluate pos, Is.EqualTo(0))

[<Test>]
let ``Evaluate flips sign when ToPlay flips`` () =
    // Same board, only side to move differs.
    let fenW = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let fenB = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"
    let posW = mustLoad fenW
    let posB = mustLoad fenB
    Assert.That(evaluate posW, Is.EqualTo(-(evaluate posB)))

[<Test>]
let ``Material advantage is positive for advantaged side to move`` () =
    // White has an extra queen. With white to move, score must be > 0.
    let fen = "k7/8/8/8/8/8/8/KQ6 w - - 0 1"
    let pos = mustLoad fen
    Assert.That(evaluate pos, Is.GreaterThan(0))

[<Test>]
let ``Material advantage is negative if disadvantaged side to move`` () =
    // Same position, but black to move: evaluation returned is side-to-move (black),
    // so it must be < 0 because black is losing material.
    let fen = "k7/8/8/8/8/8/8/KQ6 b - - 0 1"
    let pos = mustLoad fen
    Assert.That(evaluate pos, Is.LessThan(0))

[<Test>]
let ``PST orientation sanity: advancing a white pawn increases score`` () =
    // Compare two positions identical except pawn on e2 vs e4.
    // White to move in both; e4 should be at least as good as e2 by PST.
    let fenE2 = "k7/8/8/8/8/8/4P3/4K3 w - - 0 1"
    let fenE4 = "k7/8/8/8/4P3/8/8/4K3 w - - 0 1"
    let p2 = mustLoad fenE2
    let p4 = mustLoad fenE4
    Assert.That(evaluate p4, Is.GreaterThanOrEqualTo(evaluate p2))


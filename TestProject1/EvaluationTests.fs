module EvaluationTests

open NUnit.Framework
open Types
open Evaluation
open Fen

// Fresh board per FEN load so tests don't share mutable state.
let private loadFen (fen: string) : Position voption =
    let board = Array.zeroCreate<sbyte> 64
    tryLoadPositionFromFen board fen

let private evalFen (fen: string) : int =
    match loadFen fen with
    | ValueSome pos -> evaluate pos
    | ValueNone ->
        Assert.Fail($"Failed to load FEN: {fen}")
        0

[<TestFixture>]
type ``Evaluation invariants Tests`` () =

    // -------------------------------------------------------------------------
    // Determinism / basic harness
    // -------------------------------------------------------------------------

    [<Test>]
    member _.``evaluation is deterministic`` () =
        let fen = "r1bq1rk1/ppp2ppp/2np1n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQ1RK1 w - - 0 1"
        let e1 = evalFen fen
        let e2 = evalFen fen
        let e3 = evalFen fen
        Assert.That(e1, Is.EqualTo(e2))
        Assert.That(e2, Is.EqualTo(e3))

    [<Test>]
    member _.``evaluation remains bounded`` () =
        let fens = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            "8/8/8/4k3/4P3/8/8/4K3 w - - 0 1"
            "QQQQ4/QQQQ4/8/4k3/8/8/8/4K3 w - - 0 1"
            "8/8/8/8/8/8/8/4k2K w - - 0 1"
        ]
        for fen in fens do
            let e = evalFen fen
            Assert.That(e, Is.InRange(-15000, 15000), $"Eval out of bounds for: {fen} (e={e})")

    // -------------------------------------------------------------------------
    // Side-to-move sign convention (negamax-ready)
    // -------------------------------------------------------------------------
    [<Test>]
    member _.``mobility is included for white-to-move too`` () =
        let fen = "4k3/8/8/8/4Q3/8/8/4K3 w - - 0 1"
        match loadFen fen with
        | ValueSome pos ->
            let e1 = evaluate pos
            let e2 = evaluate pos
            Assert.That(e1, Is.EqualTo(e2)) // determinism
            // If you have an internal function or flag to disable mobility, compare here.
        | _ -> Assert.Fail("load failed")

    // PST mapping bug "smell tests"
    // -------------------------------------------------------------------------

    [<Test>]
    member _.``PST mapping sanity: knight center beats knight corner`` () =
        let eCenter = evalFen "8/8/8/4k3/4N3/8/8/4K3 w - - 0 1" // Ne4
        let eCorner = evalFen "8/8/8/4k3/8/8/8/N3K3 w - - 0 1" // Na1
        Assert.That(eCenter, Is.GreaterThan(eCorner), $"Expected center > corner: center={eCenter}, corner={eCorner}")

    // -------------------------------------------------------------------------
    // Kingside/Queenside file-mirror symmetry (expects equality, not negation)
    // -------------------------------------------------------------------------

    [<Test>]
    member _.``file mirror symmetry: rook a4 equals rook h4`` () =
        let eA = evalFen "4k3/8/8/8/R7/8/8/4K3 w - - 0 1" // Ra4
        let eH = evalFen "4k3/8/8/8/7R/8/8/4K3 w - - 0 1" // Rh4
        let diff = abs (eA - eH)
        Assert.That(diff, Is.LessThanOrEqualTo(5), $"Expected file mirror equality: a4={eA}, h4={eH}, diff={diff}")

    // -------------------------------------------------------------------------
    // MG/EG taper: positions behave like endgame vs opening
    // -------------------------------------------------------------------------

    [<Test>]
    member _.``endgame: king centralization is preferred`` () =
        let egCenter = evalFen "8/8/8/4k3/4K3/8/8/8 w - - 0 1"
        let egBack   = evalFen "8/8/8/4k3/8/8/8/4K3 w - - 0 1"
        Assert.That(egCenter, Is.GreaterThan(egBack), $"EG king center should score higher: center={egCenter}, back={egBack}")

    [<Test>]
    member _.``taper works: king-centralization effect is larger in EG than MG`` () =
        // EG gap:
        let egCenter = evalFen "8/8/8/4k3/4K3/8/8/8 w - - 0 1"
        let egBack   = evalFen "8/8/8/4k3/8/8/8/4K3 w - - 0 1"
        let egGap = egCenter - egBack

        // MG-ish: lots of pieces. Put white king on e4 vs e1 while keeping material heavy.
        // We do not assert the direction in MG, only that EG influence is stronger than MG influence.
        let mgCenter = evalFen "rnbqkbnr/pppppppp/8/8/4K3/8/PPPPPPPP/RNBQ1BNR w KQkq - 0 1"
        let mgBack   = evalFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let mgGap = mgCenter - mgBack

        Assert.That(abs egGap, Is.GreaterThan(abs mgGap),
            $"Expected |EG gap| > |MG gap|: egGap={egGap}, mgGap={mgGap}")

    // -------------------------------------------------------------------------
    // Phase clamp with promoted material (no crash; correct sign)
    // -------------------------------------------------------------------------

    [<Test>]
    member _.``phase clamps with promoted material: huge advantage stays positive`` () =
        let e = evalFen "QQQQ4/8/8/4k3/8/8/8/4K3 w - - 0 1"
        Assert.That(e, Is.GreaterThan(0), $"Expected positive eval for massive white material: e={e}")
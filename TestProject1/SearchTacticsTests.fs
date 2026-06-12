module SearchTacticsTests

open NUnit.Framework

open Types
open Fen
open Search
open TranspositionTable
open Uci

let private mkReq depth : SearchRequest =
    { Depth = ValueSome depth
      MoveTime = ValueNone
      WTime = ValueNone
      BTime = ValueNone
      WInc = ValueNone
      BInc = ValueNone
      Nodes = ValueNone
      Mate = ValueNone
      MovesToGo = ValueNone
      Infinite = false }

let private loadPos fen : Position =
    let board : Board = Array.zeroCreate 64

    match tryLoadPositionFromFen board fen with
    | ValueSome pos -> pos
    | ValueNone ->
        Assert.Fail($"FEN failed to load: {fen}")
        Unchecked.defaultof<_>

let private mkTT () =
    create { Mb = 16; BucketSize = 4; MaxAge = 8 }

let private bestMoveAt depth fen =
    let tt = mkTT()
    currentRootScore <- 0
    nodeCount <- 0L

    match chooseBestMove tt (loadPos fen) (mkReq depth) with
    | ValueSome mv -> moveToUci mv, currentRootScore
    | ValueNone ->
        Assert.Fail("Expected a best move")
        Unchecked.defaultof<_>

[<Test>]
let ``search finds mate in one`` () =
    let best, score =
        bestMoveAt 1 "6k1/8/6K1/8/8/8/8/R7 w - - 0 1"

    Assert.That(best, Is.EqualTo("a1a8"))
    Assert.That(score, Is.GreaterThanOrEqualTo(MateScore - 100))

[<Test>]
let ``search promotes a passed pawn to queen`` () =
    let best, score =
        bestMoveAt 2 "4k3/P7/8/8/8/8/8/4K3 w - - 0 1"

    Assert.That(best, Is.EqualTo("a7a8q"))
    Assert.That(score, Is.GreaterThan(800))

[<Test>]
let ``search captures a loose queen`` () =
    let best, score =
        bestMoveAt 2 "4k3/8/8/8/4q3/8/4R3/4K3 w - - 0 1"

    Assert.That(best, Is.EqualTo("e2e4"))
    Assert.That(score, Is.GreaterThan(400))

[<Test>]
let ``search returns no move when side to move is checkmated`` () =
    let tt = mkTT()
    let pos = loadPos "R5k1/8/6K1/8/8/8/8/8 b - - 0 1"

    let best = chooseBestMove tt pos (mkReq 1)

    match best with
    | ValueNone -> ()
    | ValueSome mv -> Assert.Fail($"Expected no legal move, got {moveToUci mv}")

module SearchQuiescenceTests

open System.Diagnostics
open NUnit.Framework

open Types
open Fen
open Evaluation
open Search
open TranspositionTable

let private loadPos fen : Position =
    let board : Board = Array.zeroCreate 64

    match tryLoadPositionFromFen board fen with
    | ValueSome pos -> pos
    | ValueNone ->
        Assert.Fail($"FEN failed to load: {fen}")
        Unchecked.defaultof<_>

let private mkTT () =
    create { Mb = 16; BucketSize = 4; MaxAge = 8 }

let private budget =
    { SoftMs = 1000L
      HardMs = 1000L }

[<SetUp>]
let setup () =
    abortSearch <- false
    softTimeUp <- false
    nodeCount <- 0L

[<Test>]
let ``depth zero negamax uses quiescence to resolve obvious capture`` () =
    let pos = loadPos "4k3/8/8/8/4q3/8/4R3/4K3 w - - 0 1"
    let tt = mkTT()
    let sw = Stopwatch.StartNew()

    let staticEval = evaluate pos
    let searched = negamax tt pos 0 (-MateScore) MateScore sw budget

    Assert.That(staticEval, Is.LessThan(0))
    Assert.That(searched, Is.GreaterThan(400))

[<Test>]
let ``quiescence matches static evaluation in quiet position`` () =
    let pos = loadPos "4k3/8/8/8/8/8/4R3/4K3 w - - 0 1"
    let tt = mkTT()
    let sw = Stopwatch.StartNew()

    let staticEval = evaluate pos
    let searched = quiescence tt pos (-MateScore) MateScore sw budget

    Assert.That(searched, Is.EqualTo(staticEval))

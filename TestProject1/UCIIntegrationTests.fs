module UCIIntegrationTests

open System
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework

open Types
open Uci
open UCIIntegration
open Zobrist
open Evaluation
open TranspositionTable

let private depthOneRequest : SearchRequest =
    { Depth = ValueSome 1
      MoveTime = ValueNone
      WTime = ValueNone
      BTime = ValueNone
      WInc = ValueNone
      BInc = ValueNone
      Nodes = ValueNone
      Mate = ValueNone
      MovesToGo = ValueNone
      Infinite = false }

let private searchRequest depth : SearchRequest =
    { depthOneRequest with Depth = ValueSome depth }

let private currentPosition () =
    match currentPositionForTests () with
    | Some pos -> pos
    | None ->
        Assert.Fail("Expected UCI current position to be set")
        Unchecked.defaultof<_>

let private assertHashConsistent (pos: Position) =
    let recomputed = hashPosition pos.Board pos.State
    Assert.That(pos.State.HashKey, Is.EqualTo(recomputed))

let private captureSearchOutput (positionCommand: string) depth =
    let api = createApi ()
    let previousOut = Console.Out

    use writer = new StringWriter(CultureInfo.InvariantCulture)

    api.NewGame()
    api.SetPosition positionCommand []

    try
        Console.SetOut(writer)
        let best, ponder = api.Search (searchRequest depth)
        Console.Out.Flush()
        best, ponder, writer.ToString()
    finally
        Console.SetOut(previousOut)

let private firstScoreCp (uciOutput: string) =
    let m = Regex.Match(uciOutput, @"score cp (-?\d+)")
    Assert.That(m.Success, Is.True, $"Expected UCI score cp in output:{Environment.NewLine}{uciOutput}")
    Int32.Parse(m.Groups.[1].Value, CultureInfo.InvariantCulture)

[<SetUp>]
let setup () =
    setLogging false
    clear tt
    resetStats()

[<Test>]
let ``UCI startpos moves e2e4 leaves current position hash consistent`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position startpos moves e2e4" []

    let pos = currentPosition ()

    Assert.That(pos.State.ToPlay, Is.EqualTo(Color.Black))
    assertHashConsistent pos
    Assert.That(evaluate pos, Is.LessThan(0))

    let best, ponder = api.Search depthOneRequest
    Assert.That(best, Is.Not.EqualTo("0000"))
    match ponder with
    | ValueNone -> ()
    | ValueSome mv -> Assert.Fail($"Expected no ponder move, got {mv}")

    assertHashConsistent (currentPosition ())

[<Test>]
let ``UCI fen moves e7e5 leaves current position hash consistent`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position fen rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 moves e7e5" []

    let pos = currentPosition ()

    Assert.That(pos.State.ToPlay, Is.EqualTo(Color.White))
    Assert.That(pos.State.FullMoveNumber, Is.EqualTo(2us))
    assertHashConsistent pos
    Assert.That(evaluate pos, Is.EqualTo(0))

[<Test>]
let ``UCI repeated searches reuse shared TT and advance generation`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position startpos moves e2e4" []

    let g0 = currentGeneration tt

    let best1, _ = api.Search (searchRequest 3)
    let g1 = currentGeneration tt
    let afterFirst = getStats()

    Assert.That(best1, Is.Not.EqualTo("0000"))
    Assert.That(g1, Is.Not.EqualTo(g0))
    Assert.That(afterFirst.Stores, Is.GreaterThan(0L))

    resetStats()

    let best2, _ = api.Search (searchRequest 3)
    let g2 = currentGeneration tt
    let afterSecond = getStats()

    Assert.That(best2, Is.Not.EqualTo("0000"))
    Assert.That(g2, Is.Not.EqualTo(g1))
    Assert.That(afterSecond.Hits, Is.GreaterThan(0L))
    assertHashConsistent (currentPosition ())

[<Test>]
let ``UCI newgame clears current position and TT entries`` () =
    let api = createApi ()
    let sentinelKey = 0x1234567800000001UL

    api.SetPosition "position startpos moves e2e4" []
    store tt sentinelKey 0 12s 12s 1 BoundExact 0uy

    let before = probe tt sentinelKey
    Assert.That(before.Hit, Is.True)
    Assert.That(currentPositionForTests().IsSome, Is.True)

    api.NewGame()

    let after = probe tt sentinelKey
    Assert.That(after.Hit, Is.False)
    Assert.That(currentPositionForTests().IsNone, Is.True)

[<Test>]
let ``UCI position commands replace current position without stale hash state`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position startpos moves e2e4" []
    let afterE4 = currentPosition ()

    api.SetPosition "position startpos moves e2e4 e7e5" []
    let afterE4E5 = currentPosition ()

    assertHashConsistent afterE4
    assertHashConsistent afterE4E5
    Assert.That(afterE4E5.State.ToPlay, Is.EqualTo(Color.White))
    Assert.That(afterE4E5.State.FullMoveNumber, Is.EqualTo(2us))
    Assert.That(afterE4E5.State.HashKey, Is.Not.EqualTo(afterE4.State.HashKey))

[<Test>]
let ``UCI invalid move clears current position instead of leaving stale state`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position startpos moves e2e4" []
    Assert.That(currentPositionForTests().IsSome, Is.True)

    api.SetPosition "position startpos moves e2e5" []

    Assert.That(currentPositionForTests().IsNone, Is.True)

[<Test>]
let ``UCI malformed fen clears current position instead of leaving stale state`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position startpos moves e2e4" []
    Assert.That(currentPositionForTests().IsSome, Is.True)

    api.SetPosition "position fen not-a-valid-fen" []

    Assert.That(currentPositionForTests().IsNone, Is.True)

[<Test>]
let ``UCI fen parsing preserves piece case across repeated position commands`` () =
    let api = createApi ()

    api.NewGame()
    api.SetPosition "position fen 4k3/8/8/8/8/8/8/4Q2K w - - 0 1" []
    let whiteQueenPos = currentPosition ()

    api.SetPosition "position fen 4k3/8/8/8/8/8/8/4q2K b - - 0 1" []
    let blackQueenPos = currentPosition ()

    assertHashConsistent whiteQueenPos
    assertHashConsistent blackQueenPos
    Assert.That(evaluate whiteQueenPos, Is.GreaterThan(0))
    Assert.That(evaluate blackQueenPos, Is.GreaterThan(0))
    Assert.That(whiteQueenPos.State.HashKey, Is.Not.EqualTo(blackQueenPos.State.HashKey))

[<Test>]
let ``UCI score cp is positive when side to move is winning as white or black`` () =
    let whiteBest, _, whiteOutput =
        captureSearchOutput "position fen 4k3/8/8/8/4Q3/8/8/4K3 w - - 0 1" 1

    let blackBest, _, blackOutput =
        captureSearchOutput "position fen 4k3/8/8/8/4q3/8/8/4K3 b - - 0 1" 1

    Assert.That(whiteBest, Is.Not.EqualTo("0000"))
    Assert.That(blackBest, Is.Not.EqualTo("0000"))
    Assert.That(firstScoreCp whiteOutput, Is.GreaterThan(0))
    Assert.That(firstScoreCp blackOutput, Is.GreaterThan(0))

[<Test>]
let ``UCI score cp is negative when side to move is losing`` () =
    let best, _, output =
        captureSearchOutput "position fen 4k3/8/8/8/4Q3/8/8/4K3 b - - 0 1" 1

    Assert.That(best, Is.Not.EqualTo("0000"))
    Assert.That(firstScoreCp output, Is.LessThan(0))

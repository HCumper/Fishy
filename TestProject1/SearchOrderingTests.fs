module SearchOrderingTests

open NUnit.Framework
open Types
open Search
open TranspositionTable
open Zobrist
open Fen
open Uci

open Board1DTestHelpers
open BoardHelpers.Board
open BoardHelpers.PieceCode
open BoardHelpers.Attacks

// --------------------
// Helpers
// --------------------

let mkReq (depth:int) : SearchRequest =
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

let loadPos (fen:string) : Position =
    let b = mkBoard()
    match tryLoadPositionFromFen b fen with
    | ValueNone ->
        Assert.Fail($"FEN failed to load: {fen}")
        Unchecked.defaultof<_>
    | ValueSome p ->
        let h = hashPosition p.Board p.State
        { p with State = { p.State with HashKey = h } }

let mkTT () =
    create { Mb = 16; BucketSize = 4; MaxAge = 8 }

let runChoose (tt:TranspositionTable) (pos:Position) (depth:int) =
    Search.nodeCount <- 0L
    let best = chooseBestMove tt pos (mkReq depth)
    let nodes = Search.nodeCount
    best, nodes

let moveEq (a:Move) (b:Move) = a = b

let firstMove (moves: Move list) =
    match moves with
    | h :: _ -> h
    | [] -> Assert.Fail("Expected non-empty move list"); Unchecked.defaultof<_>

let moveByUci (moves: Move list) (uci: string) =
    match moves |> List.tryFind (fun mv -> moveToUci mv = uci) with
    | Some mv -> mv
    | None ->
        let available = moves |> List.map moveToUci |> String.concat ", "
        Assert.Fail($"Expected move {uci}. Available: {available}")
        Unchecked.defaultof<_>

// --------------------
// Tests
// --------------------

[<TestFixture>]
type SearchOrderingTests () =

    [<Test>]
    member _.``iterative deepening visits more nodes at greater depth`` () =
        let pos =
            loadPos "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"

        let tt = mkTT()

        let _, n1 = runChoose tt pos 1
        clear tt
        let _, n3 = runChoose tt pos 3

        Assert.That(n3, Is.GreaterThan(n1), $"Expected deeper search to visit more nodes. n1={n1}, n3={n3}")

    [<Test>]
    member _.``repeated search still benefits from warmed TT after iterative deepening`` () =
        let pos =
            loadPos "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"

        let tt = mkTT()

        let _, n1 = runChoose tt pos 4
        let _, n2 = runChoose tt pos 4

        Assert.That(n2, Is.LessThan(n1), $"Expected second search to visit fewer nodes. n1={n1}, n2={n2}")

    [<Test>]
    member _.``orderMoves puts TT move first`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        Assert.That(moves.Length, Is.GreaterThan(1), "Need at least two legal moves")

        let ttMv = moves.Head
        let ttPacked = Search.packMove ttMv

        let ordered = Search.orderMoves pos ttPacked moves

        Assert.That(firstMove ordered, Is.EqualTo(ttMv), "Expected TT move to be ordered first")

    [<Test>]
    member _.``orderRootMoves keeps previous PV move first even if TT move differs`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        Assert.That(moves.Length, Is.GreaterThan(2), "Need at least three legal moves")

        let pvMove = moves.[0]
        let ttMove = moves.[1]
        Assert.That(moveEq pvMove ttMove, Is.False, "Need distinct PV and TT moves for this test")

        let ttPacked = Search.packMove ttMove
        let ordered = Search.orderRootMoves pos (ValueSome pvMove) ttPacked moves

        Assert.That(firstMove ordered, Is.EqualTo(pvMove), "Expected previous PV move to stay first at root")

    [<Test>]
    member _.``orderMoves places captures before quiet moves when no TT move`` () =
        // White queen on e4 can capture black queen on e7 and black pawn on a4;
        // also has quiet moves available. We only check capture-before-quiet here.
        let pos =
            loadPos "4k3/4q3/8/8/p3Q3/8/8/4K3 w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let ordered = Search.orderMoves pos 0 moves

        // Find first quiet and first capture positions in the ordered list.
        let indexed = ordered |> List.mapi (fun i mv -> i, mv)

        let firstCaptureIx =
            indexed
            |> List.tryFind (fun (_, mv) ->
                let dst = getSq pos.Board mv.To
                dst <> Empty)
            |> Option.map fst

        let firstQuietIx =
            indexed
            |> List.tryFind (fun (_, mv) ->
                let dst = getSq pos.Board mv.To
                dst = Empty)
            |> Option.map fst

        match firstCaptureIx, firstQuietIx with
        | Some c, Some q ->
            Assert.That(c, Is.LessThan(q), $"Expected first capture before first quiet. captureIx={c}, quietIx={q}")
        | _ ->
            Assert.Fail("Expected at least one capture and one quiet move")

    [<Test>]
    member _.``orderMoves uses MVV-LVA among captures`` () =
        // White queen on e4 can capture:
        // - black queen on e7
        // - black pawn on a4
        // We expect QxQ to be ordered before QxP because victim value is higher.
        let pos =
            loadPos "7k/4q3/8/8/p3Q3/8/8/7K w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let ordered = Search.orderMoves pos 0 moves

        let captureMoves =
            ordered
            |> List.filter (fun mv -> getSq pos.Board mv.To <> Empty)

        Assert.That(captureMoves.Length, Is.GreaterThanOrEqualTo(2), "Need at least two captures")

        let firstCap = captureMoves.[0]
        let victim = getSq pos.Board firstCap.To

        Assert.That(absKind victim, Is.EqualTo(Queen), "Expected most valuable victim capture first")

    [<Test>]
    member _.``SEE scores safe capture as profitable`` () =
        let pos =
            loadPos "4k3/8/8/8/4q3/8/4R3/4K3 w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let capture = moveByUci moves "e2e4"

        Assert.That(Search.see pos capture, Is.EqualTo(900))

    [<Test>]
    member _.``SEE scores poisoned high-value capture as losing`` () =
        let pos =
            loadPos "4r2k/8/8/4p3/4Q3/8/8/7K w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let capture = moveByUci moves "e4e5"

        Assert.That(Search.see pos capture, Is.LessThan(0))

    [<Test>]
    member _.``orderMoves uses SEE before MVV-LVA for captures`` () =
        let pos =
            loadPos "4r2k/8/8/4p3/p3Q3/8/8/7K w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let ordered = Search.orderMoves pos 0 moves
        let safePawnCapture = moveByUci moves "e4a4"
        let poisonedPawnCapture = moveByUci moves "e4e5"

        let indexed = ordered |> List.mapi (fun i mv -> mv, i) |> Map.ofList

        Assert.That(Search.see pos safePawnCapture, Is.GreaterThan(0))
        Assert.That(Search.see pos poisonedPawnCapture, Is.LessThan(0))
        Assert.That(indexed.[safePawnCapture], Is.LessThan(indexed.[poisonedPawnCapture]))

    [<Test>]
    member _.``orderMovesWithKillers places quiet killers after captures before other quiets`` () =
        let pos =
            loadPos "7k/4q3/8/8/p3Q3/8/8/7K w - - 0 1"

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let isCaptureMove mv = getSq pos.Board mv.To <> Empty
        let captures = moves |> List.filter isCaptureMove
        let quiets = moves |> List.filter (isCaptureMove >> not)

        Assert.That(captures.Length, Is.GreaterThanOrEqualTo(2), "Need captures for ordering boundary")
        Assert.That(quiets.Length, Is.GreaterThanOrEqualTo(3), "Need quiet moves for killer ordering")

        let killer1 = quiets.[1]
        let killer2 = quiets.[2]

        let ordered =
            Search.orderMovesWithKillers pos 0 (Search.packMove killer1) (Search.packMove killer2) moves

        let indexed = ordered |> List.mapi (fun i mv -> mv, i) |> Map.ofList
        let killer1Ix = indexed.[killer1]
        let killer2Ix = indexed.[killer2]

        let lastCaptureIx =
            captures
            |> List.map (fun mv -> indexed.[mv])
            |> List.max

        let firstOtherQuietIx =
            quiets
            |> List.filter (fun mv -> mv <> killer1 && mv <> killer2)
            |> List.map (fun mv -> indexed.[mv])
            |> List.min

        Assert.That(lastCaptureIx, Is.LessThan(killer1Ix), "Captures should stay before killer moves")
        Assert.That(killer1Ix, Is.LessThan(killer2Ix), "Primary killer should precede secondary killer")
        Assert.That(killer2Ix, Is.LessThan(firstOtherQuietIx), "Killers should precede ordinary quiet moves")

    [<Test>]
    member _.``orderMovesWithKillers sorts ordinary quiets by history score`` () =
        let pos =
            loadPos "7k/4q3/8/8/p3Q3/8/8/7K w - - 0 1"

        Search.clearHistoryScores()

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let isCaptureMove mv = getSq pos.Board mv.To <> Empty
        let quiets = moves |> List.filter (isCaptureMove >> not)

        Assert.That(quiets.Length, Is.GreaterThanOrEqualTo(3), "Need quiet moves for history ordering")

        let lowHistory = quiets.[0]
        let highHistory = quiets.[2]

        Search.rememberHistoryForTests pos.State.ToPlay 1 lowHistory
        Search.rememberHistoryForTests pos.State.ToPlay 4 highHistory

        let ordered = Search.orderMovesWithKillers pos 0 0 0 moves
        let indexed = ordered |> List.mapi (fun i mv -> mv, i) |> Map.ofList

        Assert.That(indexed.[highHistory], Is.LessThan(indexed.[lowHistory]))

    [<Test>]
    member _.``killer ordering takes priority over history ordering`` () =
        let pos =
            loadPos "7k/4q3/8/8/p3Q3/8/8/7K w - - 0 1"

        Search.clearHistoryScores()

        let moves = GenerateMoves.generateAllLegalMoves pos inCheck
        let isCaptureMove mv = getSq pos.Board mv.To <> Empty
        let quiets = moves |> List.filter (isCaptureMove >> not)

        Assert.That(quiets.Length, Is.GreaterThanOrEqualTo(3), "Need quiet moves for ordering")

        let killer = quiets.[0]
        let historical = quiets.[2]

        Search.rememberHistoryForTests pos.State.ToPlay 8 historical

        let ordered = Search.orderMovesWithKillers pos 0 (Search.packMove killer) 0 moves
        let indexed = ordered |> List.mapi (fun i mv -> mv, i) |> Map.ofList

        Assert.That(indexed.[killer], Is.LessThan(indexed.[historical]))

    [<Test>]
    member _.``root search remains deterministic with root ordering`` () =
        let pos =
            loadPos "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"

        let tt = mkTT()

        let best1, _ = runChoose tt pos 4
        clear tt
        let best2, _ = runChoose tt pos 4

        Assert.That(best1, Is.EqualTo(best2), "Expected deterministic best move for same position/depth")

    [<Test>]
    member _.``depth one root search uses full window without aspiration re-search`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"

        let tt = mkTT()

        let best, _nodes = runChoose tt pos 1

        Assert.That(best, Is.Not.EqualTo(ValueNone))
        Assert.That(Search.aspirationResearchCount, Is.EqualTo(0))

    [<Test>]
    member _.``aspiration windows re-search when score escapes previous iteration window`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"

        let tt = mkTT()

        let best, _nodes = runChoose tt pos 2

        Assert.That(best, Is.Not.EqualTo(ValueNone))
        Assert.That(Search.aspirationResearchCount, Is.GreaterThan(0))

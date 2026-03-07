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
      MoveTimeMs = ValueNone
      WTimeMs = ValueNone
      BTimeMs = ValueNone
      WIncMs = ValueNone
      BIncMs = ValueNone
      Nodes = ValueNone
      Mate = ValueNone
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
    member _.``root search remains deterministic with root ordering`` () =
        let pos =
            loadPos "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"

        let tt = mkTT()

        let best1, _ = runChoose tt pos 4
        clear tt
        let best2, _ = runChoose tt pos 4

        Assert.That(best1, Is.EqualTo(best2), "Expected deterministic best move for same position/depth")

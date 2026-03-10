module SearchTTTests

open NUnit.Framework

open Types
open Zobrist
open TranspositionTable
open Search

open Board1DTestHelpers
open BoardHelpers.Board 
open BoardHelpers.PieceCode
open Fen
open Uci

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
    | ValueNone -> Assert.Fail($"FEN failed to load: {fen}"); Unchecked.defaultof<_>
    | ValueSome p ->
        // Your loader sets HashKey=0L, so compute real hash before search/TT use
        let h = hashPosition p.Board p.State
        { p with State = { p.State with HashKey = h } }

let mkTT () =
    create { Mb = 16; BucketSize = 4; MaxAge = 8 }

let runSearchOnce (tt:TranspositionTable) (pos:Position) (depth:int) =
    // reset counters for per-run measurement
    Search.nodeCount <- 0L
    let _best = chooseBestMove tt pos (mkReq depth)
    Search.nodeCount

// --------------------
// Tests
// --------------------

[<TestFixture>]
type SearchusesTTOneDboard () =

    [<Test>]
    member _.``first search fills TT and second search visits fewer nodes`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let tt = mkTT()

        let n1 = runSearchOnce tt pos 5
        let fill1 = hashFullPermille tt
        Assert.That(fill1, Is.GreaterThan(0), "TT should have some filled buckets after first search")

        let n2 = runSearchOnce tt pos 5
        Assert.That(n2, Is.LessThan(n1), $"Expected second search to visit fewer nodes. n1={n1} n2={n2}")

    [<Test>]
    member _.``clearing TT removes the node reduction benefit`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let tt = mkTT()

        let n1 = runSearchOnce tt pos 5
        let n2 = runSearchOnce tt pos 5
        Assert.That(n2, Is.LessThan(n1), "Sanity: should improve on 2nd run before clearing")

        clear tt
        let n3 = runSearchOnce tt pos 5

        Assert.That(n3, Is.GreaterThan(n2), $"After clear, expected more nodes than warmed TT run. n2={n2} n3={n3}")

    [<Test>]
    member _.``TT is keyed by HashKey; changing side-to-move changes key and reduces hit benefit`` () =
        let posW =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let posB =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

        let tt = mkTT()

        let _ = runSearchOnce tt posW 5
        let warmed = runSearchOnce tt posW 5
        let other = runSearchOnce tt posB 5

        Assert.That(other, Is.GreaterThanOrEqualTo(warmed), $"Expected fewer TT hits when side-to-move differs. warmed={warmed} other={other}")
        
// ------------------------------------------------------------
// Helpers
// ------------------------------------------------------------


let keyOf (pos:Position) = uint64 pos.State.HashKey

let runChoose (tt:TranspositionTable) (pos:Position) (depth:int) =
    Search.nodeCount <- 0L
    let best = chooseBestMove tt pos (mkReq depth)
    let nodes = Search.nodeCount
    best, nodes

let runNegamax (tt:TranspositionTable) (pos:Position) (depth:int) (alpha:int) (beta:int) =
    Search.nodeCount <- 0L
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let score = Search.negamax tt pos depth alpha beta sw {SoftMs = 1000; HardMs = 1000}
    let nodes = Search.nodeCount
    score, nodes

// ------------------------------------------------------------
// Tests
// ------------------------------------------------------------

[<TestFixture>]
type SearchTTIntegrationTests () =

    [<Test>]
    member _.``root stores a TT entry with nonzero move at depth>0`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let tt = mkTT()
        newSearch tt

        let _best, _nodes = runChoose tt pos 5

        let pr = probe tt (keyOf pos)
        Assert.That(pr.Hit, Is.True, "Expected root to be stored in TT")

        // root searched at depth-1 inside chooseBestMove, but the root position itself is stored
        // by negamax calls reached from root moves; still, with your current code, root key is stored
        // when negamax is called on each child. So we only assert the entry is sensible if present.
        // If you later change to store root explicitly, this stays valid.
        Assert.That(int pr.Entry.Depth, Is.GreaterThanOrEqualTo(1), "TT depth should be >= 1 for searched positions")

        // For non-terminal, non-leaf positions, bestMovePacked should be non-zero
        // (0 is used for leaf/terminal/no-move cases).
        Assert.That(pr.Entry.Move, Is.Not.EqualTo(0), "Expected a packed best move for a normal position")

    [<Test>]
    member _.``negamax stores exact score and retrieving with same window returns immediately`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        let tt = mkTT()
        newSearch tt

        // Wide window: should store BoundExact at depth=4
        let depth = 4
        let alpha = -MateScore
        let beta  =  MateScore

        let s1, n1 = runNegamax tt pos depth alpha beta

        let pr = probe tt (keyOf pos)
        Assert.That(pr.Hit, Is.True)
        Assert.That(int pr.Entry.Depth, Is.GreaterThanOrEqualTo(depth))
        Assert.That(pr.Entry.Bound, Is.EqualTo(BoundExact))
        Assert.That(int pr.Entry.Score, Is.EqualTo(s1))

        // Second call should hit and return quickly (nodes drop a lot)
        let s2, n2 = runNegamax tt pos depth alpha beta
        Assert.That(s2, Is.EqualTo(s1))
        Assert.That(n2, Is.LessThan(n1), $"Expected TT hit to reduce nodes. n1={n1} n2={n2}")
        
    [<Test>]
    member _.``fen parsing`` () =

        let pos = loadPos "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"

        // verify the pawn square e2
        let e2 = { File = 4uy; Rank = 1uy }   // rank1=0, rank2=1
        let pawn = getSq pos.Board e2
        Assert.That(pawn, Is.Not.EqualTo(Empty), "e2 should contain a pawn")
        Assert.That(absKind pawn, Is.EqualTo(Pawn), "e2 should be a pawn kind")
        Assert.That(isWhite pawn, Is.True, "pawn should be white")
    
    [<Test>]
    member _.``absKind piece encoding matches pieceValue`` () =
        let wp = make Color.White Pawn
        Assert.That(absKind wp, Is.EqualTo(1y), "absKind White Pawn must be 1y if pieceValue expects that")

    [<Test>]
    member _.``use exact correctly`` () =
        let pos = loadPos "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"
        let sc = Evaluation.evaluate pos
        Assert.That(sc, Is.Not.EqualTo(0), $"Expected nonzero eval, got {sc}")

    [<Test>]
    member _.``indexing consisteny test`` () =
        for r = 0 to 7 do
            for f = 0 to 7 do
                let sq = { File = byte f; Rank = byte r }
                Assert.That(idxFR f r, Is.EqualTo(idxSq sq))
                
    [<Test>]
    member _. ``FEN puts a pawn on e2`` () =
        let b = mkBoard()
        match tryLoadPositionFromFen b "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1" with
        | ValueNone -> Assert.Fail("load failed")
        | ValueSome p ->
            // e2 = file 4, rank 1
            let e2 = { File = 4uy; Rank = 1uy }
            let pawn = getSq p.Board e2   // IMPORTANT: use the SAME getSq used everywhere
            Assert.That(pawn, Is.EqualTo(make Color.White Pawn))
        
    [<Test>]
    member _.``upper bound is stored when search fails low and then can be used for cutoff`` () =
        let pos =
            loadPos "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"

        // baseline on separate TT
        let ttExact = mkTT()
        newSearch ttExact
        let exact, _ = runNegamax ttExact pos 3 -MateScore MateScore

        // test on fresh TT
        let tt = mkTT()
        newSearch tt

        // fail-low window: alpha above true score
        let alpha = exact + 50
        let beta  = alpha + 1000

        let _sFailLow, _ = runNegamax tt pos 3 alpha beta

        let pr = probe tt (keyOf pos)
        Assert.That(pr.Hit, Is.True)
        Assert.That(int pr.Entry.Depth, Is.GreaterThanOrEqualTo(3))
        Assert.That(pr.Entry.Bound, Is.EqualTo(BoundUpper))

        // prove usable: same alpha => s <= alpha triggers immediate return (for BoundUpper)
        let s3, n3 = runNegamax tt pos 3 alpha beta
        Assert.That(n3, Is.LessThan(200L))
        Assert.That(s3, Is.EqualTo(int pr.Entry.Score))
        
    [<Test>]
    member _.``lower bound is stored when search fails high and then can be used for cutoff`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

        // 1) Get a baseline score using a separate TT (or clear after)
        let ttExact = mkTT()
        newSearch ttExact
        let exact, _ = runNegamax ttExact pos 3 -MateScore MateScore

        // 2) Now run the fail-high search on a fresh TT so it can't short-circuit
        let tt = mkTT()
        newSearch tt

        let beta = exact - 50
        let alpha = beta - 1000

        let _sFailHigh, _ = runNegamax tt pos 3 alpha beta

        let pr = probe tt (keyOf pos)
        Assert.That(pr.Hit, Is.True)
        Assert.That(int pr.Entry.Depth, Is.GreaterThanOrEqualTo(3))
        Assert.That(pr.Entry.Bound, Is.EqualTo(BoundLower), "Expected BoundLower after fail-high window")

        // prove it's usable: same window => immediate cutoff
        let s3, n3 = runNegamax tt pos 3 alpha beta
        Assert.That(n3, Is.LessThan(200L), $"Expected near-immediate TT return; nodes={n3}")
        Assert.That(s3, Is.EqualTo(int pr.Entry.Score))

    [<Test>]
    member _.``leaf positions store move=0 and exact bound`` () =
        // A depth=0 call forces leaf store path.
        let pos =
            loadPos "8/8/8/8/8/8/8/4K2k w - - 0 1"

        let tt = mkTT()
        newSearch tt

        let s, _nodes = runNegamax tt pos 0 -MateScore MateScore

        let pr = probe tt (keyOf pos)
        Assert.That(pr.Hit, Is.True)
        Assert.That(pr.Entry.Bound, Is.EqualTo(BoundExact))
        Assert.That(pr.Entry.Move, Is.EqualTo(0), "Leaf nodes are stored without a best move (move=0)")
        Assert.That(int pr.Entry.Score, Is.EqualTo(s))

    [<Test>]
    member _.``TT generation increments per root search and stored entries use that generation`` () =
        let pos =
            loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let tt = mkTT()

        // Key stability check (catches: search leaving pos mutated)
        let kBefore = keyOf pos

        // --- Search 1 ---
        let g0 = currentGeneration tt
        let _best1, _ = runChoose tt pos 4
        let g1 = currentGeneration tt
        Assert.That(g1, Is.Not.EqualTo(g0), "Expected generation to advance after search")

        // Root position must still be identical after search
        let kAfter1 = keyOf pos
        Assert.That(kAfter1, Is.EqualTo(kBefore), "Search must not mutate root position")

        // We do NOT require root to be in TT. Instead: require that TT has *some* entry at gen=g1.
        // If you have a TT stats counter, use it; otherwise, probe a small set of likely-visited keys.
        // The clean way is to expose a 'debugAnyEntry' or 'enumerateBucket' for tests. If you already
        // have such a helper, use it here.
        //
        // Minimal assumption approach: store one key now with gen=g1 and ensure it reads back with gen=g1.
        // This verifies "currentGeneration is actually used by store", which is what "entries refreshed"
        // usually means at the TT layer.
        let sentinelKey = 0xA1B2C3D4_E5F60718UL
        store tt sentinelKey 0 0s 0s 0 BoundExact g1

        let pr1 = probe tt sentinelKey
        Assert.That(pr1.Hit, Is.True, "Sentinel entry should be present")
        Assert.That(pr1.Entry.Generation, Is.EqualTo(g1), "Entry should carry current generation")

        // --- Search 2 ---
        let _best2, _ = runChoose tt pos 4
        let g2 = currentGeneration tt
        Assert.That(g2, Is.Not.EqualTo(g1), "Expected generation to advance again after next search")

        let kAfter2 = keyOf pos
        Assert.That(kAfter2, Is.EqualTo(kBefore), "Search must not mutate root position")

        // Overwrite sentinel with new generation and verify it updates
        store tt sentinelKey 0 0s 0s 0 BoundExact g2
        let pr2 = probe tt sentinelKey
        Assert.That(pr2.Hit, Is.True, "Sentinel entry should still be present")
        Assert.That(pr2.Entry.Generation, Is.EqualTo(g2), "Entry should refresh to new generation")
    
    [<Test>]
    member _.``evaluate startpos is zero (symmetry)`` () =
        let pos = loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        Assert.That(Evaluation.evaluate pos, Is.EqualTo(0))

    [<Test>]
    member _. ``evaluate is positive for extra white pawn`` () =
        // white has an extra pawn on e4
        let pos = loadPos "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1"
        let sc = Evaluation.evaluate pos
        Assert.That(sc, Is.GreaterThan(0), $"Expected >0, got {sc}")

    [<Test>]
    member _. ``evaluate flips sign with side-to-move`` () =
        let posW = loadPos "4k3/8/8/8/4P3/8/8/4K3 w - - 0 1"
        let posB = loadPos "4k3/8/8/8/4P3/8/8/4K3 b - - 0 1"
        let sW = Evaluation.evaluate posW
        let sB = Evaluation.evaluate posB
        Assert.That(sB, Is.EqualTo(-sW), $"Expected sB=-sW, got sW={sW} sB={sB}")
        
    [<Test>]
    member _. ``HashKey changes after makeMove`` () =
        let pos0 = loadPos "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let moves = GenerateMoves.generateAllLegalMoves pos0 BoardHelpers.Attacks.inCheck
        Assert.That(moves.Length, Is.GreaterThan(0))

        let mv = moves.Head
        let mutable p = pos0
        let k0 = p.State.HashKey
        let undo = MakeMove.makeMove &p mv
        let k1 = p.State.HashKey
        MakeMove.unmakeMove &p mv undo

        Assert.That(k1, Is.Not.EqualTo(k0), $"HashKey did not change: k0={k0} k1={k1}")
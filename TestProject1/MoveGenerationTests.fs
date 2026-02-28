module TestProject1.MoveGenerationTests

/// <summary>
/// Move generation tests using FEN positions.
/// NOTE: Coordinates are 1-indexed throughout these tests:
///   - Files: a=1, b=2, c=3, d=4, e=5, f=6, g=7, h=8
///   - Ranks: 1, 2, 3, 4, 5, 6, 7, 8
/// </summary>

open Types
open Fen
open BoardHelpers
open GenerateMoves
open Attacks
open NUnit.Framework

// ============================================================================
// Test Helpers
// ============================================================================

/// Parse a square string like "e4" into 1-indexed coordinates (5, 4)
let parseSquare (sq: string) : int * int =
    if sq.Length <> 2 then
        failwithf "Invalid square notation: %s" sq
    let file = int (System.Char.ToLower sq.[0]) - int 'a' + 1
    let rank = int sq.[1] - int '0'
    (file, rank)

/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

/// Helper to run a test with a position loaded from FEN
let private withPosition (fen: string) (testName: string) (action: Position -> unit) =
    match initBoardFromFen fen with
    | ValueSome pos -> action pos
    | ValueNone -> Assert.Fail($"FEN parse failed: {testName}")

// ============================================================================
// Test Case Types
// ============================================================================

type MoveGenCase =
    { Name: string
      Fen: string
      FromSquare: string  // e.g., "e4"
      ExpectedMoves: int
      Gen: Position -> Coordinates -> (Position -> Color -> bool) -> Move list }

type AllMovesCase =
    { Name: string
      Fen: string
      ExpectedMoves: int }

// ============================================================================
// Test Case Data - Static members for NUnit TestCaseSource
// ============================================================================

type TestData() =
    static member KnightTestCases 
        with get() : MoveGenCase list =
            [
                { Name = "Knight moves from starting position (g1 -> 2 moves)"
                  Fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                  FromSquare = "g1"
                  ExpectedMoves = 2
                  Gen = generateLegalKnightMoves }

                { Name = "Knight pinned by bishop - zero moves (c3)"
                  Fen = "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4"
                  FromSquare = "c3"
                  ExpectedMoves = 0
                  Gen = generateLegalKnightMoves }

                { Name = "Knight with five legal moves (c3)"
                  Fen = "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"
                  FromSquare = "c3"
                  ExpectedMoves = 5
                  Gen = generateLegalKnightMoves }

                { Name = "Knight in center with maximum mobility (e4 -> 8 moves)"
                  Fen = "k7/8/8/8/4N3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 8
                  Gen = generateLegalKnightMoves }
            ]

    static member BishopTestCases
        with get() : MoveGenCase list =
            [
                { Name = "Black bishop with seven moves (b4)"
                  Fen = "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR b KQkq - 0 4"
                  FromSquare = "b4"
                  ExpectedMoves = 7
                  Gen = generateLegalBishopMoves }

                { Name = "Bishop pinned - can only move along pin ray"
                  Fen = "7k/7b/8/8/8/3B4/2K5/8 w - - 0 1"
                  FromSquare = "d3"
                  ExpectedMoves = 4
                  Gen = generateLegalBishopMoves }

                { Name = "Bishop in center with maximum diagonal mobility"
                  Fen = "k7/8/8/8/4B3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 13
                  Gen = generateLegalBishopMoves }
            ]

    static member RookTestCases
        with get() : MoveGenCase list =
            [
                { Name = "White rook with three moves (g3)"
                  Fen = "rnb2r2/pppp1p1k/4p1pp/6q1/1b1Pn2P/2NB1NR1/PPPB1PP1/R2QK3 w Q - 0 10"
                  FromSquare = "g3"
                  ExpectedMoves = 3
                  Gen = generateLegalRookMoves }

                { Name = "White rook pinned - one move blocks check (f2)"
                  Fen = "rnb2rk1/pppp1ppp/4p3/8/Pb1P4/2NB1N2/1PPB1RPP/R2Qq1K1 w - - 0 11"
                  FromSquare = "f2"
                  ExpectedMoves = 1
                  Gen = generateLegalRookMoves }

                { Name = "Rook pinned along file - can move up/down only"
                  Fen = "7k/8/8/3r4/8/3R4/8/3K4 w - - 0 1"
                  FromSquare = "d3"
                  ExpectedMoves = 3
                  Gen = generateLegalRookMoves }

                { Name = "Rook in open center with maximum mobility"
                  Fen = "k7/8/8/8/4R3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 13
                  Gen = generateLegalRookMoves }
            ]

    static member QueenTestCases
        with get() : MoveGenCase list =
            [
                { Name = "Black queen with nineteen moves (g5)"
                  Fen = "rnb2rk1/pppp1ppp/4p3/6q1/1b1Pn3/2NB1N2/PPPB1PPP/R2Q1RK1 b - - 0 7"
                  FromSquare = "g5"
                  ExpectedMoves = 19
                  Gen = generateLegalQueenMoves }

                { Name = "Queen pinned - zero moves perpendicular to pin"
                  Fen = "7k/8/8/3r4/8/3Q4/8/3K4 w - - 0 1"
                  FromSquare = "d3"
                  ExpectedMoves = 3
                  Gen = generateLegalQueenMoves }

                { Name = "Queen in open center with maximum mobility"
                  Fen = "8/k7/8/8/4Q3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 26
                  Gen = generateLegalQueenMoves }
            ]

    static member PawnTestCases
        with get() : MoveGenCase list =
            [
                { Name = "White pawn initial - double push available (e2 -> e3, e4)"
                  Fen = "k7/8/8/8/8/8/4P3/4K3 w - - 0 1"
                  FromSquare = "e2"
                  ExpectedMoves = 2
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn blocked directly ahead - zero moves"
                  Fen = "k7/8/8/8/8/4p3/4P3/4K3 w - - 0 1"
                  FromSquare = "e2"
                  ExpectedMoves = 0
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn double push blocked on second square - single push only"
                  Fen = "k7/8/8/8/4n3/8/4P3/4K3 w - - 0 1"
                  FromSquare = "e2"
                  ExpectedMoves = 1
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn single push only - not on start rank (e3 -> e4)"
                  Fen = "k7/8/8/8/8/4P3/8/4K3 w - - 0 1"
                  FromSquare = "e3"
                  ExpectedMoves = 1
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn with two capture options (e4 captures d5, f5)"
                  Fen = "7k/8/8/3p1p2/4P3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 3
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn forward plus one capture (e4)"
                  Fen = "7k/8/8/3p4/4P3/8/8/4K3 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 2
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn promotion - quiet (e7 -> e8, four promotions)"
                  Fen = "7k/4P3/8/8/8/8/8/4K3 w - - 0 1"
                  FromSquare = "e7"
                  ExpectedMoves = 4
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn promotion - with captures (e7 captures d8, f8)"
                  Fen = "k2r1r2/4P3/8/8/8/8/8/4K3 w - - 0 1"
                  FromSquare = "e7"
                  ExpectedMoves = 12
                  Gen = generateLegalPawnMoves }

                { Name = "White pawn en passant capture available (e5xd6 ep)"
                  Fen = "k7/8/8/3pP3/8/8/8/4K3 w - d6 0 1"
                  FromSquare = "e5"
                  ExpectedMoves = 2
                  Gen = generateLegalPawnMoves }

                { Name = "Black pawn initial - double push available (e7 -> e6, e5)"
                  Fen = "4k3/4p3/8/8/8/8/8/4K3 b - - 0 1"
                  FromSquare = "e7"
                  ExpectedMoves = 2
                  Gen = generateLegalPawnMoves }

                { Name = "Black pawn promotion - quiet (e2 -> e1, four promotions)"
                  Fen = "K3k3/8/8/8/8/8/4p3/8 b - - 0 1"
                  FromSquare = "e2"
                  ExpectedMoves = 4
                  Gen = generateLegalPawnMoves }

                { Name = "Black pawn with two capture options"
                  Fen = "4k3/8/8/4p3/3P1P2/8/8/4K3 b - - 0 1"
                  FromSquare = "e5"
                  ExpectedMoves = 3
                  Gen = generateLegalPawnMoves }

                { Name = "Pawn pinned - cannot capture or move forward"
                  Fen = "7k/8/8/3r4/8/3P4/8/3K4 w - - 0 1"
                  FromSquare = "d3"
                  ExpectedMoves = 1
                  Gen = generateLegalPawnMoves }

                { Name = "En passant would expose king to check - illegal"
                  Fen = "k7/8/8/KPp4r/8/8/8/8 w - c6 0 1"
                  FromSquare = "b5"
                  ExpectedMoves = 1
                  Gen = generateLegalPawnMoves }
            ]

    static member KingTestCases
        with get() : MoveGenCase list =
            [
                { Name = "White king in open center - eight moves (e4)"
                  Fen = "k7/8/8/8/4K3/8/8/8 w - - 0 1"
                  FromSquare = "e4"
                  ExpectedMoves = 8
                  Gen = generateLegalKingMoves }

                { Name = "White king in corner - three moves (a1)"
                  Fen = "k7/8/8/8/8/8/8/K7 w - - 0 1"
                  FromSquare = "a1"
                  ExpectedMoves = 3
                  Gen = generateLegalKingMoves }

                { Name = "King adjacency prohibited - two safe moves (Ke1 vs ke3)"
                  Fen = "8/8/8/8/8/4k3/8/4K3 w - - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 2
                  Gen = generateLegalKingMoves }

                { Name = "King in check by rook - four escape squares (Ke1 vs Re8)"
                  Fen = "k3r3/8/8/8/8/8/8/4K3 w - - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 4
                  Gen = generateLegalKingMoves }

                { Name = "King can castle kingside (Ke1, K rights -> includes O-O)"
                  Fen = "k7/8/8/8/8/8/8/4K2R w K - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 6
                  Gen = generateLegalKingMoves }

                { Name = "Castling blocked - transit square attacked (f1 under attack)"
                  Fen = "k4r2/8/8/8/8/8/8/4K2R w K - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 3
                  Gen = generateLegalKingMoves }

                { Name = "King in check - cannot castle"
                  Fen = "k7/8/8/8/8/8/4r3/4K2R w K - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 3
                  Gen = generateLegalKingMoves }

                { Name = "Can castle queenside (O-O-O)"
                  Fen = "8/8/6k1/8/8/8/8/R3K3 w Q - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 6
                  Gen = generateLegalKingMoves }

                { Name = "Can castle even when rook is attacked"
                  Fen = "kr6/8/8/8/8/8/8/4K2R w K - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 6
                  Gen = generateLegalKingMoves }

                { Name = "Cannot castle queenside through check (d1 attacked)"
                  Fen = "k2r4/8/8/8/8/8/8/R3K3 w Q - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 3
                  Gen = generateLegalKingMoves }

                { Name = "King in double check - must move king only"
                  Fen = "k7/8/8/8/3b4/8/8/3RK2r w - - 0 1"
                  FromSquare = "e1"
                  ExpectedMoves = 2
                  Gen = generateLegalKingMoves }
            ]

    static member AllMovesTestCases
        with get() : AllMovesCase list =
            [
                { Name = "Start position - twenty legal moves"
                  Fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                  ExpectedMoves = 20 }

                { Name = "Lone white king in center - eight moves"
                  Fen = "k7/8/8/8/4K3/8/8/8 w - - 0 1"
                  ExpectedMoves = 8 }

                { Name = "King in check by rook - four escape moves only"
                  Fen = "k3r3/8/8/8/8/8/8/4K3 w - - 0 1"
                  ExpectedMoves = 4 }

                { Name = "Position with kingside castling available"
                  Fen = "k7/8/8/8/8/8/8/4K2R w K - 0 1"
                  ExpectedMoves = 15 }

                { Name = "Single pawn with king - six total moves"
                  Fen = "k7/8/8/8/8/8/4P3/4K3 w - - 0 1"
                  ExpectedMoves = 6 }

                { Name = "Pinned knight - only king can move"
                  Fen = "7k/8/8/3r4/8/3N4/8/3K4 w - - 0 1"
                  ExpectedMoves = 5 }

                { Name = "Can block check with piece"
                  Fen = "7k/8/8/3r4/8/4B3/8/3K4 w - - 0 1"
                  ExpectedMoves = 6 }

                { Name = "Checkmate - zero legal moves"
                  Fen = "3R3k/6pp/8/8/8/8/8/7K b - - 0 1"
                  ExpectedMoves = 0 }

                { Name = "Stalemate - zero legal moves (not in check)"
                  Fen = "k7/8/1Q6/1K6/8/8/8/8 b - - 0 1"
                  ExpectedMoves = 0 }

                { Name = "Double check - only king moves allowed"
                  Fen = "k7/8/8/8/3b4/8/8/3RK2r w - - 0 1"
                  ExpectedMoves = 2 }

                { Name = "Complex middlegame position"
                  Fen = "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq e6 0 4"
                  ExpectedMoves = 33 }
            ]

// ============================================================================
// Test Runners
// ============================================================================

let private runMoveGenCase (c: MoveGenCase) =
    withPosition c.Fen c.Name (fun pos ->
        let (file, rank) = parseSquare c.FromSquare
        let fromSq = Coordinates.createInts file rank
        let moves = c.Gen pos fromSq inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves), 
            $"{c.Name} - Expected {c.ExpectedMoves} moves from {c.FromSquare}, got {moves.Length}")
    )

let private runAllMovesCase (c: AllMovesCase) =
    withPosition c.Fen c.Name (fun pos ->
        let moves = generateAllLegalMoves pos inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves),
            $"{c.Name} - Expected {c.ExpectedMoves} total moves, got {moves.Length}")
    )

// ============================================================================
// Parameterized Tests
// ============================================================================

[<TestCaseSource(typeof<TestData>, "KnightTestCases")>]
let ``Knight move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "BishopTestCases")>]
let ``Bishop move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "RookTestCases")>]
let ``Rook move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "QueenTestCases")>]
let ``Queen move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "PawnTestCases")>]
let ``Pawn move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "KingTestCases")>]
let ``King move generation`` (case: MoveGenCase) =
    runMoveGenCase case

[<TestCaseSource(typeof<TestData>, "AllMovesTestCases")>]
let ``All legal moves generation`` (case: AllMovesCase) =
    runAllMovesCase case

// ============================================================================
// Additional Validation Tests
// ============================================================================

[<Test>]
let ``Square parser validation`` () =
    let testCases = 
        [
            ("a1", (1, 1))
            ("h8", (8, 8))
            ("e4", (5, 4))
            ("d2", (4, 2))
        ]
    
    for (input, expected) in testCases do
        let result = parseSquare input
        Assert.That(result, Is.EqualTo(expected), $"parseSquare {input}")

[<Test>]
let ``FEN parser handles all test positions`` () =
    let allFens = 
        List.concat [
            TestData.KnightTestCases |> List.map (fun c -> c.Fen)
            TestData.BishopTestCases |> List.map (fun c -> c.Fen)
            TestData.RookTestCases |> List.map (fun c -> c.Fen)
            TestData.QueenTestCases |> List.map (fun c -> c.Fen)
            TestData.PawnTestCases |> List.map (fun c -> c.Fen)
            TestData.KingTestCases |> List.map (fun c -> c.Fen)
            TestData.AllMovesTestCases |> List.map (fun c -> c.Fen)
        ]
        |> List.distinct
    
    for fen in allFens do
        match initBoardFromFen fen with
        | ValueSome _ -> ()  // Success
        | ValueNone -> Assert.Fail($"Failed to parse FEN: {fen}")

// ============================================================================
// Edge Case Tests
// ============================================================================

[<Test>]
let ``Underpromotion generates all four piece types`` () =
    withPosition "7k/4P3/8/8/8/8/8/4K3 w - - 0 1" "Underpromotion test" (fun pos ->
        let fromSq = Coordinates.createInts 5 7  // e7
        let moves = generateLegalPawnMoves pos fromSq inCheck
        
        // Should be 4 moves: e8=Q, e8=R, e8=B, e8=N
        Assert.That(moves.Length, Is.EqualTo(4))
        
        // Verify all four promotion types are present
        let promotionTypes = moves |> List.map (fun m -> m.PromoteTo) |> List.distinct
        Assert.That(promotionTypes.Length, Is.EqualTo(4), 
            "Should have 4 different promotion piece types")
    )

[<Test>]
let ``Castling rights are respected`` () =
    // Position where castling would be legal if rights were present
    let fenNoRights = "2k5/8/8/8/8/8/8/R3K2R w - - 0 1"
    let fenWithRights = "2k5/8/8/8/8/8/8/R3K2R w KQ - 0 1"
    
    withPosition fenNoRights "No castling rights" (fun pos ->
        let coord = Coordinates.createInts 5 1  // e1
        let moves = generateLegalKingMoves pos coord inCheck
        let castlingMoves = moves |> List.filter (fun m -> 
            // Castling is king moving 2 squares
            abs (int (m.To.File - m.From.File)) = 2
        )
        Assert.That(castlingMoves.Length, Is.EqualTo(0), 
            "Should have no castling moves without rights")
    )
    
    withPosition fenWithRights "With castling rights" (fun pos ->
        let moves = generateAllLegalMoves pos inCheck
        let castlingMoves = moves |> List.filter (fun m -> 
            abs (int (m.To.File - m.From.File)) = 2
        )
        Assert.That(castlingMoves.Length, Is.EqualTo(2), 
            "Should have both kingside and queenside castling")
    )

[<Test>]
let ``Moves from invalid square return empty list`` () =
    withPosition "k7/8/8/8/4K3/8/8/8 w - - 0 1" "Invalid square test" (fun pos ->
        // Try to generate moves from an empty square
        let fromSq = Coordinates.createInts 4 4  // d4 (empty)
        let knightMoves = generateLegalKnightMoves pos fromSq inCheck
        let bishopMoves = generateLegalBishopMoves pos fromSq inCheck
        let rookMoves = generateLegalRookMoves pos fromSq inCheck
        
        Assert.That(knightMoves.Length, Is.EqualTo(0))
        Assert.That(bishopMoves.Length, Is.EqualTo(0))
        Assert.That(rookMoves.Length, Is.EqualTo(0))
    )
    
type PerftCase =
    { Name: string
      Fen: string
      Expected: (int * int64)[] }   // (depth, nodes)

let private runCase (c: PerftCase) =
    match initBoardFromFen c.Fen with
    | ValueSome pos ->
        for depth, nodes in c.Expected do
            let got = perft pos depth
            Assert.That(got, Is.EqualTo(nodes), $"{c.Name}: depth {depth}")
    | ValueNone ->
        Assert.Fail($"FEN parse failed: {c.Name}")

[<Test>]
let ``Perft standard positions to depth 4`` () =
    let d4 a b c d = [| 1, a; 2, b; 3, c; 4, d |]

    let cases : PerftCase list =
        [
            // Initial position
            { Name = "Initial"
              Fen  = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
              Expected = d4 20L 400L 8902L 197281L }

            // Position 2 (Kiwipete)
            { Name = "Position 2"
              Fen  = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
              Expected = d4 48L 2039L 97862L 4085603L }

            // Position 3
            { Name = "Position 3"
              Fen  = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
              Expected = d4 14L 191L 2812L 43238L }

            // Position 4
            { Name = "Position 4"
              Fen  = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
              Expected = d4 6L 264L 9467L 422333L }

            // Position 5
            { Name = "Position 5"
              Fen  = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
              Expected = d4 44L 1486L 62379L 2103487L }

            // Position 6
            { Name = "Position 6"
              Fen  = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
              Expected = d4 46L 2079L 89890L 3894594L }
        ]

    cases |> List.iter runCase
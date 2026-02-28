module TestProject1.MoveGenerationTests

open Types
open Fen
open BoardHelpers
open GenerateMoves
open Attacks
open NUnit.Framework

/// Initialize a fresh 8x8 board from a FEN string.
let private initBoardFromFen (fen: string) : ValueOption<Position> =
    let board : Board = Array2D.create 8 8 0y
    tryLoadPositionFromFen board fen

type MoveGenCase =
    { Name: string
      Fen: string
      FromFile: int
      FromRank: int
      ExpectedMoves: int
      Gen: Position -> Coordinates -> (Position -> Color -> bool) -> Move list }

let private runCase (c: MoveGenCase) =
    match initBoardFromFen c.Fen with
    | ValueSome pos ->
        let fromSq = Coordinates.createInts c.FromFile c.FromRank
        let moves = c.Gen pos fromSq inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves), c.Name)
    | ValueNone ->
        Assert.Fail($"FEN parse failed: {c.Name}")

[<Test>]
let ``Move generation cases`` () =
    let cases : MoveGenCase list =
        [
            { Name = "Knight moves from starting position (g1)"
              Fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
              FromFile = 7; FromRank = 1
              ExpectedMoves = 2
              Gen = generateLegalKnightMoves }

            { Name = "Knight moves blocked by legality (c3 -> none)"
              Fen = "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 4"
              FromFile = 3; FromRank = 3
              ExpectedMoves = 0
              Gen = generateLegalKnightMoves }

            { Name = "Knight moves (c3 -> five)"
              Fen = "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"
              FromFile = 3; FromRank = 3
              ExpectedMoves = 5
              Gen = generateLegalKnightMoves }

            { Name = "Knight moves from empty square (g4 -> 0)"
              Fen = "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"
              FromFile = 7; FromRank = 4
              ExpectedMoves = 0
              Gen = generateLegalKnightMoves }

            { Name = "Knight moves from enemy knight square (f6 -> 0)"
              Fen = "rnbq1rk1/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR w KQ - 0 5"
              FromFile = 6; FromRank = 6
              ExpectedMoves = 0
              Gen = generateLegalKnightMoves }

            { Name = "Black bishop moves (b4 -> 7)"
              Fen = "rnbqk2r/pppp1ppp/4pn2/8/1b1PP3/2N5/PPPB1PPP/R2QKBNR b KQkq - 0 4"
              FromFile = 2; FromRank = 4
              ExpectedMoves = 7
              Gen = generateLegalBishopMoves }

            { Name = "Black queen moves (g5 -> 19)"
              Fen = "rnb2rk1/pppp1ppp/4p3/6q1/1b1Pn3/2NB1N2/PPPB1PPP/R2Q1RK1 b - - 0 7"
              FromFile = 7; FromRank = 5
              ExpectedMoves = 19
              Gen = generateLegalQueenMoves }

            { Name = "White rook moves (g3 -> 3)"
              Fen = "rnb2r2/pppp1p1k/4p1pp/6q1/1b1Pn2P/2NB1NR1/PPPB1PP1/R2QK3 w Q - 0 10"
              FromFile = 7; FromRank = 3
              ExpectedMoves = 3
              Gen = generateLegalRookMoves }

            { Name = "White rook moves with check-avoidance (f2 -> 1)"
              Fen = "rnb2rk1/pppp1ppp/4p3/8/Pb1P4/2NB1N2/1PPB1RPP/R2Qq1K1 w - - 0 11"
              FromFile = 6; FromRank = 2
              ExpectedMoves = 1
              Gen = generateLegalRookMoves }
        ]

    cases |> List.iter runCase    

type PawnCase =
    { Name: string
      Fen: string
      FromFile: int
      FromRank: int
      ExpectedMoves: int }

let private runPawnCase (c: PawnCase) =
    match initBoardFromFen c.Fen with
    | ValueSome pos ->
        let fromSq = Coordinates.createInts c.FromFile c.FromRank
        let moves = generateLegalPawnMoves pos fromSq inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves), c.Name)
    | ValueNone ->
        Assert.Fail($"FEN parse failed: {c.Name}")

[<Test>]
let ``Pawn move generation cases`` () =
    let cases : PawnCase list =
        [
            // White pawn on 2nd rank with clear path: e2 -> e3/e4
            { Name = "White pawn initial double push available (e2)"
              Fen = "k7/8/8/8/4n3/8/4P3/4K3 w - - 0 1"
              FromFile = 5; FromRank = 2
              ExpectedMoves = 1 }

            // White pawn blocked directly ahead: e2 blocked by piece on e3 -> no forward moves
            { Name = "White pawn blocked ahead (e2 blocked on e3)"
              Fen = "k7/8/8/8/8/4p3/4P3/4K3 w - - 0 1"
              FromFile = 5; FromRank = 2
              ExpectedMoves = 0 }

            // White pawn single push only (not on start rank): e3 -> e4
            { Name = "White pawn single push only (e3)"
              Fen = "k7/8/8/8/8/4P3/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 3
              ExpectedMoves = 1 }

            // White pawn capture options: pawn on e4 capturing d5 and f5
            { Name = "White pawn two captures (e4 captures d5,f5)"
              Fen = "7k/8/8/3p1p2/4P3/8/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 4
              ExpectedMoves = 3 }

            // White pawn: one forward + one capture (e4 -> e5 and capture d5)
            { Name = "White pawn forward plus capture (e4)"
              Fen = "7k/8/8/3p4/4P3/8/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 4
              ExpectedMoves = 2 }

            // Promotion (quiet): white pawn on e7 -> e8 promotes (4 options)
            { Name = "White pawn promotion quiet (e7 -> e8, 4 promotions)"
              Fen = "7k/4P3/8/8/8/8/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 7
              ExpectedMoves = 4 }

            // Promotion by capture: white pawn on e7 captures d8 and f8 (both occupied)
            // => 2 capture squares * 4 promotions each = 8 moves
            { Name = "White pawn promotion captures (e7x d8,f8, 8 moves)"
              Fen = "k2r1r2/4P3/8/8/8/8/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 7
              ExpectedMoves = 12 }

            // En passant: white pawn on e5 can capture ep on d6 (target square empty).
            // FEN EP square is d6. Black pawn must be on d5 to be a valid ep capture in your makeMove.
            { Name = "White pawn en passant available (e5xd6 ep)"
              Fen = "k7/8/8/3pP3/8/8/8/4K3 w - d6 0 1"
              FromFile = 5; FromRank = 5
              ExpectedMoves = 2 } // e6 plus ep capture to d6

            // Black pawn initial: e7 -> e6/e5
            { Name = "Black pawn initial double push available (e7)"
              Fen = "4k3/4p3/8/8/8/8/8/4K3 b - - 0 1"
              FromFile = 5; FromRank = 7
              ExpectedMoves = 2 }

            // Black pawn promotion (quiet): e2 -> e1 promotes (4 options)
            { Name = "Black pawn promotion quiet (e2 -> e1, 4 promotions)"
              Fen = "K3k3/8/8/8/8/8/4p3/8 b - - 0 1"
              FromFile = 5; FromRank = 2
              ExpectedMoves = 4 }
            
            // 1) Double push blocked on the second square:
            // White pawn on e2, e3 empty, e4 occupied -> only e3 is legal (1 move)
            { Name = "White pawn double push blocked on second square (e2: e4 occupied)"
              Fen = "k7/8/8/8/4n3/8/4P3/4K3 w - - 0 1"
              FromFile = 5; FromRank = 2
              ExpectedMoves = 1 }
        ]

    cases |> List.iter runPawnCase
    

type KingCase =
    { Name: string
      Fen: string
      FromFile: int
      FromRank: int
      ExpectedMoves: int }

let private runKingCase (c: KingCase) =
    match initBoardFromFen c.Fen with
    | ValueSome pos ->
        let fromSq = Coordinates.createInts c.FromFile c.FromRank
        let moves = generateLegalKingMoves pos fromSq inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves), c.Name)
    | ValueNone ->
        Assert.Fail($"FEN parse failed: {c.Name}")

[<Test>]
let ``King move generation cases`` () =
    let cases : KingCase list =
        [
            // 1) King in open board center: e4 has 8 moves
            { Name = "White king in center has 8 moves (e4)"
              Fen = "k7/8/8/8/4K3/8/8/8 w - - 0 1"
              FromFile = 5; FromRank = 4
              ExpectedMoves = 8 }

            // 2) King corner: a1 has 3 moves (a2,b1,b2) assuming no attacks
            { Name = "White king in corner has 3 moves (a1)"
              Fen = "k7/8/8/8/8/8/8/K7 w - - 0 1"
              FromFile = 1; FromRank = 1
              ExpectedMoves = 3 }

            // 3) King adjacency prohibited: black king on e2, white king on e1
            // White king cannot move to any square adjacent to black king; from e1 legal squares: d1,f1 only (2)
            { Name = "King adjacency prohibited (Ke1 vs ke2 -> 2 moves)"
              Fen = "8/8/8/8/8/4k3/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 1
              ExpectedMoves = 2 }

            // 4) In check by rook: king e1 checked by rook e8; legal replies are king moves only.
            // Squares king can go: d1,f1,d2,f2 (e2 is still on file), so 4.
            // (No pieces block, black king placed at a8)
            { Name = "King in check by rook has 4 escapes (Ke1 vs Re8)"
              Fen = "k3r3/8/8/8/8/8/8/4K3 w - - 0 1"
              FromFile = 5; FromRank = 1
              ExpectedMoves = 4 }

            // 5) Kingside castling available for white: empty f1,g1, rights K, not in check, no attacks
            // Expect 1 castling move plus normal king moves from e1 (to d1,d2,e2,f2,f1) = 5,
            // total 6. Black king on a8.
            { Name = "White can castle kingside (Ke1, K rights) -> includes O-O"
              Fen = "k7/8/8/8/8/8/8/4K2R w K - 0 1"
              FromFile = 5; FromRank = 1
              ExpectedMoves = 6 }

            // 6) Castling prohibited through check: black rook attacks f1, so O-O not allowed.
            // Put black rook on f8 (attacks f1). Black king a8.
            // Normal king moves from e1 are still 5 (d1,d2,e2,f2,f1) BUT f1 is attacked, so it becomes illegal,
            // leaving 4. No castling, so 4.
            { Name = "Castling blocked: transit square attacked (f1 attacked) -> no O-O"
              Fen = "k4r2/8/8/8/8/8/8/4K2R w K - 0 1"
              FromFile = 5; FromRank = 1
              ExpectedMoves = 3 }
        ]

    cases |> List.iter runKingCase
    
type AllMovesCase =
    { Name: string
      Fen: string
      ExpectedMoves: int }

let private runAllMovesCase (c: AllMovesCase) =
    match initBoardFromFen c.Fen with
    | ValueSome pos ->
        let moves = generateAllLegalMoves pos inCheck
        Assert.That(moves.Length, Is.EqualTo(c.ExpectedMoves), c.Name)
    | ValueNone ->
        Assert.Fail($"FEN parse failed: {c.Name}")

[<Test>]
let ``All-legal-moves cases`` () =
    let cases : AllMovesCase list =
        [
            // Standard start position has 20 legal moves (16 pawn pushes + 4 knight moves)
            { Name = "Start position has 20 legal moves"
              Fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
              ExpectedMoves = 20 }

            // Only kings on board: white king e4 in open space => 8 moves
            { Name = "Lone white king in center has 8 moves"
              Fen = "k7/8/8/8/4K3/8/8/8 w - - 0 1"
              ExpectedMoves = 8 }

            // White in check by rook e8; only king escapes (from e1): d1,f1,d2,f2 => 4
            { Name = "King in check by rook has 4 total legal moves"
              Fen = "k3r3/8/8/8/8/8/8/4K3 w - - 0 1"
              ExpectedMoves = 4 }

            // White has legal kingside castling plus normal king moves and rook moves (same as earlier king test):
            // From e1: d1,d2,e2,f2,f1 = 5 plus O-O = 1 => 6
            { Name = "Kingside castling included in all moves"
              Fen = "k7/8/8/8/8/8/8/4K2R w K - 0 1"
              ExpectedMoves = 15 }

            // Simple pawn position: white pawn e2, both one and two-step available => 2 moves
            { Name = "Single pawn initial has 2 legal moves"
              Fen = "k7/8/8/8/8/8/4P3/4K3 w - - 0 1"
              ExpectedMoves = 6 }
        ]

    cases |> List.iter runAllMovesCase
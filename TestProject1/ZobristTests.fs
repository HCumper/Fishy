module ZobristTests

open NUnit.Framework
open Types
open Zobrist
open BoardHelpers
open CastlingRights
open PieceCode

[<TestFixture>]
type ``Zobrist tests`` () =

    // Board = sbyte[,]
    let mkBoard () : Board =
        Array2D.create 8 8 0y

    let mkGs (toPlay:Color) (rights:byte) (ep:ValueOption<Coordinates>) : GameState =
        { HashKey = 0L
          EPSquare = ep
          FullMoveNumber = 1us
          CastlingRights = rights
          HalfMoveClock = 0uy
          ToPlay = toPlay }

    [<Test>]
    member _.``hashPosition deterministic for same board/state`` () =
        let b = mkBoard()
        // 0-based: e1 = (4,0), e8 = (4,7), d2 = (3,1), d7 = (3,6)
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)
        Board.set b 3 1 (make Color.White Pawn)
        Board.set b 3 6 (make Color.Black Pawn)

        let gs = mkGs Color.White (WK ||| WQ ||| BK ||| BQ) ValueNone
        let h1 = hashPosition b gs
        let h2 = hashPosition b gs
        Assert.That(h2, Is.EqualTo(h1))

    [<Test>]
    member _.``togglePiece is involution`` () =
        let p = make Color.White Knight
        let f0 = 2
        let r0 = 5
        let h0 = 0L
        let h1 = togglePiece p f0 r0 h0
        let h2 = togglePiece p f0 r0 h1
        Assert.That(h2, Is.EqualTo(h0))

    [<Test>]
    member _.``side to move differs by toggleBlackToMove`` () =
        let b = mkBoard()
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)

        let hW = hashPosition b (mkGs Color.White 0uy ValueNone)
        let hB = hashPosition b (mkGs Color.Black 0uy ValueNone)

        Assert.That(hB, Is.EqualTo(toggleBlackToMove hW))

    [<Test>]
    member _.``castling flags change hash by XORing their keys`` () =
        let b = mkBoard()
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)

        let baseGs = mkGs Color.White 0uy ValueNone
        let h0 = hashPosition b baseGs

        let hWK = hashPosition b (mkGs Color.White WK ValueNone)
        Assert.That(hWK, Is.EqualTo(toggleCastleWK h0))

        let hWQ = hashPosition b (mkGs Color.White WQ ValueNone)
        Assert.That(hWQ, Is.EqualTo(toggleCastleWQ h0))

        let hBK = hashPosition b (mkGs Color.White BK ValueNone)
        Assert.That(hBK, Is.EqualTo(toggleCastleBK h0))

        let hBQ = hashPosition b (mkGs Color.White BQ ValueNone)
        Assert.That(hBQ, Is.EqualTo(toggleCastleBQ h0))

        let all = WK ||| WQ ||| BK ||| BQ
        let hAll = hashPosition b (mkGs Color.White all ValueNone)
        let expected = h0 |> toggleCastleWK |> toggleCastleWQ |> toggleCastleBK |> toggleCastleBQ
        Assert.That(hAll, Is.EqualTo(expected))

    [<Test>]
    member _.``en passant file changes hash by toggleEpFile`` () =
        let b = mkBoard()
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)

        let h0 = hashPosition b (mkGs Color.White 0uy ValueNone)

        // EP file is hashed by 0-based file 0..7 (a..h).
        // Rank is ignored by hashing (but still stored in Coordinates).
        // Example: d6 => file=3 rank=5 (0-based).
        let ep = ValueSome { File = 3uy; Rank = 5uy }
        let h1 = hashPosition b (mkGs Color.White 0uy ep)

        Assert.That(h1, Is.EqualTo(toggleEpFile 3 h0))

    [<Test>]
    member _.``full hash equals manual XOR construction`` () =
        let b = mkBoard()
        // e1 (4,0), e8 (4,7), d2 (3,1)
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)
        Board.set b 3 1 (make Color.White Pawn)

        let rights = WK ||| BK
        // Example EP square: e6 => file=4 rank=5 (0-based)
        let ep = ValueSome { File = 4uy; Rank = 5uy }
        let gs = mkGs Color.Black rights ep

        let hFull = hashPosition b gs

        // Manually build expected based on the same entities hashPosition includes.
        let mutable h = 0L

        // Pieces (togglePiece takes 0-based file/rank)
        h <- togglePiece (make Color.White King) 4 0 h
        h <- togglePiece (make Color.Black King) 4 7 h
        h <- togglePiece (make Color.White Pawn) 3 1 h

        // Castling rights
        h <- toggleCastleWK h
        h <- toggleCastleBK h

        // EP file
        h <- toggleEpFile 4 h

        // Side to move is black
        h <- toggleBlackToMove h

        Assert.That(hFull, Is.EqualTo(h))

    [<Test>]
    member _.``moving a piece changes hash by XORing from/to piece keys`` () =
        let b = mkBoard()
        Board.set b 4 0 (make Color.White King)
        Board.set b 4 7 (make Color.Black King)

        // Knight on b2 => (1,1)
        Board.set b 1 1 (make Color.White Knight)

        let gs = mkGs Color.White 0uy ValueNone
        let h0 = hashPosition b gs

        // Move knight b2 -> c4: (1,1) -> (2,3)
        Board.set b 1 1 Empty
        Board.set b 2 3 (make Color.White Knight)

        let h1 = hashPosition b gs

        // Incremental expectation: XOR out from-square, XOR in to-square
        let hExpected =
            h0
            |> togglePiece (make Color.White Knight) 1 1
            |> togglePiece (make Color.White Knight) 2 3

        Assert.That(h1, Is.EqualTo(hExpected))
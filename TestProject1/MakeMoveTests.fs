module MakeMoveTests

open NUnit.Framework

open Types
open BoardHelpers
open Board
open PieceCode
open CastlingRights
open Zobrist
open MakeMove

// Board is 8x8 (NOT padded).
// Coordinates are 0-based: File/Rank = 0..7
// Access board via getC/setC consistently.

[<TestFixture>]
type ``MakeMove tests`` () =

    // -------- helpers --------

    let mkBoard () : Board =
        Array2D.create 8 8 0y

    let sq (f:int) (r:int) : Coordinates =
        { File = byte f; Rank = byte r }

    let cloneBoard (b:Board) : Board =
        b.Clone() :?> Board

    let boardsEqual (a:Board) (b:Board) =
        Assert.That(a.GetLength(0), Is.EqualTo(b.GetLength(0)))
        Assert.That(a.GetLength(1), Is.EqualTo(b.GetLength(1)))
        for file = 0 to a.GetLength(0) - 1 do
            for rank = 0 to a.GetLength(1) - 1 do
                Assert.That(a.[file, rank], Is.EqualTo(b.[file, rank]), $"Mismatch at [{file},{rank}]")

    let mkGs (toPlay:Color) (rights:byte) (ep:ValueOption<Coordinates>) : GameState =
        { HashKey = 0L
          EPSquare = ep
          FullMoveNumber = 1us
          CastlingRights = rights
          HalfMoveClock = 0uy
          ToPlay = toPlay }

    // Edit if your Kings record differs.
    let mkKings (wk:Coordinates) (bk:Coordinates) =
        { WhiteKingSq = wk; BlackKingSq = bk }

    // Edit if your Position record differs.
    let mkPos (b:Board) (gs:GameState) (wk:Coordinates) (bk:Coordinates) : Position =
        // hashPosition reads board[0..7,0..7] and gs fields (including EP file, rights, stm)
        let h = hashPosition b gs
        { Board = b
          State = { gs with HashKey = h }
          Kings = mkKings wk bk }

    let assertHashConsistent (pos:Position) =
        // IMPORTANT: recompute using state EXCEPT HashKey, so make sure your hashPosition ignores gs.HashKey
        let h = hashPosition pos.Board pos.State
        Assert.That(pos.State.HashKey, Is.EqualTo(h), "Incremental HashKey != full recomputed hashPosition")

    let mkMove (board:Board) (fromSq:Coordinates) (toSq:Coordinates) (promoteTo:sbyte) : Move =
        let piece = getC board fromSq
        Assert.That(piece, Is.Not.EqualTo(Empty), "From-square is empty in test setup")
        { From = fromSq
          To = toSq
          Piece = piece
          PromoteTo = promoteTo }

    // -------- tests --------

    [<Test>]
    member _.``makeMove then unmakeMove restores board and state and hash`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)    // e1
        setC b (sq 4 7) (make Color.Black King)    // e8
        setC b (sq 1 1) (make Color.White Knight)  // b2

        let gs = mkGs Color.White 0uy ValueNone
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)

        let bBefore = cloneBoard pos0.Board
        let stBefore = pos0.State
        let kingsBefore = pos0.Kings

        let mv = mkMove pos0.Board (sq 1 1) (sq 2 3) 0y  // Nb2-c4

        let mutable pos = pos0
        let undo = makeMove &pos mv

        assertHashConsistent pos

        unmakeMove &pos mv undo

        boardsEqual pos.Board bBefore
        Assert.That(pos.State, Is.EqualTo(stBefore))
        Assert.That(pos.Kings, Is.EqualTo(kingsBefore))
        assertHashConsistent pos

    [<Test>]
    member _.``normal capture updates hash and unmake restores`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)
        setC b (sq 4 7) (make Color.Black King)
        setC b (sq 3 3) (make Color.White Knight)   // d4
        setC b (sq 4 5) (make Color.Black Pawn)     // e6

        let gs = mkGs Color.White 0uy ValueNone
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)
        let bBefore = cloneBoard pos0.Board
        let stBefore = pos0.State

        let mv = mkMove pos0.Board (sq 3 3) (sq 4 5) 0y

        let mutable pos = pos0
        let undo = makeMove &pos mv

        Assert.That(undo.CapturedPiece, Is.EqualTo(make Color.Black Pawn))
        assertHashConsistent pos

        unmakeMove &pos mv undo

        boardsEqual pos.Board bBefore
        Assert.That(pos.State, Is.EqualTo(stBefore))
        assertHashConsistent pos

    [<Test>]
    member _.``castling moves rook, updates castling rights, hash consistent, unmake restores`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)  // e1
        setC b (sq 7 0) (make Color.White Rook)  // h1
        setC b (sq 4 7) (make Color.Black King)  // e8

        let rights = WK ||| WQ
        let gs = mkGs Color.White rights ValueNone
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)

        let bBefore = cloneBoard pos0.Board
        let stBefore = pos0.State

        let mv = mkMove pos0.Board (sq 4 0) (sq 6 0) 0y // O-O: e1->g1

        let mutable pos = pos0
        let undo = makeMove &pos mv

        // After O-O: king g1 (6,0), rook f1 (5,0)
        Assert.That(getC pos.Board (sq 6 0), Is.EqualTo(make Color.White King))
        Assert.That(getC pos.Board (sq 5 0), Is.EqualTo(make Color.White Rook))
        Assert.That(getC pos.Board (sq 4 0), Is.EqualTo(Empty))
        Assert.That(getC pos.Board (sq 7 0), Is.EqualTo(Empty))

        Assert.That(pos.State.CastlingRights &&& (WK ||| WQ), Is.EqualTo(0uy))
        assertHashConsistent pos

        unmakeMove &pos mv undo

        boardsEqual pos.Board bBefore
        Assert.That(pos.State, Is.EqualTo(stBefore))
        assertHashConsistent pos

    [<Test>]
    member _.``en passant capture removes pawn from cap square, hash consistent, unmake restores`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)
        setC b (sq 4 7) (make Color.Black King)

        setC b (sq 4 4) (make Color.White Pawn)  // e5
        setC b (sq 3 4) (make Color.Black Pawn)  // d5

        // EP target square d6 => (3,5)
        let ep = ValueSome (sq 3 5)
        let gs = mkGs Color.White 0uy ep
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)

        let bBefore = cloneBoard pos0.Board
        let stBefore = pos0.State

        // e5xd6 ep: (4,4)->(3,5)
        let mv = mkMove pos0.Board (sq 4 4) (sq 3 5) 0y

        let mutable pos = pos0
        let undo = makeMove &pos mv

        Assert.That(getC pos.Board (sq 3 5), Is.EqualTo(make Color.White Pawn))
        Assert.That(getC pos.Board (sq 3 4), Is.EqualTo(Empty)) // captured pawn removed
        Assert.That(getC pos.Board (sq 4 4), Is.EqualTo(Empty))
        Assert.That(undo.CapturedPiece, Is.EqualTo(make Color.Black Pawn))

        assertHashConsistent pos

        unmakeMove &pos mv undo

        boardsEqual pos.Board bBefore
        Assert.That(pos.State, Is.EqualTo(stBefore))
        assertHashConsistent pos

    [<Test>]
    member _.``promotion changes piece on destination, hash consistent, unmake restores pawn`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)
        setC b (sq 4 7) (make Color.Black King)

        setC b (sq 0 6) (make Color.White Pawn)  // a7

        let gs = mkGs Color.White 0uy ValueNone
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)

        let bBefore = cloneBoard pos0.Board
        let stBefore = pos0.State

        let mv = mkMove pos0.Board (sq 0 6) (sq 0 7) (make Color.White Queen)

        let mutable pos = pos0
        let undo = makeMove &pos mv

        Assert.That(getC pos.Board (sq 0 6), Is.EqualTo(Empty))
        Assert.That(getC pos.Board (sq 0 7), Is.EqualTo(make Color.White Queen))
        assertHashConsistent pos

        unmakeMove &pos mv undo

        boardsEqual pos.Board bBefore
        Assert.That(pos.State, Is.EqualTo(stBefore))
        assertHashConsistent pos

    [<Test>]
    member _.``double pawn push sets EP square and hash consistent`` () =
        let b = mkBoard()
        setC b (sq 4 0) (make Color.White King)
        setC b (sq 4 7) (make Color.Black King)

        setC b (sq 4 1) (make Color.White Pawn) // e2

        let gs = mkGs Color.White 0uy ValueNone
        let pos0 = mkPos b gs (sq 4 0) (sq 4 7)

        // e2->e4: (4,1)->(4,3)
        let mv = mkMove pos0.Board (sq 4 1) (sq 4 3) 0y

        let mutable pos = pos0
        let _undo = makeMove &pos mv

        // EP square should be the passed-over square e3 => (4,2)
        match pos.State.EPSquare with
        | ValueSome epSq ->
            Assert.That(epSq.File, Is.EqualTo(4uy))
            Assert.That(epSq.Rank, Is.EqualTo(2uy))
        | ValueNone ->
            Assert.Fail("Expected EPSquare to be set after double pawn push")

        assertHashConsistent pos
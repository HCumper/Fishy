module Board1DTestHelpers 

    open NUnit.Framework
    open Types
    open BoardHelpers.PieceCode

    // Board = sbyte[]
    let inline idx (file:int) (rank:int) = file + (rank * 8)

    let mkBoard () : Board =
        Array.zeroCreate<sbyte> 64

    let cloneBoard (b:Board) : Board =
        Array.copy b

    let boardsEqual (a:Board) (b:Board) =
        Assert.That(a.Length, Is.EqualTo(b.Length))
        for i = 0 to a.Length - 1 do
            Assert.That(a[i], Is.EqualTo(b[i]), $"Mismatch at idx {i}")

    let inline getFR (b:Board) (file:int) (rank:int) : sbyte =
        b[idx file rank]

    let inline setFR (b:Board) (file:int) (rank:int) (p:sbyte) : unit =
        b[idx file rank] <- p

    let inline getC (b:Board) (sq:Coordinates) : sbyte =
        b[int sq.File + 8 * int sq.Rank]

    let inline setC (b:Board) (sq:Coordinates) (p:sbyte) : unit =
        b[int sq.File + 8 * int sq.Rank] <- p

    let sq (f:int) (r:int) : Coordinates =
        { File = byte f; Rank = byte r }

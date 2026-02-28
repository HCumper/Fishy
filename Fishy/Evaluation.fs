module Evaluation

open BoardHelpers
open Types
open PieceCode

let pawnPlacementTable =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 50; 50; 50; 50; 50; 50; 50; 50 |]
           [| 10; 10; 20; 30; 30; 20; 10; 10 |]
           [| 5; 5; 10; 25; 25; 10; 5; 5 |]
           [| 0; 0; 0; 20; 20; 0; 0; 0 |]
           [| 5; -5; -10; 0; 0; -10; -5; 5 |]
           [| 5; 10; 10; -20; -20; 10; 10; 5 |]
           [| 0; 0; 0; 0; 0; 0; 0; 0 |] |]

let knightPlacementTable =
    array2D
        [| [| -50; -40; -30; -30; -30; -30; -40; -50 |]
           [| -40; -20; 0; 0; 0; 0; -20; -40 |]
           [| -30; 0; 10; 15; 15; 10; 0; -30 |]
           [| -30; 5; 15; 20; 20; 15; 5; -30 |]
           [| -30; 0; 15; 20; 20; 15; 0; -30 |]
           [| -30; 5; 10; 15; 15; 10; 5; -30 |]
           [| -40; -20; 0; 5; 5; 0; -20; -40 |]
           [| -50; -40; -30; -30; -30; -30; -40; -50 |] |]

let bishopPlacementTable =
    array2D
        [| [| -20; -10; -10; -10; -10; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 10; 10; 5; 0; -10 |]
           [| -10; 5; 5; 10; 10; 5; 5; -10 |]
           [| -10; 0; 10; 10; 10; 10; 0; -10 |]
           [| -10; 10; 10; 10; 10; 10; 10; -10 |]
           [| -10; 5; 0; 0; 0; 0; 5; -10 |]
           [| -20; -10; -10; -10; -10; -10; -10; -20 |] |]

let rookPlacementTable =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 5; 10; 10; 10; 10; 10; 10; 5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| 0; 0; 0; 5; 5; 0; 0; 0 |] |]

let queenPlacementTable =
    array2D
        [| [| -20; -10; -10; -5; -5; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 5; 5; 5; 0; -10 |]
           [| -5; 0; 5; 5; 5; 5; 0; -5 |]
           [| 0; 0; 5; 5; 5; 5; 0; -5 |]
           [| -10; 5; 5; 5; 5; 5; 0; -10 |]
           [| -10; 0; 5; 0; 0; 0; 0; -10 |]
           [| -20; -10; -10; -5; -5; -10; -10; -20 |] |]

let kingMiddlegamePlacementTable =
    array2D
        [| [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -20; -30; -30; -40; -40; -30; -30; -20 |]
           [| -10; -20; -20; -20; -20; -20; -20; -10 |]
           [| 20; 20; 0; 0; 0; 0; 20; 20 |]
           [| 20; 30; 10; 0; 0; 10; 30; 20 |] |]

let kingEndgamePlacementTable =
    array2D
        [| [| -50; -40; -30; -20; -20; -30; -40; -50 |]
           [| -30; -20; -10; 0; 0; -10; -20; -30 |]
           [| -30; -10; 20; 30; 30; 20; -10; -30 |]
           [| -30; -10; 30; 40; 40; 30; -10; -30 |]
           [| -30; -10; 30; 40; 40; 30; -10; -30 |]
           [| -30; -10; 20; 30; 30; 20; -10; -30 |]
           [| -30; -30; 0; 0; 0; 0; -30; -30 |]
           [| -50; -30; -30; -30; -30; -30; -30; -50 |] |]

// -----------------------------
// Material in centipawns
// -----------------------------
[<Literal>]
let PawnV   = 100
[<Literal>]
let KnightV = 320
[<Literal>]
let BishopV = 330
[<Literal>]
let RookV   = 500
[<Literal>]
let QueenV  = 900
[<Literal>]
let KingV   = 0

// PSTs are assumed already defined as int[,] in cp scale
let pawnPst   : int[,] = pawnPlacementTable
let knightPst : int[,] = knightPlacementTable
let bishopPst : int[,] = bishopPlacementTable
let rookPst   : int[,] = rookPlacementTable
let queenPst  : int[,] = queenPlacementTable
let kingMgPst : int[,] = kingMiddlegamePlacementTable
let kingEgPst : int[,] = kingEndgamePlacementTable  // unused for now

// -----------------------------
// Helpers
// -----------------------------

let inline private pieceValue (kind:sbyte) : int =
    match kind with
    | 1y -> PawnV
    | 2y -> KnightV
    | 3y -> BishopV
    | 4y -> RookV
    | 5y -> QueenV
    | 6y -> KingV
    | _  -> 0

let inline private pstValue (kind:sbyte) (pstRow:int) (file0:int) : int =
    match kind with
    | 1y -> pawnPst.[pstRow, file0]
    | 2y -> knightPst.[pstRow, file0]
    | 3y -> bishopPst.[pstRow, file0]
    | 4y -> rookPst.[pstRow, file0]
    | 5y -> queenPst.[pstRow, file0]
    | 6y -> kingMgPst.[pstRow, file0]
    | _  -> 0

// board rank0: 0=rank1 .. 7=rank8
// pst row:     0=rank8 .. 7=rank1
let inline private pstRowForWhite (rank0:int) = 7 - rank0
let inline private pstRowForBlack (rank0:int) = rank0

/// Evaluate from SIDE-TO-MOVE viewpoint.
/// + means good for pos.State.ToPlay.
let evaluate (pos: Position) : int =
    let b = pos.Board
    let mutable whiteScore = 0

    for rank0 = 0 to 7 do
        for file0 = 0 to 7 do
            let p = b.[file0, rank0]
            if p <> Empty then
                let k = absKind p
                let mat = pieceValue k

                if isWhite p then
                    let pstRow = pstRowForWhite rank0
                    whiteScore <- whiteScore + mat + pstValue k pstRow file0
                else
                    let pstRow = pstRowForBlack rank0
                    whiteScore <- whiteScore - (mat + pstValue k pstRow file0)

    // Negamax-ready
    if pos.State.ToPlay = Color.White then whiteScore else -whiteScore
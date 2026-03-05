module Evaluation

open Types
open BoardHelpers.PieceCode

// Assumptions (match your codebase):
// - Board is 1D: sbyte[] length 64, indexed rank*8 + file (rank 0 = rank1).
// - Empty = 0y
// - absKind p returns 1y..6y for Pawn..King
// - isWhite p / isBlack p based on sign
//
// This module builds a fast "piece-square value" table:
//   pieceSqv[pieceIndex].[sq] = material(piece) + PST(piece, sq)
// with black already negated, so evaluation is one add per piece.

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

// -----------------------------
// PSTs in your original 2D layout (pstRow 0 = rank8 .. 7 = rank1)
// Keep these for readability, but we will convert once into 1D.
// -----------------------------

let pawnPst2D : int[,] =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 50; 50; 50; 50; 50; 50; 50; 50 |]
           [| 10; 10; 20; 30; 30; 20; 10; 10 |]
           [| 5; 5; 10; 25; 25; 10; 5; 5 |]
           [| 0; 0; 0; 20; 20; 0; 0; 0 |]
           [| 5; -5; -10; 0; 0; -10; -5; 5 |]
           [| 5; 10; 10; -20; -20; 10; 10; 5 |]
           [| 0; 0; 0; 0; 0; 0; 0; 0 |] |]

let knightPst2D : int[,] =
    array2D
        [| [| -50; -40; -30; -30; -30; -30; -40; -50 |]
           [| -40; -20; 0; 0; 0; 0; -20; -40 |]
           [| -30; 0; 10; 15; 15; 10; 0; -30 |]
           [| -30; 5; 15; 20; 20; 15; 5; -30 |]
           [| -30; 0; 15; 20; 20; 15; 0; -30 |]
           [| -30; 5; 10; 15; 15; 10; 5; -30 |]
           [| -40; -20; 0; 5; 5; 0; -20; -40 |]
           [| -50; -40; -30; -30; -30; -30; -40; -50 |] |]

let bishopPst2D : int[,] =
    array2D
        [| [| -20; -10; -10; -10; -10; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 10; 10; 5; 0; -10 |]
           [| -10; 5; 5; 10; 10; 5; 5; -10 |]
           [| -10; 0; 10; 10; 10; 10; 0; -10 |]
           [| -10; 10; 10; 10; 10; 10; 10; -10 |]
           [| -10; 5; 0; 0; 0; 0; 5; -10 |]
           [| -20; -10; -10; -10; -10; -10; -10; -20 |] |]

let rookPst2D : int[,] =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 5; 10; 10; 10; 10; 10; 10; 5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| 0; 0; 0; 5; 5; 0; 0; 0 |] |]

let queenPst2D : int[,] =
    array2D
        [| [| -20; -10; -10; -5; -5; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 5; 5; 5; 0; -10 |]
           [| -5; 0; 5; 5; 5; 5; 0; -5 |]
           [| 0; 0; 5; 5; 5; 5; 0; -5 |]
           [| -10; 5; 5; 5; 5; 5; 0; -10 |]
           [| -10; 0; 5; 0; 0; 0; 0; -10 |]
           [| -20; -10; -10; -5; -5; -10; -10; -20 |] |]

let kingMgPst2D : int[,] =
    array2D
        [| [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -20; -30; -30; -40; -40; -30; -30; -20 |]
           [| -10; -20; -20; -20; -20; -20; -20; -10 |]
           [| 20; 20; 0; 0; 0; 0; 20; 20 |]
           [| 20; 30; 10; 0; 0; 10; 30; 20 |] |]

// -----------------------------
// 1D helpers
// -----------------------------

let inline private sqIndex (file0:int) (rank0:int) : int =
    // rank0: 0..7 (rank1..rank8)
    (rank0 <<< 3) + file0

let inline private mirrorRank (sq:int) : int =
    // flips rank for converting white PST to black orientation on a 1D board
    // (file stays the same)
    let file0 = sq &&& 7
    let rank0 = sq >>> 3
    let rankM = 7 - rank0
    (rankM <<< 3) + file0

let private pst2D_toWhite1D (pst:int[,]) : int[] =
    // pstRow: 0=rank8 .. 7=rank1
    // board rank0: 0=rank1 .. 7=rank8
    // For a given board (file0,rank0), the corresponding pstRow is (7-rank0).
    let a = Array.zeroCreate<int> 64
    for rank0 = 0 to 7 do
        let pstRow = 7 - rank0
        for file0 = 0 to 7 do
            a.[sqIndex file0 rank0] <- pst.[pstRow, file0]
    a

// Convert 2D PSTs to 1D (white orientation)
let pawnPstW   = pst2D_toWhite1D pawnPst2D
let knightPstW = pst2D_toWhite1D knightPst2D
let bishopPstW = pst2D_toWhite1D bishopPst2D
let rookPstW   = pst2D_toWhite1D rookPst2D
let queenPstW  = pst2D_toWhite1D queenPst2D
let kingMgPstW = pst2D_toWhite1D kingMgPst2D

// -----------------------------
// Piece indexing
// -----------------------------
//
// Map your piece code (sbyte) to an index 0..11:
// 0..5  = White Pawn..King
// 6..11 = Black Pawn..King
//
// absKind p is sbyte 1..6. Convert once to int.

let inline private pieceIndex (p:sbyte) : int =
    // assumes non-empty
    let k = int (absKind p) - 1  // 0..5
    if p > 0y then k else 6 + k

let inline private materialOfKind (k0:int) : int =
    // k0: 0..5 => Pawn..King
    match k0 with
    | 0 -> PawnV
    | 1 -> KnightV
    | 2 -> BishopV
    | 3 -> RookV
    | 4 -> QueenV
    | 5 -> KingV
    | _ -> 0

let inline private pstW_ofKind (k0:int) : int[] =
    match k0 with
    | 0 -> pawnPstW
    | 1 -> knightPstW
    | 2 -> bishopPstW
    | 3 -> rookPstW
    | 4 -> queenPstW
    | 5 -> kingMgPstW
    | _ -> pawnPstW

// -----------------------------
// Precomputed piece-square values
// -----------------------------
//
// pieceSqv[pieceIndex].[sq] is:
//   + (material + PST) for white pieces
//   - (material + PST mirrored) for black pieces
//
// After this, evaluation is just a sum over pieces.

let pieceSqv : int[][] =
    let t = Array.init 12 (fun _ -> Array.zeroCreate<int> 64)
    for k0 = 0 to 5 do
        let mat = materialOfKind k0
        let pstW = pstW_ofKind k0
        // White piece index
        let wi = k0
        // Black piece index
        let bi = 6 + k0
        for sq = 0 to 63 do
            let vW = mat + pstW.[sq]
            t.[wi].[sq] <- vW
            // black uses mirrored square for same PST shape, and is negated in white-eval
            let sqM = mirrorRank sq
            let vB = mat + pstW.[sqM]
            t.[bi].[sq] <- -vB
    t

// -----------------------------
// Evaluation
// -----------------------------
//
// Returns score from side-to-move viewpoint (negamax-ready):
// + means good for pos.State.ToPlay.

let evaluate (pos: Position) : int =
    let b = pos.Board
    let mutable whiteView = 0

    // 1D board: 64 squares
    for sq = 0 to 63 do
        let p = b.[sq]
        if p <> Empty then
            whiteView <- whiteView + pieceSqv.[pieceIndex p].[sq]

    if pos.State.ToPlay = Color.White then whiteView else -whiteView
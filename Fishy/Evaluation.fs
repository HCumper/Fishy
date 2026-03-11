module Evaluation

open Types
open BoardHelpers.PieceCode

// Tapered eval:
//   score = (mg * phase + eg * (MaxPhase - phase)) / MaxPhase
// where phase depends on remaining non-pawn material.
//
// Mobility (Option A): position-only pseudo-mobility, NOT legal movegen,
// and NOT dependent on pos.State.ToPlay. Pawn mobility = captures-only.

[<Literal>]
let PawnMg   = 100
[<Literal>]
let KnightMg = 320
[<Literal>]
let BishopMg = 330
[<Literal>]
let RookMg   = 500
[<Literal>]
let QueenMg  = 900
[<Literal>]
let KingMg   = 0

[<Literal>]
let PawnEg   = 100
[<Literal>]
let KnightEg = 310
[<Literal>]
let BishopEg = 340
[<Literal>]
let RookEg   = 500
[<Literal>]
let QueenEg  = 900
[<Literal>]
let KingEg   = 0

[<Literal>]
let IsolatedPawnMgPenalty = 12
[<Literal>]
let IsolatedPawnEgPenalty = 8

[<Literal>]
let DoubledPawnMgPenalty = 10
[<Literal>]
let DoubledPawnEgPenalty = 14

// rank-based passed pawn bonus by side-relative advancement:
let passedPawnMgBonus : int[] = [| 0; 5; 10; 20; 35; 60; 100; 0 |]
let passedPawnEgBonus : int[] = [| 0; 10; 20; 40; 70; 120; 200; 0 |]

// -----------------------------
// PSTs (2D: row 0 = rank8 .. row 7 = rank1)
// -----------------------------

let pawnPstMg2D : int[,] =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 50; 50; 50; 50; 50; 50; 50; 50 |]
           [| 10; 10; 20; 30; 30; 20; 10; 10 |]
           [| 5; 5; 10; 25; 25; 10; 5; 5 |]
           [| 0; 0; 0; 20; 20; 0; 0; 0 |]
           [| 5; -5; -10; 0; 0; -10; -5; 5 |]
           [| 5; 10; 10; -20; -20; 10; 10; 5 |]
           [| 0; 0; 0; 0; 0; 0; 0; 0 |] |]

let knightPstMg2D : int[,] =
    array2D
        [| [| -50; -40; -30; -30; -30; -30; -40; -50 |]
           [| -40; -20; 0; 0; 0; 0; -20; -40 |]
           [| -30; 0; 10; 15; 15; 10; 0; -30 |]
           [| -30; 5; 15; 20; 20; 15; 5; -30 |]
           [| -30; 0; 15; 20; 20; 15; 0; -30 |]
           [| -30; 5; 10; 15; 15; 10; 5; -30 |]
           [| -40; -20; 0; 5; 5; 0; -20; -40 |]
           [| -50; -40; -30; -30; -30; -30; -40; -50 |] |]

let bishopPstMg2D : int[,] =
    array2D
        [| [| -20; -10; -10; -10; -10; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 10; 10; 5; 0; -10 |]
           [| -10; 5; 5; 10; 10; 5; 5; -10 |]
           [| -10; 0; 10; 10; 10; 10; 0; -10 |]
           [| -10; 10; 10; 10; 10; 10; 10; -10 |]
           [| -10; 5; 0; 0; 0; 0; 5; -10 |]
           [| -20; -10; -10; -10; -10; -10; -10; -20 |] |]

let rookPstMg2D : int[,] =
    array2D
        [| [| 0; 0; 0; 0; 0; 0; 0; 0 |]
           [| 5; 10; 10; 10; 10; 10; 10; 5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| -5; 0; 0; 0; 0; 0; 0; -5 |]
           [| 0; 0; 0; 5; 5; 0; 0; 0 |] |]

let queenPstMg2D : int[,] =
    array2D
        [| [| -20; -10; -10; -5; -5; -10; -10; -20 |]
           [| -10; 0; 0; 0; 0; 0; 0; -10 |]
           [| -10; 0; 5; 5; 5; 5; 0; -10 |]
           [| -5; 0; 5; 5; 5; 5; 0; -5 |]
           [| 0; 0; 5; 5; 5; 5; 0; -5 |]
           [| -10; 5; 5; 5; 5; 5; 0; -10 |]
           [| -10; 0; 5; 0; 0; 0; 0; -10 |]
           [| -20; -10; -10; -5; -5; -10; -10; -20 |] |]

let kingPstMg2D : int[,] =
    array2D
        [| [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -30; -40; -40; -50; -50; -40; -40; -30 |]
           [| -20; -30; -30; -40; -40; -30; -30; -20 |]
           [| -10; -20; -20; -20; -20; -20; -20; -10 |]
           [| 20; 20; 0; 0; 0; 0; 20; 20 |]
           [| 20; 30; 10; 0; 0; 10; 30; 20 |] |]

let kingPstEg2D : int[,] =
    array2D
        [| [| -50; -40; -30; -20; -20; -30; -40; -50 |]
           [| -30; -20; -10;   0;   0; -10; -20; -30 |]
           [| -30; -10;  20;  30;  30;  20; -10; -30 |]
           [| -30; -10;  30;  40;  40;  30; -10; -30 |]
           [| -30; -10;  30;  40;  40;  30; -10; -30 |]
           [| -30; -10;  20;  30;  30;  20; -10; -30 |]
           [| -30; -20; -10;   0;   0; -10; -20; -30 |]
           [| -50; -40; -30; -20; -20; -30; -40; -50 |] |]

let pawnPstEg2D : int[,] =
    array2D
        [| [|   0;   0;   0;   0;   0;   0;   0;   0 |]
           [| 178; 173; 158; 134; 134; 158; 173; 178 |]
           [|  94;  88;  82;  76;  76;  82;  88;  94 |]
           [|  32;  24;  13;   5;   5;  13;  24;  32 |]
           [|  13;   8;  -5; -10; -10;  -5;   8;  13 |]
           [|   4;  -2; -14; -20; -20; -14;  -2;   4 |]
           [|  13;  10;   3; -10; -10;   3;  10;  13 |]
           [|   0;   0;   0;   0;   0;   0;   0;   0 |] |]

let knightPstEg2D : int[,] =
    array2D
        [| [| -80; -40; -20; -20; -20; -20; -40; -80 |]
           [| -40; -20;   0;   5;   5;   0; -20; -40 |]
           [| -20;   0;  15;  20;  20;  15;   0; -20 |]
           [| -20;   5;  20;  30;  30;  20;   5; -20 |]
           [| -20;   5;  20;  30;  30;  20;   5; -20 |]
           [| -20;   0;  15;  20;  20;  15;   0; -20 |]
           [| -40; -20;   0;   5;   5;   0; -20; -40 |]
           [| -80; -40; -20; -20; -20; -20; -40; -80 |] |]

let bishopPstEg2D : int[,] =
    array2D
        [| [| -20; -15; -10;  -5;  -5; -10; -15; -20 |]
           [| -15;  -5;   5;  10;  10;   5;  -5; -15 |]
           [| -10;   5;  15;  20;  20;  15;   5; -10 |]
           [|  -5;  10;  20;  25;  25;  20;  10;  -5 |]
           [|  -5;  10;  20;  25;  25;  20;  10;  -5 |]
           [| -10;   5;  15;  20;  20;  15;   5; -10 |]
           [| -15;  -5;   5;  10;  10;   5;  -5; -15 |]
           [| -20; -15; -10;  -5;  -5; -10; -15; -20 |] |]

let rookPstEg2D : int[,] =
    array2D
        [| [|  0;   5;  10;  10;  10;  10;   5;   0 |]
           [| 20;  20;  20;  20;  20;  20;  20;  20 |]
           [| 10;  10;  10;  10;  10;  10;  10;  10 |]
           [|  5;   5;   5;   5;   5;   5;   5;   5 |]
           [|  0;   0;   0;   0;   0;   0;   0;   0 |]
           [| -5;  -5;  -5;  -5;  -5;  -5;  -5;  -5 |]
           [| -5;  -5;  -5;  -5;  -5;  -5;  -5;  -5 |]
           [| -10; -10; -10; -10; -10; -10; -10; -10 |] |]

let queenPstEg2D : int[,] =
    array2D
        [| [| -20; -10;  -5;   0;   0;  -5; -10; -20 |]
           [| -10;   0;   5;  10;  10;   5;   0; -10 |]
           [|  -5;   5;  10;  15;  15;  10;   5;  -5 |]
           [|   0;  10;  15;  20;  20;  15;  10;   0 |]
           [|   0;  10;  15;  20;  20;  15;  10;   0 |]
           [|  -5;   5;  10;  15;  15;  10;   5;  -5 |]
           [| -10;   0;   5;  10;  10;   5;   0; -10 |]
           [| -20; -10;  -5;   0;   0;  -5; -10; -20 |] |]

// -----------------------------
// 1D helpers
// -----------------------------

let inline private sqIndex (file0:int) (rank0:int) : int =
    (rank0 <<< 3) + file0

let inline private mirrorRank (sq:int) : int =
    let file0 = sq &&& 7
    let rank0 = sq >>> 3
    let rankM = 7 - rank0
    (rankM <<< 3) + file0

let private pst2D_toWhite1D (pst:int[,]) : int[] =
    // pstRow: 0=rank8 .. 7=rank1
    // board rank0: 0=rank1 .. 7=rank8
    let a = Array.zeroCreate<int> 64
    for rank0 = 0 to 7 do
        let pstRow = 7 - rank0
        for file0 = 0 to 7 do
            a.[sqIndex file0 rank0] <- pst.[pstRow, file0]
    a

// Convert PSTs once (avoid rebuilding arrays repeatedly)
let pawnPstMgW   = pst2D_toWhite1D pawnPstMg2D
let knightPstMgW = pst2D_toWhite1D knightPstMg2D
let bishopPstMgW = pst2D_toWhite1D bishopPstMg2D
let rookPstMgW   = pst2D_toWhite1D rookPstMg2D
let queenPstMgW  = pst2D_toWhite1D queenPstMg2D
let kingPstMgW   = pst2D_toWhite1D kingPstMg2D

let pawnPstEgW   = pst2D_toWhite1D pawnPstEg2D
let knightPstEgW = pst2D_toWhite1D knightPstEg2D
let bishopPstEgW = pst2D_toWhite1D bishopPstEg2D
let rookPstEgW   = pst2D_toWhite1D rookPstEg2D
let queenPstEgW  = pst2D_toWhite1D queenPstEg2D
let kingPstEgW   = pst2D_toWhite1D kingPstEg2D

let inline private fileOf (sq:int) = sq &&& 7
let inline private rankOf (sq:int) = sq >>> 3
let inline private sqOf (file0:int) (rank0:int) = (rank0 <<< 3) + file0
let inline private inBounds (file0:int) (rank0:int) =
    file0 >= 0 && file0 <= 7 && rank0 >= 0 && rank0 <= 7

// -----------------------------
// Piece indexing
// -----------------------------

let inline private kind0 (p:sbyte) : int =
    int (absKind p) - 1  // 0..5

let inline private pieceIndex (p:sbyte) : int =
    let k = kind0 p
    if p > 0y then k else 6 + k

// -----------------------------
// Material by kind (MG/EG)
// -----------------------------

let inline private materialMgOfKind (k0:int) : int =
    match k0 with
    | 0 -> PawnMg
    | 1 -> KnightMg
    | 2 -> BishopMg
    | 3 -> RookMg
    | 4 -> QueenMg
    | 5 -> KingMg
    | _ -> 0

let inline private materialEgOfKind (k0:int) : int =
    match k0 with
    | 0 -> PawnEg
    | 1 -> KnightEg
    | 2 -> BishopEg
    | 3 -> RookEg
    | 4 -> QueenEg
    | 5 -> KingEg
    | _ -> 0

let inline private pstMgW_ofKind (k0:int) : int[] =
    match k0 with
    | 0 -> pawnPstMgW
    | 1 -> knightPstMgW
    | 2 -> bishopPstMgW
    | 3 -> rookPstMgW
    | 4 -> queenPstMgW
    | 5 -> kingPstMgW
    | _ -> pawnPstMgW

let inline private pstEgW_ofKind (k0:int) : int[] =
    match k0 with
    | 0 -> pawnPstEgW
    | 1 -> knightPstEgW
    | 2 -> bishopPstEgW
    | 3 -> rookPstEgW
    | 4 -> queenPstEgW
    | 5 -> kingPstEgW
    | _ -> pawnPstEgW

// -----------------------------
// Phase (tapered) constants
// -----------------------------

[<Literal>]
let private PhaseKnight = 1
[<Literal>]
let private PhaseBishop = 1
[<Literal>]
let private PhaseRook   = 2
[<Literal>]
let private PhaseQueen  = 4
[<Literal>]
let private MaxPhase    = 24

let inline private phaseWeightOfKind (k0:int) : int =
    match k0 with
    | 1 -> PhaseKnight
    | 2 -> PhaseBishop
    | 3 -> PhaseRook
    | 4 -> PhaseQueen
    | _ -> 0

// -----------------------------
// Precomputed PSQT (MG and EG)
// -----------------------------

let pieceSqvMg : int[][] =
    let t = Array.init 12 (fun _ -> Array.zeroCreate<int> 64)
    for k0 = 0 to 5 do
        let mat = materialMgOfKind k0
        let pstW = pstMgW_ofKind k0
        let wi = k0
        let bi = 6 + k0
        for sq = 0 to 63 do
            let vW = mat + pstW.[sq]
            t.[wi].[sq] <- vW
            let sqM = mirrorRank sq
            let vB = mat + pstW.[sqM]
            t.[bi].[sq] <- -vB
    t

let pieceSqvEg : int[][] =
    let t = Array.init 12 (fun _ -> Array.zeroCreate<int> 64)
    for k0 = 0 to 5 do
        let mat = materialEgOfKind k0
        let pstW = pstEgW_ofKind k0
        let wi = k0
        let bi = 6 + k0
        for sq = 0 to 63 do
            let vW = mat + pstW.[sq]
            t.[wi].[sq] <- vW
            let sqM = mirrorRank sq
            let vB = mat + pstW.[sqM]
            t.[bi].[sq] <- -vB
    t

// -----------------------------
// Position-only mobility (pseudo), pawn = captures-only
// -----------------------------

let inline private knightMobility (board: Board) (fromSq:int) (p:sbyte) : int =
    let f = fileOf fromSq
    let r = rankOf fromSq
    let inline addIfOk (df:int) (dr:int) (acc:int) =
        let nf = f + df
        let nr = r + dr
        if not (inBounds nf nr) then acc
        else
            let toSq = sqOf nf nr
            let q = board.[toSq]
            if q = Empty || not (sameColor p q) then acc + 1 else acc

    0
    |> addIfOk  1  2 |> addIfOk  2  1 |> addIfOk  2 -1 |> addIfOk  1 -2
    |> addIfOk -1 -2 |> addIfOk -2 -1 |> addIfOk -2  1 |> addIfOk -1  2

let inline private sliderMobility (board: Board) (fromSq:int) (p:sbyte) (dirs:(int*int)[]) : int =
    let f0 = fileOf fromSq
    let r0 = rankOf fromSq
    let mutable total = 0

    for (df, dr) in dirs do
        let mutable f = f0 + df
        let mutable r = r0 + dr
        let mutable stop = false
        while not stop && inBounds f r do
            let toSq = sqOf f r
            let q = board.[toSq]
            if q = Empty then
                total <- total + 1
                f <- f + df
                r <- r + dr
            else
                if not (sameColor p q) then total <- total + 1
                stop <- true

    total

let bishopDirs = [| ( 1, 1); ( 1,-1); (-1, 1); (-1,-1) |]
let rookDirs   = [| ( 1, 0); (-1, 0); ( 0, 1); ( 0,-1) |]
let queenDirs  = [| ( 1, 1); ( 1,-1); (-1, 1); (-1,-1); ( 1, 0); (-1, 0); ( 0, 1); ( 0,-1) |]

let inline private pawnMobility (board: Board) (fromSq:int) (p:sbyte) : int =
    // captures-only, position-only (no ep)
    let f = fileOf fromSq
    let r = rankOf fromSq
    let dir = if p > 0y then 1 else -1
    let rCap = r + dir
    if rCap < 0 || rCap > 7 then 0
    else
        let mutable m = 0
        if f > 0 then
            let q = board.[sqOf (f - 1) rCap]
            if q <> Empty && not (sameColor p q) then m <- m + 1
        if f < 7 then
            let q = board.[sqOf (f + 1) rCap]
            if q <> Empty && not (sameColor p q) then m <- m + 1
        m

let inline private countMobility (pos: Position) : int =
    let board = pos.Board
    let mutable whiteMob = 0
    let mutable blackMob = 0

    for sq = 0 to 63 do
        let p = board.[sq]
        if p <> Empty && not (isKing p) then
            let mob =
                match absKind p with
                | Knight -> knightMobility board sq p
                | Bishop -> sliderMobility board sq p bishopDirs
                | Rook   -> sliderMobility board sq p rookDirs
                | Queen  -> sliderMobility board sq p queenDirs
                | Pawn   -> pawnMobility board sq p
                | _      -> 0

            if p > 0y then whiteMob <- whiteMob + mob
            else blackMob <- blackMob + mob

    // Tune factor. Pseudo mobility counts are larger than legal move counts.
    (whiteMob - blackMob) * 2

let inline private isWhitePawn (p:sbyte) = p = Pawn
let inline private isBlackPawn (p:sbyte) = p = -Pawn

let private buildPawnFileCounts (board: Board) =
    let white = Array.zeroCreate<int> 8
    let black = Array.zeroCreate<int> 8

    for sq = 0 to 63 do
        match board.[sq] with
        | p when isWhitePawn p ->
            white.[fileOf sq] <- white.[fileOf sq] + 1
        | p when isBlackPawn p ->
            black.[fileOf sq] <- black.[fileOf sq] + 1
        | _ ->
            ()

    white, black

let inline private hasFriendlyPawnOnAdjacentFile (file0:int) (fileCounts:int[]) =
    let left  = file0 > 0 && fileCounts.[file0 - 1] > 0
    let right = file0 < 7 && fileCounts.[file0 + 1] > 0
    left || right

let inline private whiteRelativeRank (sq:int) = rankOf sq
let inline private blackRelativeRank (sq:int) = 7 - rankOf sq

let private isWhitePassedPawn (board: Board) (sq:int) =
    let f = fileOf sq
    let r = rankOf sq
    let fMin = max 0 (f - 1)
    let fMax = min 7 (f + 1)

    let mutable passed = true
    let mutable ef = fMin
    while passed && ef <= fMax do
        let mutable er = r + 1
        while passed && er <= 7 do
            if board.[sqOf ef er] = -Pawn then
                passed <- false
            er <- er + 1
        ef <- ef + 1

    passed

let private isBlackPassedPawn (board: Board) (sq:int) =
    let f = fileOf sq
    let r = rankOf sq
    let fMin = max 0 (f - 1)
    let fMax = min 7 (f + 1)

    let mutable passed = true
    let mutable ef = fMin
    while passed && ef <= fMax do
        let mutable er = r - 1
        while passed && er >= 0 do
            if board.[sqOf ef er] = -Pawn then
                passed <- false
            er <- er - 1
        ef <- ef + 1

    passed

/// Returns pawn-structure score from White's viewpoint as MG and EG components.
let private evalPawnStructure (pos: Position) : int * int =
    let board = pos.Board
    let whiteFileCounts, blackFileCounts = buildPawnFileCounts board

    let mutable mg = 0
    let mutable eg = 0

    // isolated + passed
    for sq = 0 to 63 do
        match board.[sq] with
        | p when isWhitePawn p ->
            let f = fileOf sq

            // isolated pawn penalty
            if not (hasFriendlyPawnOnAdjacentFile f whiteFileCounts) then
                mg <- mg - IsolatedPawnMgPenalty
                eg <- eg - IsolatedPawnEgPenalty

            // passed pawn bonus
            if isWhitePassedPawn board sq then
                let rr = whiteRelativeRank sq
                mg <- mg + passedPawnMgBonus.[rr]
                eg <- eg + passedPawnEgBonus.[rr]

        | p when isBlackPawn p ->
            let f = fileOf sq

            // isolated pawn penalty (subtract from White view by adding for Black)
            if not (hasFriendlyPawnOnAdjacentFile f blackFileCounts) then
                mg <- mg + IsolatedPawnMgPenalty
                eg <- eg + IsolatedPawnEgPenalty

            // passed pawn bonus for Black is bad for White
            if isBlackPassedPawn board sq then
                let rr = blackRelativeRank sq
                mg <- mg - passedPawnMgBonus.[rr]
                eg <- eg - passedPawnEgBonus.[rr]

        | _ ->
            ()

    // doubled pawns: apply per extra pawn on each file
    for f = 0 to 7 do
        if whiteFileCounts.[f] > 1 then
            let extras = whiteFileCounts.[f] - 1
            mg <- mg - extras * DoubledPawnMgPenalty
            eg <- eg - extras * DoubledPawnEgPenalty

        if blackFileCounts.[f] > 1 then
            let extras = blackFileCounts.[f] - 1
            mg <- mg + extras * DoubledPawnMgPenalty
            eg <- eg + extras * DoubledPawnEgPenalty

    mg, eg
    
// -----------------------------
// Evaluation (tapered)
// -----------------------------

let evaluate (pos: Position) : int =
    let b = pos.Board
    let mutable mgWhiteView = 0
    let mutable egWhiteView = 0
    let mutable phase = 0

    for sq = 0 to 63 do
        let p = b.[sq]
        if p <> Empty then
            let pi = pieceIndex p
            mgWhiteView <- mgWhiteView + pieceSqvMg.[pi].[sq]
            egWhiteView <- egWhiteView + pieceSqvEg.[pi].[sq]

            let k0 = kind0 p
            phase <- phase + phaseWeightOfKind k0

    let pawnMg, pawnEg = evalPawnStructure pos

    let mgWhiteView = mgWhiteView + pawnMg
    let egWhiteView = egWhiteView + pawnEg

    let phaseClamped =
        if phase < 0 then 0
        elif phase > MaxPhase then MaxPhase
        else phase

    let scoreWhiteView =
        (mgWhiteView * phaseClamped + egWhiteView * (MaxPhase - phaseClamped)) / MaxPhase

    let mobilityScore = countMobility pos
    let totalScore = scoreWhiteView + mobilityScore

    // side-to-move viewpoint (negamax-ready)
    if pos.State.ToPlay = Color.White then totalScore else -totalScore
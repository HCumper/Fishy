module Evaluation

open Fishy

[<Literal>]
let pawnValue = 1000

[<Literal>]
let knightValue = 3100

[<Literal>]
let bishopValue = 3200

[<Literal>]
let rookValue = 5000

[<Literal>]
let queenValue = 9000

[<Literal>]
let kingValue = 9000

let private pawnPlacementValues = Array2D.createBased 1 1 8 8 0
let private knightPlacementValues = Array2D.createBased 1 1 8 8 0
let private bishopPlacementValues = Array2D.createBased 1 1 8 8 0
let private rookPlacementValues = Array2D.createBased 1 1 8 8 0
let private queenPlacementValues = Array2D.createBased 1 1 8 8 0
let private kingMiddlegamePlacementValues = Array2D.createBased 1 1 8 8 0
let private kingEndgamePlacementValues = Array2D.createBased 1 1 8 8 0

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

let initializePlacementValues () =
    for rank = 1 to 8 do
        for file = 1 to 8 do
            pawnPlacementValues[file, rank] <- pawnPlacementTable[file - 1, rank - 1] * 10
            knightPlacementValues[file, rank] <- knightPlacementTable[file - 1, rank - 1] * 10
            bishopPlacementValues[file, rank] <- bishopPlacementTable[file - 1, rank - 1] * 10
            rookPlacementValues[file, rank] <- rookPlacementTable[file - 1, rank - 1] * 10
            queenPlacementValues[file, rank] <- queenPlacementTable[file - 1, rank - 1] * 10
            kingMiddlegamePlacementValues[file, rank] <- kingMiddlegamePlacementTable[file - 1, rank - 1] * 10
            kingEndgamePlacementValues[file, rank] <- kingEndgamePlacementTable[file - 1, rank - 1] * 10

let evaluate (board: sbyte[,]) otherState : int =

    let placementValue (board: sbyte[,]) file rank =
        match board[file, rank] with
        | WhitePawn -> pawnPlacementValues[9-rank, file]
        | WhiteKnight -> knightPlacementValues[9-rank, file]
        | WhiteBishop -> bishopPlacementValues[9-rank, file]
        | WhiteRook -> rookPlacementValues[9-rank, file]
        | WhiteQueen -> queenPlacementValues[9-rank, file]
        | WhiteKing -> kingMiddlegamePlacementValues[9-rank, file]
        | _ -> 0

    let inversePlacementValue (board: sbyte[,]) file rank =
        match board[file, rank] with
        | BlackPawn -> pawnPlacementValues[rank, file]
        | BlackKnight -> knightPlacementValues[rank, file]
        | BlackBishop -> bishopPlacementValues[rank, file]
        | BlackRook -> rookPlacementValues[rank, file]
        | BlackQueen -> queenPlacementValues[rank, file]
        | BlackKing -> kingMiddlegamePlacementValues[rank, file]
        | _ -> 0

    let pieceValue pieceType =
        match pieceType with
        | WhitePawn | BlackPawn -> pawnValue
        | WhiteKnight | BlackKnight -> knightValue
        | WhiteBishop | BlackBishop -> bishopValue
        | WhiteRook | BlackRook -> rookValue
        | WhiteQueen | BlackQueen -> queenValue
        | WhiteKing | BlackKing -> kingValue
        | _ -> 0

    // Evaluate body
    let mutable evaluation = 0

    for rank = 1 to 8 do
        for file = 1 to 8 do
            if board[file, rank] <> Empty then
                if board[file, rank] > Empty then
                    evaluation <- evaluation + pieceValue board[file, rank] + placementValue board file rank
                else
                    evaluation <- evaluation - pieceValue board[file, rank] - inversePlacementValue board file rank

    evaluation

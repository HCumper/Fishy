﻿module Evaluation

open Types
//open Microsoft.FSharp.Collections.Array2D

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

let pawnPlacementValues = Array2D.createBased 1 1 8 8 0
let knightPlacementValues = Array2D.createBased 1 1 8 8 0
let bishopPlacementValues = Array2D.createBased 1 1 8 8 0
let rookPlacementValues = Array2D.createBased 1 1 8 8 0
let queenPlacementValues = Array2D.createBased 1 1 8 8 0
let kingPlacementValues = Array2D.createBased 1 1 8 8 0

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
            pawnPlacementValues[file, rank] <- pawnPlacementTable[file - 1, rank - 1]
            knightPlacementValues[file, rank] <- knightPlacementTable[file - 1, rank - 1]
            bishopPlacementValues[file, rank] <- bishopPlacementTable[file - 1, rank - 1]
            rookPlacementValues[file, rank] <- rookPlacementTable[file - 1, rank - 1]
            queenPlacementValues[file, rank] <- queenPlacementTable[file - 1, rank - 1]
            kingPlacementValues[file, rank] <- kingMiddlegamePlacementTable[file - 1, rank - 1]

let private placementValue (board: (Piece * Color) option[,]) file rank =
    let absValue =
        match fst board[file, rank].Value with
        | Pawn -> pawnPlacementValues[file, rank]
        | Knight -> knightPlacementValues[file, rank]
        | Bishop -> bishopPlacementValues[file, rank]
        | Rook -> rookPlacementValues[file, rank]
        | Queen -> queenPlacementValues[file, rank]
        | _ -> 0

    if snd board[file, rank].Value = White then absValue else absValue * -1

let private pieceValue (pieceType, color) =
    let absValue =
        match pieceType with
        | Pawn -> pawnValue
        | Knight -> knightValue
        | Bishop -> bishopValue
        | Rook -> rookValue
        | Queen -> queenValue
        | _ -> 0

    if color = White then absValue else absValue * -1

let evaluate (board: (Piece * Color) option[,]) otherState : int =
    let mutable evaluation = 0

    for rank = 1 to 8 do
        for file = 1 to 8 do
            if board[file, rank] <> None then
                evaluation <- evaluation + pieceValue board[file, rank].Value + placementValue board file rank

    evaluation
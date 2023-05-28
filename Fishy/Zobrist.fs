module Zobrist

open Types
open System
open Fishy

let random = Random ()

let convertPieceTypeToZobristType pieceType =
    match pieceType with
    | WhiteKing -> 0
    | WhiteQueen -> 1
    | WhiteRook -> 2
    | WhiteBishop -> 3
    | WhiteKnight -> 4
    | WhitePawn -> 5
    | BlackKing -> 6
    | BlackQueen -> 7
    | BlackRook -> 8
    | BlackBishop -> 9
    | BlackKnight -> 10
    | BlackPawn -> 11
    | _ -> 0

// map that stores the precomputed Zobrist keys for each piece and square combination
let zobristKeys: int64[,,] =
    let keys = Array3D.zeroCreate 13 8 8

    for pieceType = 0 to 12 do
        for file = 0 to 7 do
            for rank = 0 to 7 do
                keys[pieceType, file, rank] <- int64 (random.NextDouble () * (float Int64.MaxValue))
    keys

//  update the hash value for an added or removed piece
let updatePositionHash pieceWithColor file rank positionHash =
    let (pieceSquareKey: int64) = zobristKeys[pieceWithColor, file, rank]
    int64 positionHash ^^^ pieceSquareKey

// initializes the hash value based on the given board position
let initializePositionHash (board: Board) (gameState: GameState) =
    let mutable (positionHash: int64) = 0
    for rank = 0 to 7 do
        for file = 0 to 7 do
            let piece = convertPieceTypeToZobristType board[file+1, rank+1]
            if piece <> 0 then positionHash <- updatePositionHash piece file rank positionHash
    if gameState.WhiteCanCastleKingside then
        positionHash <- updatePositionHash 12 0 0 positionHash // white castle kingside
    if gameState.WhiteCanCastleQueenside then
        positionHash <- updatePositionHash 12 0 1 positionHash // white castle queenside
    if gameState.BlackCanCastleKingside then
        positionHash <- updatePositionHash 12 0 2 positionHash // black castle kingside
    if gameState.BlackCanCastleQueenside then
        positionHash <- updatePositionHash 12 0 3 positionHash // black castle queenside
    if gameState.EPSquare <> None then
        let epFile, _ = gameState.EPSquare.Value
        positionHash <- updatePositionHash 12 1 epFile  positionHash
    if gameState.ToPlay = Black then
        positionHash <- updatePositionHash 12 2 0  positionHash
    positionHash

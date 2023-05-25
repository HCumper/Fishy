module FENParser

open Types
open Fishy

let parseFEN (fen: string) =
    let parsePiece c =
        match c with
        | 'k' -> BlackKing
        | 'q' -> BlackQueen
        | 'r' -> BlackRook
        | 'b' -> BlackBishop
        | 'n' -> BlackKnight
        | 'p' -> BlackPawn
        | 'K' -> WhiteKing
        | 'Q' -> WhiteQueen
        | 'R' -> WhiteRook
        | 'B' -> WhiteBishop
        | 'N' -> WhiteKnight
        | 'P' -> WhitePawn
        | _ -> Empty

    let parseBoard boardStr =
        let mutable rank = 8
        let mutable file = 1
        for c in boardStr do
            match c with
            | '/' -> rank <- rank - 1; file <- 1
            | _ when c >= '1' && c <= '8' ->
                let numEmptySquares = int(c) - int('0')
                for i in 1 .. numEmptySquares do
                    currentBoard[file + i - 1, rank] <- Empty
                file <- file + numEmptySquares
            | _ -> currentBoard[file, rank] <- parsePiece c; file <- file + 1
        currentBoard

    let parts = fen.Split(' ')
    parseBoard parts[0]

let pieceToChar (piece: sbyte) =
    match piece with
    | WhiteKing -> 'K'
    | WhiteQueen -> 'Q'
    | WhiteRook -> 'R'
    | WhiteBishop -> 'B'
    | WhiteKnight -> 'N'
    | WhitePawn -> 'P'

let colorToChar (color: sbyte) =
    match color with
    | White -> 'w'
    | Black -> 'b'

let boardToFen (board: Board) =
    let mutable fen = ""
    for rank = 8 downto 1 do  // Iterate ranks in reverse order
        let mutable emptySquares = 0
        for file = 1 to 8 do
            match board[file, rank] with
            | Empty ->
                emptySquares <- emptySquares + 1
            | piece ->
                if emptySquares > 0 then
                    fen <- fen + string emptySquares  // Append number of empty squares
                    emptySquares <- 0
                let temp = string (pieceToChar (abs piece))
                if piece > Empty then
                    fen <- fen + temp
                else
                    fen <- fen + temp.ToLower()

        if emptySquares > 0 then
            fen <- fen + string emptySquares  // Append number of empty squares
        if rank > 1 then
            fen <- fen + "/"

    fen

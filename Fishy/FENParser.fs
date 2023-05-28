module FENParser

open System.Collections.Generic
open Types
open Fishy
open Zobrist

let private convertCoordinatesToNumbers (square: string) : (int * int) option =
    let file = int square[0] - int 'a' + 1
    match square with
    | "-" -> None
    | x -> Some (file, int square[1])

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
        | _ -> 0y

    let parseBoard boardStr =
        let mutable rank = 8
        let mutable file = 1
        let mutable board = Array2D.createBased -1 -1 12 12 offBoard

        for c in boardStr do
            match c with
            | '/' -> rank <- rank - 1; file <- 1
            | _ when c >= '1' && c <= '8' ->
                let numEmptySquares = int(c) - int('0')
                for i in 1 .. numEmptySquares do
                    board[file + i - 1, rank] <- Empty
                file <- file + numEmptySquares
            | _ -> board[file, rank] <- parsePiece c; file <- file + 1
        board

    let fields = fen.Split(' ')
    let board = parseBoard fields[0]
    transpositionTable <- Dictionary<int64, (int * int)>()

    let activeColor = fields[1]
    let castlingStr = fields[2]
    let whiteKingSide = castlingStr.Contains('K')
    let whiteQueenSide = castlingStr.Contains('Q')
    let blackKingSide = castlingStr.Contains('k')
    let blackQueenSide = castlingStr.Contains('q')

    let enPassantTargetSquare = fields[3]
    let halfMoveClock = int fields[4]
    let fullmoveNumber = int fields[5]

    board, {
        WhiteCanCastleKingside = whiteKingSide
        WhiteCanCastleQueenside = whiteQueenSide
        BlackCanCastleKingside = blackKingSide
        BlackCanCastleQueenside = blackQueenSide
        ToPlay = match activeColor with | "w" -> 1y | _ -> -1y
        EPSquare = convertCoordinatesToNumbers enPassantTargetSquare
        HalfMoveClock = halfMoveClock
        FullMoveNumber = fullmoveNumber
        HashKey = initializePositionHash board
    }

let pieceToChar (piece: sbyte) =
    match piece with
    | WhiteKing -> 'K'
    | WhiteQueen -> 'Q'
    | WhiteRook -> 'R'
    | WhiteBishop -> 'B'
    | WhiteKnight -> 'N'
    | WhitePawn -> 'P'
    | _ -> ' '

// Handles board string only
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

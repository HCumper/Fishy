module FENParser

open Types
open Chess
let parseFEN (fen: string) =
    let parsePiece c =
        match c with
        | 'k' -> Some (King, Black)
        | 'q' -> Some (Queen, Black)
        | 'r' -> Some (Rook, Black)
        | 'b' -> Some (Bishop, Black)
        | 'n' -> Some (Knight, Black)
        | 'p' -> Some (Pawn, Black)
        | 'K' -> Some (King, White)
        | 'Q' -> Some (Queen, White)
        | 'R' -> Some (Rook, White)
        | 'B' -> Some (Bishop, White)
        | 'N' -> Some (Knight, White)
        | 'P' -> Some (Pawn, White)
        | _ -> None

    let parseBoard boardStr =
        let mutable rank = 8
        let mutable file = 1
        for c in boardStr do
            match c with
            | '/' -> rank <- rank - 1; file <- 1
            | _ when c >= '1' && c <= '8' ->
                let numEmptySquares = int(c) - int('0')
                for i in 1 .. numEmptySquares do
                    board.[file + i - 1, rank] <- None
                file <- file + numEmptySquares
            | _ -> board[file, rank] <- parsePiece c; file <- file + 1
        board

    let parts = fen.Split(' ')
    let board = parseBoard parts.[0]
    board


let pieceToChar (piece: Piece) =
    match piece with
    | King -> 'K'
    | Queen -> 'Q'
    | Rook -> 'R'
    | Bishop -> 'B'
    | Knight -> 'N'
    | Pawn -> 'P'

let colorToChar (color: Color) =
    match color with
    | White -> 'w'
    | Black -> 'b'

let boardToFen (board: Board) =
    let mutable fen = ""
    for rank = 8 downto 1 do  // Iterate ranks in reverse order
        let mutable emptySquares = 0
        for file = 1 to 8 do
            match board.[file, rank] with
            | Some (piece, color) ->
                if emptySquares > 0 then
                    fen <- fen + string emptySquares  // Append number of empty squares
                    emptySquares <- 0
                let temp = string (pieceToChar piece)
                if color = White then
                    fen <- fen + temp
                else
                    fen <- fen + temp.ToLower()
            | None ->
                emptySquares <- emptySquares + 1
        if emptySquares > 0 then
            fen <- fen + string emptySquares  // Append number of empty squares
        if rank > 1 then
            fen <- fen + "/"

    fen

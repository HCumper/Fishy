module MakeMove

open System
open Chess
open Types

let makeMove move currentState =
    let pieceMoving = board[move.fromFile, move.toFile].Value

    let epSquare =
        if board[move.toFile, move.toRank] = None then None else Some (move.toFile, move.fromRank)

    // capture field in move already populated by move generator
    board[move.toFile, move.toRank] <-
        if move.promoteTo = None then
            if epSquare = None then
                board[move.fromFile, move.fromRank]
            else
                // remove captured pawn
                board[move.toFile, move.fromRank] = None
                Some (Pawn, (snd board[move.fromFile, move.fromRank].Value))
        else
            Some (move.promoteTo.Value, (snd board[move.fromFile, move.fromRank].Value))

    board[move.fromFile, move.fromRank] <- None

    { currentState with
        WhiteKingMoved = pieceMoving = (King, White)
        WhiteKRMoved = (pieceMoving = (Rook, White)) && move.fromFile = 8 && move.fromRank = 1
        WhiteQRMoved = ((pieceMoving = (Rook, White)) && move.fromFile = 1 && move.fromRank = 1)
        BlackKingMoved = pieceMoving = (King, Black)
        BlackKRMoved = (pieceMoving = (Rook, Black)) && move.fromFile = 8 && move.fromRank = 1
        BlackQRMoved = ((pieceMoving = (Rook, Black)) && move.fromFile = 1 && move.fromRank = 1)
        ToPlay = if snd pieceMoving = White then Black else White
    }

let parseMove (move: string) : int * int * int * int * Piece option =
    let parseFile c =
        match c with
        | 'a' -> 1
        | 'b' -> 2
        | 'c' -> 3
        | 'd' -> 4
        | 'e' -> 5
        | 'f' -> 6
        | 'g' -> 7
        | _ -> 8

    let convertLetterToPiece letter =
        match letter with
        | 'q' -> Queen
        | 'r' -> Rook
        | 'b' -> Bishop
        | 'n' -> Knight
        | _ -> Pawn

    let promoteTo =
        if move.Length = 5 then Some (convertLetterToPiece move[4]) else None

    parseFile move[0],
    Int32.Parse (move[ 1 ].ToString ()),
    parseFile move[2],
    Int32.Parse (move[ 3 ].ToString ()),
    promoteTo

let parseAndMakeMove strMove currentState =
    let fromFile, fromRank, toFile, toRank, promoteTo = parseMove strMove
    let capturedPiece =
        if board[toFile, toRank] = None then
            None
        else
            Some (fst board[toFile, toRank].Value)
    let move = {
        fromFile = fromFile
        fromRank = fromRank
        toFile = toFile
        toRank = toRank
        promoteTo = promoteTo
        capturedPiece = capturedPiece
    }
    makeMove move currentState

module MakeMove

open System
open Chess
open Types

let makeMove fromFile fromRank toFile toRank promoteTo =
    let pieceMoving = board[fromFile, fromRank]

    if pieceMoving = None then
        failwith "attempt to move None"

    let epSquare =
        if board[toFile, toRank] = None then None else Some (toFile, fromRank)

    let updatedState =
        { currentState with
            WhiteKingMoved = pieceMoving = Some (King, White)
            WhiteKRMoved = (pieceMoving = Some (Rook, White)) && fromFile = 8 && fromRank = 1
            WhiteQRMoved = ((pieceMoving = Some (Rook, White)) && fromFile = 1 && fromRank = 1)
            BlackKingMoved = pieceMoving = Some (King, Black)
            BlackKRMoved = (pieceMoving = Some (Rook, Black)) && fromFile = 8 && fromRank = 1
            BlackQRMoved = ((pieceMoving = Some (Rook, Black)) && fromFile = 1 && fromRank = 1)
            ToPlay = if snd pieceMoving.Value = White then Black else White
            PromoteTo = promoteTo
            EPSquare = epSquare }
    currentState <- updatedState

    board[toFile, toRank] <- if promoteTo <> None then
                                Some (promoteTo.Value, (snd board[fromFile, fromRank].Value))
                             else
                                board[fromFile, fromRank]

    board[fromFile, fromRank] <- None
    ()

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

let parseAndMakeMove move =
    let fromFile, fromRank, toFile, toRank, promotTo = parseMove move
    makeMove fromFile fromRank toFile toRank promotTo

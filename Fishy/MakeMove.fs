module MakeMove

open System
open Fishy
open Types

let makeMove (board: sbyte[,]) currentState move =

    let pieceMoving = board[move.fromFile, move.fromRank]

    let isEp =
        abs pieceMoving = WhitePawn
        && move.fromFile <> move.toFile
        && board[move.toFile, move.toRank] = Empty

    // capture field in move already populated by move generator
    if move.promoteTo = Empty then
        if isEp then
            board[move.toFile, move.toRank] <- pieceMoving
            board[move.toFile, move.fromRank] <- Empty
        else
            if
                abs pieceMoving = WhiteKing
                && (abs (move.fromFile - move.toFile)) = 2
                && (move.toFile = 7 && ((abs board[8, move.toRank]) = WhiteRook)
                    || (move.toFile = 3 && ((abs board[1, move.toRank]) = WhiteRook)))
            then // castling
                if move.toFile = 7 then
                    board[6, move.toRank] <- board[8, move.toRank]
                    board[8, move.toRank] <- Empty
                else
                    board[4, move.toRank] <- board[1, move.toRank]
                    board[1, move.toRank] <- Empty

            board[move.toFile, move.toRank] <- pieceMoving
    else
        board[move.toFile, move.toRank] <- move.promoteTo

    board[move.fromFile, move.fromRank] <- Empty

    let epSquare = if isEp then Some (move.toFile, move.fromRank) else None

    (board,
     { currentState with
         WhiteKingMoved = pieceMoving = WhiteKing
         WhiteKRMoved = (pieceMoving = WhiteRook) && move.fromFile = 8 && move.fromRank = 1
         WhiteQRMoved = ((pieceMoving = WhiteRook) && move.fromFile = 1 && move.fromRank = 1)
         BlackKingMoved = pieceMoving = BlackKing
         BlackKRMoved = (pieceMoving = BlackRook) && move.fromFile = 8 && move.fromRank = 1
         BlackQRMoved = ((pieceMoving = BlackRook) && move.fromFile = 1 && move.fromRank = 1)
         ToPlay = -currentState.ToPlay })

let parseAndMakeMove (board: sbyte[,]) (currentState: OtherState) strMove =
    let parseMove (move: string) : int * int * int * int * sbyte =
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
            | 'q' -> WhiteQueen
            | 'r' -> WhiteRook
            | 'b' -> WhiteBishop
            | 'n' -> WhiteKnight
            | _ -> WhitePawn

        let promoteTo = if move.Length = 5 then convertLetterToPiece move[4] else Empty

        parseFile move[0],
        Int32.Parse (move[ 1 ].ToString ()),
        parseFile move[2],
        Int32.Parse (move[ 3 ].ToString ()),
        promoteTo

    let fromFile, fromRank, toFile, toRank, promoteTo = parseMove strMove

    let capturedPiece =
        if board[toFile, toRank] = Empty then Empty else board[toFile, toRank]

    let move =
        { fromFile = fromFile
          fromRank = fromRank
          toFile = toFile
          toRank = toRank
          capturedPiece = capturedPiece
          promoteTo = promoteTo }

    makeMove board currentState move

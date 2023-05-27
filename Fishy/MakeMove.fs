module MakeMove

open System
open Fishy
open Types

let makeMove (board: sbyte[,]) (currentState: GameState) move =

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
    let whiteCastling =
        match pieceMoving with
        | WhiteKing -> false, false
        | x when WhiteRook = x && move.fromRank = 1 -> false, currentState.WhiteCanCastleQueenside
        | x when WhiteRook = x && move.fromRank = 1 -> currentState.WhiteCanCastleKingside, false
        | _ -> currentState.WhiteCanCastleKingside, currentState.WhiteCanCastleQueenside

    let blackCastling =
        match pieceMoving with
        | BlackKing -> false, false
        | x when BlackRook = x && move.fromRank = 1 -> false, currentState.BlackCanCastleQueenside
        | x when BlackRook = x && move.fromRank = 1 -> currentState.BlackCanCastleKingside, false
        | _ -> currentState.BlackCanCastleKingside, currentState.BlackCanCastleQueenside

    (board,
     { currentState with
        WhiteCanCastleKingside = fst whiteCastling
        WhiteCanCastleQueenside = snd whiteCastling
        BlackCanCastleKingside = fst blackCastling
        BlackCanCastleQueenside =  snd blackCastling
        ToPlay = -currentState.ToPlay
        EPSquare = epSquare
        FullMoveNumber = match pieceMoving with | x when x > 0y -> currentState.FullMoveNumber | _ -> currentState.FullMoveNumber + 1
        HalfMoveClock = currentState.HalfMoveClock + 1
        })

let parseAndMakeMove (board: sbyte[,]) (currentState: GameState) strMove =
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

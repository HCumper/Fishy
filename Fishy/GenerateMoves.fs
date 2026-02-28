module GenerateMoves

open System.Diagnostics
open Types
open BoardHelpers
open PieceCode
open Board
open MakeMove

let inline private otherColor (c: Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

let inline private onBoard (file:int) (rank:int) =
    file >= MinFileRank && file <= MaxFileRank &&
    rank >= MinFileRank && rank <= MaxFileRank

let inline private getAt (board: Board) (file:int) (rank:int) : sbyte =
    // 1-based -> 0-based internal array
    board.[file - 1, rank - 1]

let inline private isAttackerColor (attacker: Color) (p: sbyte) =
    p <> Empty &&
    ((attacker = Color.White && isWhite p) ||
     (attacker = Color.Black && isBlack p))

let inline private isAttackerPawn (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Pawn

let inline private isAttackerKnight (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Knight

let inline private isAttackerBishop (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Bishop

let inline private isAttackerRook (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Rook

let inline private isAttackerQueen (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Queen

let inline private isAttackerKing (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = King

let private knightOffsets =
    [|
        ( 1,  2); ( 2,  1); (-1,  2); (-2,  1)
        ( 1, -2); ( 2, -1); (-1, -2); (-2, -1)
    |]

let private kingOffsets =
    [|
        ( 1,  0); ( 1,  1); ( 0,  1); (-1,  1)
        (-1,  0); (-1, -1); ( 0, -1); ( 1, -1)
    |]

let generateLegalKnightMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq

    let side = pos.State.ToPlay
    // Must be a knight of side to move
    if not (isKnight piece) || (side = Color.White && isBlack piece) || (side = Color.Black && isWhite piece) then
        []
    else
        let moves = ResizeArray<Move>(8)

        for (df, dr) in knightOffsets do
            let newFile = int fromSq.File + df
            let newRank = int fromSq.Rank + dr

            if Coordinates.isValid {File = byte newFile; Rank = byte newRank} then

                let toSq =
                    { File = byte newFile
                      Rank = byte newRank }

                let target = getC board toSq

                // empty square OR opponent piece
                if target = Empty || oppositeColor piece target then

                    let move =
                        { From = fromSq
                          To = toSq
                          Piece = piece
                          PromoteTo = 0y }

                    // Make move
                    let mutable p = pos
                    let undo = makeMove &p move

                    // If king not left in check, it's legal
                    if not (inCheck p side) then
                        moves.Add(move)

                    // Undo
                    unmakeMove &p move undo

        List.ofSeq moves

// Generic helper for bishops/rooks/queens (sliding pieces)
let private generateLegalSlidingMoves
    (pos: Position)
    (fromSq: Coordinates)
    (dirs: (int * int)[])
    (pieceOk: sbyte -> bool)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq
    let side = pos.State.ToPlay

    // Must be correct piece type AND belong to side to move
    if not (pieceOk piece) ||
       (side = Color.White && isBlack piece) ||
       (side = Color.Black && isWhite piece) then
        []
    else
        // Upper bound: queen from center can have up to 27 pseudo-legal moves.
        let moves = ResizeArray<Move>(32)

        for (df, dr) in dirs do
            let mutable newFile = int fromSq.File + df
            let mutable newRank = int fromSq.Rank + dr
            let mutable blocked = false

            while not blocked &&
                  newFile >= MinFileRank && newFile <= MaxFileRank &&
                  newRank >= MinFileRank && newRank <= MaxFileRank do

                let toSq = { File = byte newFile; Rank = byte newRank }
                let target = getC board toSq

                // empty OR capture opponent
                if target = Empty || oppositeColor piece target then
                    let move =
                        { From = fromSq
                          To = toSq
                          Piece = piece
                          PromoteTo = 0y }

                    let mutable p = pos
                    let undo = makeMove &p move
                    if not (inCheck p side) then moves.Add(move)
                    unmakeMove &p move undo

                // stop ray on any occupied square (capture or blocked by friendly)
                if target <> Empty then
                    blocked <- true
                else
                    newFile <- newFile + df
                    newRank <- newRank + dr

        List.ofSeq moves


let generateLegalBishopMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let bishopDirs = [| ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
    generateLegalSlidingMoves pos fromSq bishopDirs isBishop inCheck


let generateLegalRookMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let rookDirs = [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1) |]
    generateLegalSlidingMoves pos fromSq rookDirs isRook inCheck


let generateLegalQueenMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let queenDirs =
        [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1)
           ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
    generateLegalSlidingMoves pos fromSq queenDirs isQueen inCheck
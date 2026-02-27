module GenerateMoves

open System.Diagnostics
open Types
open BoardHelpers
open PieceCode
open Board

// Knight relative offsets
let private knightOffsets =
    [|
        ( 1,  2); ( 2,  1)
        (-1,  2); (-2,  1)
        ( 1, -2); ( 2, -1)
        (-1, -2); (-2, -1)
    |]

module Attacks =

    open Types
    open PieceCode
    open Board

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

/// True if `side`'s king is currently in check.

let generateLegalKnightMoves
    (pos: Position)
    (fromSq: Coordinates)
    (makeMove: byref<Position> -> Move -> MoveUndo)
    (unmakeMove: byref<Position> -> Move -> MoveUndo -> unit)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq

    // Must be a knight of side to move
    if not (isKnight piece) then
        []
    else

        let side = pos.State.ToPlay
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

// let private bishopDirs = [| ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
// let private rookDirs   = [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1) |]
//
// /// True if `square` is attacked by any piece of `attacker`.
// /// Coordinates are 1-based (File/Rank = 1..8).
// let isSquareAttacked (pos: Position) (square: Coordinates) (attacker: Color) : bool =
//     let board = pos.Board
//     let f = int square.File
//     let r = int square.Rank
//
//     // --- Pawn attacks (check from target backwards to pawn source squares) ---
//     // If attacker is White: pawns that attack (f,r) would be on (f±1, r-1)
//     // If attacker is Black: pawns that attack (f,r) would be on (f±1, r+1)
//     let pawnSourceRank =
//         if attacker = Color.White then r - 1 else r + 1
//
//     if onBoard (f - 1) pawnSourceRank then
//         let p = getAt board (f - 1) pawnSourceRank
//         if isAttackerPawn attacker p then true
//         else
//             if onBoard (f + 1) pawnSourceRank then
//                 let p2 = getAt board (f + 1) pawnSourceRank
//                 if isAttackerPawn attacker p2 then true
//                 else
//                     // continue checks
//                     ()
//             // fallthrough
//     elif onBoard (f + 1) pawnSourceRank then
//         let p2 = getAt board (f + 1) pawnSourceRank
//         if isAttackerPawn attacker p2 then true
//         else
//             ()
//     // If we returned true above, we already exited; otherwise continue.
//
//     // To keep it simple and fast to read, do early-exit checks in a sequence:
//     // We'll use a local function to bail out.
//     let mutable attacked = false
//
//     // Pawn check again but integrated with mutable flow (covers all cases cleanly)
//     if not attacked then
//         if onBoard (f - 1) pawnSourceRank then
//             let p = getAt board (f - 1) pawnSourceRank
//             if isAttackerPawn attacker p then attacked <- true
//         if not attacked && onBoard (f + 1) pawnSourceRank then
//             let p = getAt board (f + 1) pawnSourceRank
//             if isAttackerPawn attacker p then attacked <- true
//
//     // --- Knight attacks ---
//     if not attacked then
//         for (df, dr) in knightOffsets do
//             let nf = f + df
//             let nr = r + dr
//             if onBoard nf nr then
//                 let p = getAt board nf nr
//                 if isAttackerKnight attacker p then attacked <- true
//
//     // --- King adjacency (rare but required) ---
//     if not attacked then
//         for (df, dr) in kingOffsets do
//             let nf = f + df
//             let nr = r + dr
//             if onBoard nf nr then
//                 let p = getAt board nf nr
//                 if isAttackerKing attacker p then attacked <- true
//
//     // --- Sliding attacks: bishops/queens on diagonals ---
//     let inline scanRay (df:int) (dr:int) (isSlider: sbyte -> bool) =
//         let mutable nf = f + df
//         let mutable nr = r + dr
//         while not attacked && onBoard nf nr do
//             let p = getAt board nf nr
//             if p <> Empty then
//                 if isSlider p then attacked <- true
//                 // blocked either way
//                 nf <- 99
//                 nr <- 99
//             else
//                 nf <- nf + df
//                 nr <- nr + dr
//
//     if not attacked then
//         for (df, dr) in bishopDirs do
//             scanRay df dr (fun p -> isAttackerBishop attacker p || isAttackerQueen attacker p)
//
//     // --- Sliding attacks: rooks/queens on ranks/files ---
//     if not attacked then
//         for (df, dr) in rookDirs do
//             scanRay df dr (fun p -> isAttackerRook attacker p || isAttackerQueen attacker p)
//
//     attacked


// let generationStopwatch = Stopwatch()
// let mutable generationCount = 0
//
// // Create list of all available moves on the supplied board
// let generateMoves (board: Board) gameState : Move list =
//
//     let isSquareOnBoard (file, rank) = file >= 1 && file <= 8 && rank >= 1 && rank <= 8
//
//     let isSquareOpponentPiece (movingPiece: sbyte) (file, rank) = board[file, rank] * movingPiece < 0y
//     let isSquareEmptyOrOpponentPiece (movingPiece: sbyte) (file, rank) = board[file, rank] * movingPiece <= 0y && file >= 1 && file <= 8 && rank >= 1 && rank <= 8
//     
//     let convertToMove fromFile fromRank toFile toRank promoteTo capturedPiece : Move =
//         {
//             FromFile = fromFile
//             FromRank = fromRank
//             ToFile = toFile
//             ToRank = toRank
//             PromoteTo = promoteTo
//             CapturedPiece = capturedPiece
//         }
//
//     // can move into check
//     let generateKingMoves (file, rank) =
//         let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
//         let mutable kingMoves = []
//
//         // ordinary king moves
//         let isOppositionAllows (deltaFile, deltaRank) =
//             if isSquareEmptyOrOpponentPiece gameState.ToPlay (deltaFile, deltaRank) then
//                 let availableDirections = List.filter (fun x -> isSquareOnBoard ((fst x) + deltaFile, (snd x) + deltaRank)) directions
//                 List.exists (fun x -> abs board[(fst x) + deltaFile, (snd x) + deltaRank] = WhiteKing) availableDirections
//             else
//                 false
//
//         kingMoves <- directions
//             |> List.map (fun (x,y) -> (x + file, y + rank))
//             |> List.filter isSquareOnBoard
//             |> List.filter (isSquareEmptyOrOpponentPiece gameState.ToPlay)
//             |> List.filter isOppositionAllows
//             |> List.map (fun (x, y) -> convertToMove file rank x y Empty board[x, y])
//                      
//         // castling
//         if gameState.ToPlay = White then
//             if gameState.WhiteCanCastleKingside then
//                 if board[6, 1] = Empty && board[7, 1] = Empty then
//                     kingMoves <- convertToMove 5 1 7 1 Empty Empty :: kingMoves
//
//             if gameState.WhiteCanCastleQueenside then
//                 if board[4, 1] = Empty && board[3, 1] = Empty && board[2, 1] = Empty then
//                     kingMoves <- convertToMove 5 1 3 1 Empty Empty:: kingMoves
//         else
//             if gameState.BlackCanCastleKingside then
//                 if board[6, 8] = Empty && board[7, 8] = Empty then
//                     kingMoves <- convertToMove 5 8 7 8 Empty Empty :: kingMoves
//
//                 if not gameState.BlackCanCastleQueenside then
//                     kingMoves <- convertToMove 5 8 3 8 Empty Empty:: kingMoves
//
//         kingMoves
//
//     let generatePawnMoves (file, rank) =
//         let direction = if gameState.ToPlay = Black then -1 else 1
//
//         let singleAdvanceMoves =
//             if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = Empty then
//                 if rank + direction = 1 || rank + direction = 8 then
//                     [WhiteQueen; WhiteRook; WhiteBishop; WhiteKnight]
//                     |> List.map (fun promoteTo -> convertToMove file rank file (rank + direction) (promoteTo * sbyte direction) Empty)
//                 else [convertToMove file rank file (rank + direction) Empty Empty]
//             else []
//
//         let ordinaryCaptureMoves =
//             let captureMoves deltaFile =
//                 if isSquareOnBoard (file + deltaFile, rank + direction) && isSquareOpponentPiece board[file, rank] (file + deltaFile, rank + direction) then
//                     if rank + direction = 1 || rank + direction = 8 then
//                         [WhiteQueen; WhiteRook; WhiteBishop; WhiteKnight]
//                         |> List.map (fun promoteTo -> convertToMove file rank (file + deltaFile) (rank + direction) (promoteTo * sbyte direction) Empty)
//                     else [convertToMove file rank (file + deltaFile) (rank + direction) Empty Empty]
//                 else []
//             captureMoves -1 @ captureMoves 1
//
//         let epCaptureMoves =
//             let epCapture deltaFile =
//                 if gameState.EPSquare = Some (file + deltaFile, rank + direction) then
//                     [convertToMove file rank (file + deltaFile) (rank + direction) Empty Empty]
//                 else []
//             epCapture -1 @ epCapture 1
//
//         let doubleAdvanceMove =
//             if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = Empty &&
//                isSquareOnBoard (file, rank + direction * 2) && board[file, rank + direction * 2] = Empty && (rank = 2 || rank = 7) then
//                 [convertToMove file rank file (rank + direction * 2) Empty Empty]
//             else []
//
//         singleAdvanceMoves @ ordinaryCaptureMoves @ epCaptureMoves @ doubleAdvanceMove
//
//     let generateKnightMoves (file, rank) : Move list =
//         [ (file + 2, rank + 1)
//           (file + 2, rank - 1)
//           (file - 2, rank + 1)
//           (file - 2, rank - 1)
//           (file + 1, rank + 2)
//           (file + 1, rank - 2)
//           (file - 1, rank + 2)
//           (file - 1, rank - 2) ]
//         |> List.filter isSquareOnBoard
//         |> List.filter (isSquareEmptyOrOpponentPiece gameState.ToPlay)
//         |> List.map (fun (x, y) -> convertToMove file rank x y Empty board[x, y])
//
//     let generateVectorMoves (file, rank) directions =
//
//         let generateMoves deltaFile deltaRank =
//             let rec loop newFile newRank (moves: Move list) =
//                 let nFile, nRank = newFile + deltaFile, newRank + deltaRank
//
//                 if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece gameState.ToPlay (nFile, nRank) then
//                     let updatedMoves = (convertToMove file rank nFile nRank Empty board[nFile, nRank]) :: moves
//                     if board[nFile, nRank] = Empty then
//                         loop nFile nRank updatedMoves
//                     else
//                         updatedMoves
//                 else
//                     moves
//
//             loop file rank []
//
//         List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions
//
//     // generate moves body
//     generationCount <- generationCount + 1
//     generationStopwatch.Start()
//
//     let mutable availableMoves = []
//     for rank = 1 to 8 do
//         for file = 1 to 8 do
//             match board[file, rank] with
//             | x when x * gameState.ToPlay > 0y ->
//                 let pieceMoves =
//                     match abs x with
//                     | WhiteKnight -> generateKnightMoves (file, rank)
//                     | WhiteBishop -> generateVectorMoves (file, rank) [(1, 1); (1, -1); (-1, 1); (-1, -1)]
//                     | WhiteRook -> generateVectorMoves (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1)]
//                     | WhiteQueen -> generateVectorMoves (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
//                     | WhitePawn -> generatePawnMoves (file, rank)
//                     | WhiteKing -> generateKingMoves (file, rank)
//                     | _ -> []
//
//                 availableMoves <- pieceMoves @ availableMoves
//             | _ -> ()
//         generationStopwatch.Stop()
//     availableMoves

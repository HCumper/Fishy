module GenerateMoves

open Types

let private convertToMove fromFile fromRank toFile toRank promoteTo : Move = ((fromFile, fromRank), (toFile, toRank), promoteTo)
let private isSquareOnBoard (file, rank) = file >= 1 && file <= 8 && rank >= 1 && rank <= 8

let private isSquareEmptyOrOpponentPiece (board: Board) movingColor coordinates =
    match board[fst coordinates, snd coordinates] with
    | None -> true
    | Some (_, color) -> color <> movingColor

let private isSquareOpponentPiece (board: Board) movingColor coordinates =
    match board[fst coordinates, snd coordinates] with
    | None -> false
    | Some (_, color) -> color <> movingColor

let private isSquareEmpty (board: Board) (file, rank) : bool = board[file, rank] = None

// the generatePawnMoves function now includes support for en passant captures. The function takes an additional parameter enPassantTarget, which represents the square where an en passant capture is possible.
//
// If the enPassantTarget matches the conditions for an en passant capture (same file and adjacent rank), the function adds the en passant capture move to the resulting list.
//
// The en passant capture move is represented as (x + forwardOffset, ey, None), where x is the pawn's rank, forwardOffset is the direction based on the pawn's color, and ey is the captured pawn's file. The None value represents that no piece is placed on the destination square.
//
// Please ensure that you provide the correct enPassantTarget value based on the game state to generate the valid en passant capture moves.
//

// can move into check
let generateKingMoves board (gameState: OtherState) (file, rank) =
    let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
    let mutable (kingMoves: Move list) = []

    // ordinary king moves
    let isOppositionAllows (deltaFile, deltaRank) : bool =
        if isSquareEmptyOrOpponentPiece board gameState.ToPlay (deltaFile, deltaRank) then
            let availableDirections = List.filter (fun x -> isSquareOnBoard ((fst x) + deltaFile, (snd x) + deltaRank)) directions
            not (List.exists (fun x -> board[(fst x) + deltaFile, (snd x) + deltaRank] = Some (King, Black)) availableDirections)
        else
            false

    kingMoves <- directions
        |> List.map (fun (x,y) -> (x + file, y + rank))
        |> List.filter isSquareOnBoard
        |> List.filter (isSquareEmptyOrOpponentPiece board gameState.ToPlay)
        |> List.filter isOppositionAllows
        |> List.map (fun x -> ((file, rank), x, None))

    // castling
    if gameState.ToPlay = White then
        if not gameState.WhiteKingMoved then
            if not gameState.WhiteKRMoved then
                if board[6, 1] = None && board[7, 1] = None then
                    kingMoves <- convertToMove 5 1 7 1 None :: kingMoves

            if not gameState.WhiteQRMoved then
                if board[4, 1] = None && board[3, 1] = None && board[2, 1] = None then
                    kingMoves <- convertToMove 5 1 3 1 None :: kingMoves
    else
        if not gameState.BlackKingMoved then
            if not gameState.BlackKRMoved then
                if board[6, 1] = None && board[7, 1] = None then
                    kingMoves <- convertToMove 5 1 7 1 None :: kingMoves

            if not gameState.BlackQRMoved then
                if board[4, 1] = None && board[3, 1] = None && board[2, 1] = None then
                    kingMoves <- convertToMove 5 1 3 1 None :: kingMoves

    kingMoves

let generatePawnMoves board (gameState: OtherState) (file, rank) =
    let direction = if gameState.ToPlay = Black then -1 else 1

    let singleAdvanceMoves =
        if isSquareOnBoard (file, rank + direction) && isSquareEmpty board (file, rank + direction) then
            if rank + direction = 1 || rank + direction = 8 then
                [convertToMove file rank file (rank + direction) (Some Queen);
                 convertToMove file rank file (rank + direction) (Some Rook);
                 convertToMove file rank file (rank + direction) (Some Bishop);
                 convertToMove file rank file (rank + direction) (Some Knight)]
            else
                [convertToMove file rank file (rank + direction) None]
        else
            []

    let ordinaryCaptureMoves =
        let leftCapture =
            if isSquareOnBoard (file - 1, rank + direction) && isSquareOpponentPiece board gameState.ToPlay (file - 1, rank + direction) then
                [convertToMove file rank (file - 1) (rank + direction) None]
            else
                []
        let rightCapture =
            if isSquareOnBoard (file + 1, rank + direction) && isSquareOpponentPiece board gameState.ToPlay (file + 1, rank + direction) then
                [convertToMove file rank (file + 1) (rank + direction) None]
            else
                []
        leftCapture @ rightCapture

    let epCaptureMoves =
        let epLeftCapture =
            if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file - 1, rank) then
                [convertToMove file rank (file - 1) (rank + direction) None]
            else
                []
        let epRightCapture =
            if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file + 1, rank) then
                [convertToMove file rank (file + 1) (rank + direction) None]
            else
                []
        epLeftCapture @ epRightCapture

    let doubleAdvanceMove =
        if isSquareOnBoard (file, rank + direction) && isSquareEmpty board (file, rank + direction) &&
           isSquareOnBoard (file, rank + direction * 2) && isSquareEmpty board (file, rank + direction * 2) then
            [convertToMove file rank file (rank + direction * 2) None]
        else
            []

    singleAdvanceMoves @ ordinaryCaptureMoves @ epCaptureMoves @ doubleAdvanceMove

//     let isOpponentPiece (x: int, y: int) : bool =
//         match board.[x, y] with
//         | (_, Some color) -> color <> pieceColor
//         | _ -> false
//
//     let generateMoves (x: int, y: int) : Coordinates list =
//         let moves =
//             [ (x + forwardOffset, y - 1)
//               (x + forwardOffset, y + 1) ]
//         moves
//         |> List.filter (fun (x, y) -> isValidSquare x y && (isSquareEmpty x y || isOpponentPiece x y))
//
//     match squareColor with
//     | Some color when color = pieceColor ->
//         let moves =
//             [ (x + forwardOffset, y)
//               (x + 2 * forwardOffset, y) ]
//         moves
//         |> List.filter (fun (x, y) -> isValidSquare x y && isSquareEmpty x y)
//
//         let capturingMoves =
//             generateMoves x y
//             |> List.filter (fun (x, y) -> isOpponentPiece x y)
//
//         let promotionMoves =
//             if (pieceColor = White && x = 7) || (pieceColor = Black && x = 2) then
//                 moves
//                 |> List.map (fun (x, y) -> [(x, y, Queen); (x, y, Rook); (x, y, Bishop); (x, y, Knight)])
//                 |> List.concat
//             else
//                 []
//
//         let enPassantCaptureMoves =
//             match enPassantTarget with
//             | Some (ex, ey) when ex = x && abs(ey - y) = 1 ->
//                 [(x + forwardOffset, ey, None)]
//             | _ -> []
//
//         moves @ capturingMoves @ promotionMoves @ enPassantCaptureMoves
//     | _ -> []
//
// let enPassantTarget = Some (3, 4)
// let pawnMoves = generatePawnMoves board White (2, 3) enPassantTarget
//
// // In this calling function generateAllPawnMoves, we first obtain the coordinates of all pawns on the board that match the given pieceColor. We iterate over each square on the board and filter for pawns of the specified color.
// //
// // Then, we map each pawn's coordinates to a tuple containing the coordinates and the corresponding generated pawn moves using the generatePawnMoves function. The enPassantTarget is passed as a parameter to generatePawnMoves for considering en passant captures.
// //
// // The resulting list allPawnMoves contains tuples where the first element is the pawn's coordinates, and the second element is a list of valid moves for that pawn.
// //
// // Please note that you need to provide the board, pieceColor, and enPassantTarget values appropriate for your specific game state.
// let generateAllPawnMoves (board: Board) (pieceColor: Color) (enPassantTarget: Coordinates option) : (Coordinates * Coordinates list) list =
//     let allPawnCoordinates =
//         [ for i = 1 to 8 do
//             for j = 1 to 8 do
//                 if let (Some piece, Some color) = board.[i, j] then
//                     if piece = Pawn && color = pieceColor then
//                         yield (i, j)
//         ]
//
//     allPawnCoordinates
//     |> List.map (fun coordinates -> (coordinates, generatePawnMoves board pieceColor coordinates enPassantTarget))
//
// let enPassantTarget = Some (3, 4)
// let allPawnMoves = generateAllPawnMoves board White enPassantTarget
//
let generateKnightMoves board gameState (file, rank) : Move list =
    [ (file + 2, rank + 1)
      (file + 2, rank - 1)
      (file - 2, rank + 1)
      (file - 2, rank - 1)
      (file + 1, rank + 2)
      (file + 1, rank - 2)
      (file - 1, rank + 2)
      (file - 1, rank - 2) ]
    |> List.filter isSquareOnBoard
    |> List.filter (isSquareEmptyOrOpponentPiece board gameState.ToPlay)
    |> List.map (fun x -> ((file, rank), x, None))

let generateVectorMoves board gameState (file, rank) directions =

    let generateMoves deltaFile deltaRank : Move list =
        let rec loop newFile newRank (moves: Move list) : Move list =
            let nFile = newFile + deltaFile
            let nRank = newRank + deltaRank

            if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece board gameState.ToPlay (nFile, nRank) then
                let updatedMoves = (convertToMove file rank nFile nRank None) :: moves
                if board[nFile, nRank] = None then
                    loop nFile nRank updatedMoves
                else
                    updatedMoves
            else
                moves

        loop file rank []

    List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions

//
// //  generateKingMoves function, we added the logic for castling moves.
// //
// // The isCastlingAvailable function checks if castling is available for the given king's position.
// //
// // The canCastleShort and canCastleLong variables determine whether castling is possible for the king's current position based on the piece's color and position.
// //
// // The generateMoves function includes the potential moves as before but now also adds the castling moves if they are available. Castling moves are added to the list of potential moves depending on the color and position of the king.
// //
// // The resulting kingMoves list will contain all valid moves for the king, including normal moves and castling moves.
// //
// // Please note that you need to provide the board, pieceColor, and coordinates values suitable for your specific game state.
//
// let generateKingMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) : Coordinates list =
//     let (x, y) = coordinates
//
//     let isValidSquare (x: int, y: int) : bool =
//         x >= 1 && x <= 8 && y >= 1 && y <= 8
//
//     let isSquareEmptyOrOpponentPiece (x: int, y: int) : bool =
//         match board.[x, y] with
//         | (None, _) -> true
//         | (_, Some color) -> color <> pieceColor
//         | _ -> false
//
//     let isCastlingAvailable (x: int, y: int) : bool =
//         match board.[x, y] with
//         | (Some King, Some color) when color = pieceColor ->
//             true
//         | _ ->
//             false
//
//     let canCastleShort =
//         match pieceColor with
//         | White ->
//             x = 1 && y = 5 && isCastlingAvailable 1 8
//         | Black ->
//             x = 8 && y = 5 && isCastlingAvailable 8 8
//
//     let canCastleLong =
//         match pieceColor with
//         | White ->
//             x = 1 && y = 5 && isCastlingAvailable 1 1
//         | Black ->
//             x = 8 && y = 5 && isCastlingAvailable 8 1
//
//     let generateMoves : (int * int) list =
//         let potentialMoves =
//             [ (x + 1, y + 1)
//               (x + 1, y)
//               (x + 1, y - 1)
//               (x, y + 1)
//               (x, y - 1)
//               (x - 1, y + 1)
//               (x - 1, y)
//               (x - 1, y - 1) ]
//
//         let castlingMoves =
//             match pieceColor with
//             | White ->
//                 if canCastleShort then
//                     (1, 7) :: potentialMoves
//                 else
//                     potentialMoves
//                 |> List.filter (fun (x, y) -> isValidSquare x y && isSquareEmptyOrOpponentPiece x y)
//             | Black ->
//                 if canCastleShort then
//                     (8, 7) :: potentialMoves
//                 else
//                     potentialMoves
//                 |> List.filter (fun (x, y) -> isValidSquare x y && isSquareEmptyOrOpponentPiece x y)
//
//         castlingMoves
//
//     generateMoves
//
// let kingMoves = generateKingMoves board White (4, 4)

// Create list of all available moves on the board
let generateMoves (board: Board) (gameState: OtherState) : Move list =
    let mutable availableMoves = []

    for rank = 1 to 8 do
        for file = 1 to 8 do
            match board[file, rank] with
            | Some (piece, color) when color = gameState.ToPlay ->
                let pieceMoves =
                    match piece with
                    | Knight -> generateKnightMoves board gameState (file, rank)
                    | Bishop -> generateVectorMoves board gameState (file, rank) [(1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | Rook -> generateVectorMoves board gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1)]
                    | Queen -> generateVectorMoves board gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | Pawn -> generatePawnMoves board gameState (file, rank)
                    | King -> generateKingMoves board gameState (file, rank)

                availableMoves <- pieceMoves @ availableMoves
            | _ -> ()

    availableMoves

module GenerateMoves

open Types

let isSquareOnBoard (coordinates: int * int) =
    fst coordinates >= 1
    && fst coordinates <= 8
    && snd coordinates >= 1
    && snd coordinates <= 8

let isSquareEmptyOrOpponentPiece (board: Board) movingColor coordinates =
    match board[fst coordinates, snd coordinates] with
    | None -> true
    | Some (_, color) -> color <> movingColor

// the generatePawnMoves function now includes support for en passant captures. The function takes an additional parameter enPassantTarget, which represents the square where an en passant capture is possible.
//
// If the enPassantTarget matches the conditions for an en passant capture (same file and adjacent rank), the function adds the en passant capture move to the resulting list.
//
// The en passant capture move is represented as (x + forwardOffset, ey, None), where x is the pawn's rank, forwardOffset is the direction based on the pawn's color, and ey is the captured pawn's file. The None value represents that no piece is placed on the destination square.
//
// Please ensure that you provide the correct enPassantTarget value based on the game state to generate the valid en passant capture moves.
//
// let generatePawnMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) (enPassantTarget: Coordinates option) : Coordinates list =
//     let (x, y) = coordinates
//     let (_, squareColor) = board.[x, y]
//     let forwardOffset = if pieceColor = White then 1 else -1
//
//     let isValidSquare (x: int, y: int) : bool =
//         x >= 1 && x <= 8 && y >= 1 && y <= 8
//
//     let isSquareEmpty (x: int, y: int) : bool =
//         match board.[x, y] with
//         | (None, _) -> true
//         | _ -> false
//
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
let generateKnightMoves (board: Board) gameState (file, rank) : Move list =
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
    |> List.map (fun x -> ((file, rank), x))

let generateBishopMoves board gameState (file, rank): Move list =
    let directions = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]

    // generate all moves in a direction
    let generateMoves deltaFile deltaRank =
        let rec loop file rank moves =
            let nFile = file + deltaFile
            let nRank = rank + deltaRank

            if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece board gameState.ToPlay (nFile, nRank) then
                let updatedMoves = (nFile, nRank) :: moves
                loop nFile nRank updatedMoves
            else
                moves

        loop file rank []

    List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions
    |> List.map (fun (newSquare) -> ((file, rank), newSquare))

let generateRookMoves board gameState (file, rank): Move list =
    let directions = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

    // generate all moves in a direction
    let generateMoves deltaFile deltaRank =
        let rec loop file rank moves =
            let nFile = file + deltaFile
            let nRank = rank + deltaRank

            if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece board gameState.ToPlay (nFile, nRank) then
                let updatedMoves = (nFile, nRank) :: moves
                if board[nFile, nRank] = None
                then
                    loop nFile nRank updatedMoves
                else
                    updatedMoves
            else
                moves

        loop file rank []

    List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions
    |> List.map (fun (newSquare) -> ((file, rank), newSquare))

// let bishopMoves = generateBishopMoves board White (4, 4)
//
// // In this generateBishopMoves function, we take the board, pieceColor, and coordinates of the bishop as input parameters. The function calculates the possible bishop moves based on the bishop's current position.
// //
// // The isValidSquare function ensures that the generated move is within the chessboard's boundaries. The isSquareEmptyOrOpponentPiece function checks if the destination square is either empty or contains an opponent's piece.
// //
// // The generateMoves function takes the direction values (dx, dy) as input and generates all possible moves in that direction. It uses a recursive loop to continue generating moves along the diagonal until it reaches an invalid square or encounters an occupied square.
// //
// // The directions list contains the four diagonal directions, and List.collect is used to generate moves for each direction and flatten the resulting lists into a single list of moves.
// //
// // The generated bishop moves are returned as a list.
// //
// // Please note that you need to provide the board, pieceColor, and coordinates values suitable for your specific game state.
// let generateBishopMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) : Coordinates list =
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
//     let generateMoves (dx: int) (dy: int) : Coordinates list =
//         let rec loop (x: int) (y: int) (moves: Coordinates list) =
//             let nx = x + dx
//             let ny = y + dy
//
//             if isValidSquare nx ny then
//                 if isSquareEmptyOrOpponentPiece nx ny then
//                     let updatedMoves = (nx, ny) :: moves
//                     loop nx ny updatedMoves
//                 else
//                     moves
//             else
//                 moves
//
//         loop x y []
//
//     let directions =
//         [ (1, 1)
//           (1, -1)
//           (-1, 1)
//           (-1, -1) ]
//
//     directions
//     |> List.collect (fun (dx, dy) -> generateMoves dx dy)
//
// let bishopMoves = generateBishopMoves board White (4, 4)
//
//
// Certainly! Here's a function that generates rook moves for a given rook piece on a chessboard:
//
// fsharp
//
// let generateRookMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) : Coordinates list =
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
//     let generateMoves (dx: int) (dy: int) : Coordinates list =
//         let rec loop (x: int) (y: int) (moves: Coordinates list) =
//             let nx = x + dx
//             let ny = y + dy
//
//             if isValidSquare nx ny then
//                 if isSquareEmptyOrOpponentPiece nx ny then
//                     let updatedMoves = (nx, ny) :: moves
//                     loop nx ny updatedMoves
//                 else
//                     moves
//             else
//                 moves
//
//         loop x y []
//
//     let directions =
//         [ (1, 0)
//           (0, 1)
//           (-1, 0)
//           (0, -1) ]
//
//     directions
//     |> List.collect (fun (dx, dy) -> generateMoves dx dy)
//
// let rookMoves = generateRookMoves board White (4, 4)
//
// // In this generateRookMoves function, we take the board, pieceColor, and coordinates of the rook as input parameters. The function calculates the possible rook moves based on the rook's current position.
// //
// // The isValidSquare function ensures that the generated move is within the chessboard's boundaries. The isSquareEmptyOrOpponentPiece function checks if the destination square is either empty or contains an opponent's piece.
// //
// // The generateMoves function takes the direction values (dx, dy) as input and generates all possible moves in that direction. It uses a recursive loop to continue generating moves along the rank or file until it reaches an invalid square or encounters an occupied square.
// //
// // The directions list contains the four directions (up, right, down, left), and List.collect is used to generate moves for each direction and flatten the resulting lists into a single list of moves.
// //
// // The generated rook moves are returned as a list.
// //
// // Please note that you need to provide the board, pieceColor, and coordinates values suitable for your specific game state.
//
// let generateRookMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) : Coordinates list =
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
//     let generateMoves (dx: int) (dy: int) : Coordinates list =
//         let rec loop (x: int) (y: int) (moves: Coordinates list) =
//             let nx = x + dx
//             let ny = y + dy
//
//             if isValidSquare nx ny then
//                 if isSquareEmptyOrOpponentPiece nx ny then
//                     let updatedMoves = (nx, ny) :: moves
//                     loop nx ny updatedMoves
//                 else
//                     moves
//             else
//                 moves
//
//         loop x y []
//
//     let directions =
//         [ (1, 0)
//           (0, 1)
//           (-1, 0)
//           (0, -1) ]
//
//     directions
//     |> List.collect (fun (dx, dy) -> generateMoves dx dy)
//
// let rookMoves = generateRookMoves board White (4, 4)
//
// let generateQueenMoves (board: Board) (pieceColor: Color) (coordinates: Coordinates) : Coordinates list =
//     let bishopMoves = generateBishopMoves board pieceColor coordinates
//     let rookMoves = generateRookMoves board pieceColor coordinates
//     bishopMoves @ rookMoves
//
// let queenMoves = generateQueenMoves board White (4, 4)
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
            availableMoves <- availableMoves
            @ match board[file, rank] with
              | None -> []
              | Some (piece, color) ->
                  match color with
                  | x when x = gameState.ToPlay ->
                      match piece with
                      | Knight -> generateKnightMoves board gameState (file, rank)
                      | Bishop -> generateBishopMoves board gameState (file, rank)
                      | Rook -> generateRookMoves board gameState (file, rank)
                      | Queen -> generateBishopMoves board gameState (file, rank) @ generateRookMoves board gameState (file, rank)
                      | Pawn -> []
                      | King -> []
                  | _ -> []

    availableMoves

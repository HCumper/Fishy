module Chess

open Types
open GenerateMoves
let board = Array2D.createBased 1 1 8 8 (None)

let setupStartingPosition () =
    // Setting up the white pieces
    board[1, 1] <- Some (Rook, White)
    board[2, 1] <- Some (Knight,White)
    board[3, 1] <- Some (Bishop, White)
    board[4, 1] <- Some (Queen, White)
    board[5, 1] <- Some (King, White)
    board[6, 1] <- Some (Bishop, White)
    board[7, 1] <- Some (Knight, White)
    board[8, 1] <- Some (Rook, White)

    //for file in 1..8 do board[file, 2] <- Some (Pawn, White)

    // Setting up the black pieces
    board[1, 7] <- Some (Rook, Black)
    board[2, 7] <- Some (Knight, Black)
    board[3, 7] <- Some (Bishop, Black)
    board[4, 7] <- Some (Queen, Black)
    board[5, 7] <- Some (King, Black)
    board[6, 7] <- Some (Bishop, Black)
    board[7, 7] <- Some (Knight, Black)
    board[8, 7] <- Some (Rook, Black)

    for file in 1..8 do
        board[file, 8] <- Some (Pawn, Black)

let startState = { WhiteKingMoved = false; WhiteQRMoved = false; WhiteKRMoved = false; BlackKingMoved = false; BlackQRMoved = false; BlackKRMoved = false; EPSquare = None; JustCaptured = false; PromoteTo = None; ToPlay = White }
setupStartingPosition ()
let availableMoves = generateMoves board startState


// let getPawnMoves board color : Coordinates list =
//     let mutable legalMoves = []
//
//     let pawnColor = piece.Color
//     let toSquare = if pawnColor = White then fromSquare.North else fromSquare.South
//
//     let addMoveIfLegal toSq =
//         if isSquareOnBoard toSq then
//             match getPieceAtSquare board toSq with
//             | Some target when target.Color <> pawnColor -> legalMoves <- toSq :: legalMoves
//             | _ -> ()
//
//     if isEmptySquare board toSquare then
//         legalMoves <- toSquare :: legalMoves
//
//         if (pawnColor = White && fromSquare.Row = 2) || (pawnColor = Black && fromSquare.Row = 7) then
//             let doubleJumpToSq = if pawnColor = White then toSquare.North else toSquare.South
//             if isEmptySquare board doubleJumpToSq then
//                 legalMoves <- doubleJumpToSq :: legalMoves
//
//         let leftCaptureToSq = toSquare.West
//         let rightCaptureToSq = toSquare.East
//         addMoveIfLegal leftCaptureToSq
//         addMoveIfLegal rightCaptureToSq
//
//         // Check for en passant captures
//         let lastMove = board.LastMove
//         match lastMove with
//         | Some (prevSq, enPassantTarget) when enPassantTarget = toSquare ->
//             let leftEnPassantSq = toSquare.West.North
//             let rightEnPassantSq = toSquare.East.North
//             if pawnColor = White then
//                 addMoveIfLegal leftEnPassantSq
//                 addMoveIfLegal rightEnPassantSq
//             else
//                 addMoveIfLegal leftEnPassantSq.South
//                 addMoveIfLegal rightEnPassantSq.South
//         | _ -> ()
//
//         legalMoves
//
// let getKnightMoves (position: ChessPosition) : ChessMove list =
//     let x, y = position
//     let moves = [
//         (x + 1, y + 2); (x + 1, y - 2);
//         (x - 1, y + 2); (x - 1, y - 2);
//         (x + 2, y + 1); (x + 2, y - 1);
//         (x - 2, y + 1); (x - 2, y - 1)
//     ]
//     moves
//     |> List.choose (fun (x, y) ->
//         if isValidSquare (x, y) then Some (position, (x, y)) else None)
//     |> List.map (fun move -> Move(Knight, move))
// let getBishopMoves (board: ChessBoard) (position: ChessPosition) : ChessMove list =
//     let mutable validMoves = []
//     let mutable current = {rank = position.rank+1; file = position.file+1}
//
//     // Check the diagonals to the bottom right
//     while current.rank <= 8 && current.file <= 'h' do
//         match board.[current.rank].[current.file] with
//         | Empty -> validMoves <- {from=position; toPos=current}::validMoves
//                    current <- {rank = current.rank+1; file = char (int current.file + 1)}
//         | Piece(pieceColor, _) when pieceColor <> board.[position.rank].[position.file].color ->
//                    validMoves <- {from=position; toPos=current}::validMoves
//                    break
//         | _ -> break
//
//     current <- {rank = position.rank+1; file = position.file-1}
//     // Check the diagonals to the bottom left
//     while current.rank <= 8 && current.file >= 'a' do
//         match board.[current.rank].[current.file] with
//         | Empty -> validMoves <- {from=position; toPos=current}::validMoves
//                    current <- {rank = current.rank+1; file = char (int current.file - 1)}
//         | Piece(pieceColor, _) when pieceColor <> board.[position.rank].[position.file].color ->
//                    validMoves <- {from=position; toPos=current}::validMoves
//                    break
//         | _ -> break
//
//     current <- {rank = position.rank-1; file = position.file+1}
//     // Check the diagonals to the top right
//     while current.rank >= 1 && current.file <= 'h' do
//         match board.[current.rank].[current.file] with
//         | Empty -> validMoves <- {from=position; toPos=current}::validMoves
//                    current <- {rank = current.rank-1; file = char (int current.file + 1)}
//         | Piece(pieceColor, _) when pieceColor <> board.[position.rank].[position.file].color ->
//                    validMoves <- {from=position; toPos=current}::validMoves
//                    break
//         | _ -> break
//
//     current <- {rank = position.rank-1; file = position.file-1}
//     // Check the diagonals to the top left
//     while current.rank >= 1 && current.file >= 'a' do
//         match board.[current.rank].[current.file] with
//         | Empty -> validMoves <- {from=position; toPos=current}::validMoves
//                    current <- {rank = current.rank-1; file = char (int current.file - 1)}
//         | Piece(pieceColor, _) when pieceColor <> board.[position.rank].[position.file].color ->
//                    validMoves <- {from=position; toPos=current}::validMoves
//                    break
//         | _ -> break
//
//     validMoves
//
// let getRookMoves (position: Position) (color: Color) (row: int) (col: int) (board: Piece list list) : Move list =
//     let mutable moves = []
//
//     let mutable rowInc = 1
//     while (row + rowInc) < 8 do
//         match board.[row + rowInc].[col] with
//         | Empty -> moves <- (Move(position, (row, col), (row + rowInc, col), None) :: moves)
//                    rowInc <- rowInc + 1
//         | Occupied (pColor, _) -> if pColor <> color then moves <- (Move(position, (row, col), (row + rowInc, col), Some OccupiedSquare) :: moves)
//                                              break
//                                  else break
//
//     let mutable rowDec = -1
//     while (row + rowDec) >= 0 do
//         match board.[row + rowDec].[col] with
//         | Empty -> moves <- (Move(position, (row, col), (row + rowDec, col), None) :: moves)
//                    rowDec <- rowDec - 1
//         | Occupied (pColor, _) -> if pColor <> color then moves <- (Move(position, (row, col), (row + rowDec, col), Some OccupiedSquare) :: moves)
//                                              break
//                                  else break
//
//     let mutable colInc = 1
//     while (col + colInc) < 8 do
//         match board.[row].[col + colInc] with
//         | Empty -> moves <- (Move(position, (row, col), (row, col + colInc), None) :: moves)
//                    colInc <- colInc + 1
//         | Occupied (pColor, _) -> if pColor <> color then moves <- (Move(position, (row, col), (row, col + colInc), Some OccupiedSquare) :: moves)
//                                              break
//                                  else break
//
//     let mutable colDec = -1
//     while (col + colDec) >= 0 do
//         match board.[row].[col + colDec] with
//         | Empty -> moves <- (Move(position, (row, col), (row, col + colDec), None) :: moves)
//                    colDec <- colDec - 1
//         | Occupied (pColor, _) -> if pColor <> color then moves <- (Move(position, (row, col), (row, col + colDec), Some OccupiedSquare) :: moves)
//                                              break
//                                  else break
//
//     moves
//
// let getQueenMoves (x:int,y:int) (board:Piece array array) : (int*int) list =
//     let bishopMoves = getBishopMoves (x,y) board
//     let rookMoves = getRookMoves (x,y) board
//     bishopMoves @ rookMoves
//
// let issafe (board: Board) (pos: Position) (player: Player) : bool =
//     let oppPlayer = Player.Opponent player
//
//     // Check for pawn attacks
//     let pawnAttackPos =
//         match player with
//         | Player.White -> [pos.DownLeft; pos.DownRight]
//         | Player.Black -> [pos.UpLeft; pos.UpRight]
//
//     let pawnAttacks =
//         List.filter (fun p -> Board.IsOnBoard p && board.[p] = Piece.Pawn oppPlayer) pawnAttackPos
//
//     if pawnAttacks.Length > 0 then
//         false
//     else
//         // Check for knight attacks
//         let knightAttackPos = pos.KnightMoves |> List.filter Board.IsOnBoard
//         let knightAttacks =
//             List.filter (fun p -> board.[p] = Piece.Knight oppPlayer) knightAttackPos
//
//         if knightAttacks.Length > 0 then
//             false
//         else
//             // Check for bishop/queen attacks along diagonals
//             let diagonalAttackPos = pos.DiagonalMoves |> List.filter Board.IsOnBoard
//             let bishopQueenAttacks =
//                 diagonalAttackPos
//                 |> List.tryFind (fun p ->
//                     let piece = board.[p]
//                     piece = Piece.Bishop oppPlayer || piece = Piece.Queen oppPlayer
//                 )
//
//             if Option.isSome bishopQueenAttacks then
//                 false
//             else
//                 // Check for rook/queen attacks along rows and columns
//                 let rowColAttackPos = pos.RowColMoves |> List.filter Board.IsOnBoard
//                 let rookQueenAttacks =
//                     rowColAttackPos
//                     |> List.tryFind (fun p ->
//                         let piece = board.[p]
//                         piece = Piece.Rook oppPlayer || piece = Piece.Queen oppPlayer
//                     )
//
//                 if Option.isSome rookQueenAttacks then
//                     false
//                 else
//                     // Check for king attacks
//                     let kingAttackPos = pos.KingMoves |> List.filter Board.IsOnBoard
//                     let kingAttacks =
//                         List.filter (fun p -> board.[p] = Piece.King oppPlayer) kingAttackPos
//
//                     kingAttacks.Length = 0
//
// let getKingMoves (board: ChessBoard) (square: Square) : Move list =
//     let king = board.[square.Row, square.Column]
//     let color = king.Color
//     let opponentColor = match color with White -> Black | Black -> White
//     let opponentAttacks = getAttacks board opponentColor
//
//     let validKingMove (move: Move) : bool =
//         let target = board.[move.To.Row, move.To.Column]
//         isSafe board square move.To && target.IsEmptyOrCapturableBy color
//
//     let kingMoves = [
//         for i in -1..1 do
//             for j in -1..1 do
//                 let newRow = square.Row + i
//                 let newColumn = square.Column + j
//                 if isValidSquare newRow newColumn then
//                     let toSquare = {Row=newRow; Column=newColumn}
//                     let move = {From=square; To=toSquare; Promotion=None}
//                     if validKingMove move then yield move
//
//         // Castling
//         if king.HasMoved = false && isSafe board square square then
//             let rookColumn = if king.Color = White then 7 else 0
//             let rook = board.[square.Row, rookColumn]
//             if rook.Piece = Rook && rook.HasMoved = false &&
//                not (isAttackedBy opponentAttacks {Row=square.Row; Column=square.Column+1}) &&
//                not (isAttackedBy opponentAttacks {Row=square.Row; Column=square.Column+2}) then
//                 let kingSide = {Row=square.Row; Column=square.Column+2}
//                 let queenSide = {Row=square.Row; Column=square.Column-2}
//                 let kingSideEmpty = board.[square.Row, square.Column+1].IsEmpty &&
//                                     board.[square.Row, square.Column+2].IsEmpty
//                 let queenSideEmpty = board.[square.Row, square.Column-1].IsEmpty &&
//                                      board.[square.Row, square.Column-2].IsEmpty &&
//                                      board.[square.Row, square.Column-3].IsEmpty
//                 if kingSideEmpty && isSafe board square kingSide then
//                     yield {From=square; To=kingSide; Promotion=None; IsCastle=true}
//                 if queenSideEmpty && isSafe board square queenSide then
//                     yield {From=square; To=queenSide; Promotion=None; IsCastle=true}
//     ]
//     kingMoves
//
// let getMoves (position, toMove) =
//     let moves = []
//
//     // Loop over each square on the board
//     for i = 0 to 7 do
//         for j = 0 to 7 do
//             match position.[i].[j] with
//             | Some(piece, isWhite) when isWhite = toMove ->
//                 // Generate moves for the piece at this square
//                 let pieceMoves = match piece with
//                                  | Pawn -> getPawnMoves (i, j) position toMove
//                                  | Knight -> getKnightMoves (i, j) position toMove
//                                  | Bishop -> getBishopMoves (i, j) position toMove
//                                  | Rook -> getRookMoves (i, j) position toMove
//                                  | Queen -> getQueenMoves (i, j) position toMove
//                                  | King -> getKingMoves (i, j) position toMove
//
//                 // Add the generated moves to the list of all moves
//                 moves <- moves @ pieceMoves
//             | _ -> ()
//
//     // Return the list of all generated moves
//     moves
//
//     (*
//         negascout algorithm implementation using alpha-beta pruning.
//         Takes as input:
//         - depth: the current depth in the game tree (int)
//         - alpha: the best score found so far for MAX (int)
//         - beta: the best score found so far for MIN (int)
//         - is_max: a boolean indicating if the current player is MAX or not (bool)
//         - eval: a function that evaluates a game state and returns a score (a -> int)
//         - gen_moves: a function that generates all legal moves from a game state (a -> 'a list)
//         - pos: the current game state (a)
//         Returns:
//         - the best score found for the current player (int)
//     *)
// let rec negascout (depth: int) (alpha: int) (beta: int) (is_max: bool) (eval: 'a -> int) (gen_moves: 'a -> 'a list) (pos: 'a): int =
//     // If we've reached the desired depth, evaluate the position and return the result
//     if depth = 0 then
//         eval pos
//     else
//         // Initialize the mutable variables to their starting values
//         let mutable a = alpha
//         let mutable b = beta
//         let mutable best_score = Int32.MinValue
//         let mutable first_child = true
//         // Loop through each move and evaluate its score
//         for move in gen_moves pos do
//             // Recursively call negascout on the next depth level, updating the alpha-beta window as necessary
//             let score = -negascout (depth - 1) (-b) (-a) (not is_max) eval gen_moves move
//             // If this is the first child, set its score as the best score
//             if first_child then
//                 best_score <- score
//                 first_child <- false
//             // If the score is within the alpha-beta window and not at the bottom of the tree, do a re-search
//             else if score > a && score < b && not is_max && depth > 1 then
//                 best_score <- -negascout (depth - 1) (-b) (-score) (not is_max) eval gen_moves move
//             // Update the best score if this move has a higher score than the current best score
//             if score > best_score then
//                 best_score <- score
//             // Update the alpha value if the best score is greater than the current alpha value
//             if best_score > a then
//                 a <- best_score
//             // Break out of the loop if the alpha value is greater than or equal to the beta value
//             if a >= b then
//                 break
//         best_score

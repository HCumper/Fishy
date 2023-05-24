module Chess

open Types
open GenerateMoves
let board = Array2D.createBased 1 1 8 8 None

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

    for file in 1..8 do board[file, 2] <- Some (Pawn, White)

    // Setting up the black pieces
    board[1, 8] <- Some (Rook, Black)
    board[2, 8] <- Some (Knight, Black)
    board[3, 8] <- Some (Bishop, Black)
    board[4, 8] <- Some (Queen, Black)
    board[5, 8] <- Some (King, Black)
    board[6, 8] <- Some (Bishop, Black)
    board[7, 8] <- Some (Knight, Black)
    board[8, 8] <- Some (Rook, Black)

    for file in 1..8 do
        board[file, 7] <- Some (Pawn, Black)

let mutable currentState = { WhiteKingMoved = false; WhiteQRMoved = false; WhiteKRMoved = false; BlackKingMoved = false; BlackQRMoved = false; BlackKRMoved = false; EPSquare = None; PromoteTo = None; ToPlay = White }

//setupStartingPosition ()
let availableMoves = generateMoves board currentState

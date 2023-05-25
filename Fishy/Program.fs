module Chess

open Types
open GenerateMoves

// This is a global variable on which moves are made and unmade
let mutable (board: (Piece * Color) option[,]) = Array2D.createBased 1 1 8 8 None

// current state is passed around and coninually copied and destroyed
// let mutable currentState = { WhiteKingMoved = false; WhiteQRMoved = false; WhiteKRMoved = false; BlackKingMoved = false; BlackQRMoved = false; BlackKRMoved = false; EPSquare = None; PromoteTo = None; ToPlay = White }

let availableMoves () = generateMoves board currentState

let engineMove () = availableMoves ()

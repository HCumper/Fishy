module Chess

open Types
open GenerateMoves
let board = Array2D.createBased 1 1 8 8 None

let mutable currentState = { WhiteKingMoved = false; WhiteQRMoved = false; WhiteKRMoved = false; BlackKingMoved = false; BlackQRMoved = false; BlackKRMoved = false; EPSquare = None; PromoteTo = None; ToPlay = White }

let availableMoves () = generateMoves board currentState

let engineMove () = availableMoves ()

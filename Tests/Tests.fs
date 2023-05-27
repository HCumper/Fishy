module Tests
open Fishy
open Types
open  FENParser

//[<EntryPoint>]
//setupStartingPosition ()
//let moves = GenerateMoves.generateMoves board startState
parseFEN "rnbqkb1r/pp2pp1p/3p1np1/8/3NP3/2N5/PPP2PPP/R1BQKB1R w KQkq - 0 6" |> ignore
//let newFENString = boardToFen globalBoard
()

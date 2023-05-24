module Tests
open Chess

[<EntryPoint>]
setupStartingPosition ()
let moves = GenerateMoves.generateMoves board startState
()

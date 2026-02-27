module Fishy

open Types
open Fen

[<EntryPoint>]
let main (_argv: string[]) = 
    let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    let board : Board = Array2D.create 8 8 0y

    match tryLoadPositionFromFen board startFen with
    | ValueSome position ->
        printfn "Ready"
        0
    | ValueNone ->
        printfn "Failed to parse FEN."
        1
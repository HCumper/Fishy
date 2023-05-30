module LookAhead

open Types
open System
open System.Diagnostics
open GenerateMoves
open Evaluation
open MakeMove
open UCILogger
open Transpositions
open Fishy

let mutable repNodes = 0
let mutable repStopwatch = Stopwatch.StartNew()
let mutable repMainLine = []
let mutable repTopLevelBestValue = 0
let mutable repDepth = 0
let mutable repCurrMove = defaultMove
let mutable repMoveNumber = 0

// The heart of the matter
let rec negascout board otherState depthLeft currentDepth alpha beta : int * Move list =

    repDepth <- currentDepth
    repNodes <- repNodes+1
    if depthLeft = 0 then
        ((int otherState.ToPlay) * (evaluate board otherState), [])
    else
        let mutable bestValue = -2000000000
        let mutable isFirstChild = true
        let mutable betaCutoff = false
        let mutable chosenMoves = []

        repMoveNumber <- 0
        for move in generateMoves board otherState do
            repMoveNumber <- repMoveNumber + 1
            if currentDepth = 1 then repCurrMove <- move

            if not betaCutoff then

                let newBoard, newState = makeMove (Array2D.copy board) otherState move

                let score, remainingMoves =
                    match transpositionTableLookup newBoard newState depthLeft with
                    | None ->
    //                if isFirstChild then
                        // First child search with full window
                        negascout newBoard newState (depthLeft - 1) (currentDepth + 1) -beta -alpha
    //                else
    //                    // Null-window search
    //                    let score = -(negascout board otherState (depthLeft - 1) (-alpha - 1) -alpha -color)

    //                    if alpha < score && score < beta && depthLeft > 1 then
    //                        // Perform a re-search with a full window
    //                        -(negascout board otherState (depthLeft - 1) -beta -score -color)
    //                    else
    //                        score
                    | Some (value, _) -> value, []

                if -score > bestValue then // this move is the best so far
                    bestValue <- -score
                    repMainLine <- (List.take (min (currentDepth-1) repMainLine.Length) repMainLine) @ [move] @ (List.rev chosenMoves) // for reporting only
                    chosenMoves <- remainingMoves @ [move]
                    if currentDepth = 1 then repTopLevelBestValue <- bestValue

                if (max alpha -score) >= beta then
                    // Beta cutoff, prune remaining moves
                    betaCutoff <- true

//                if depthLeft >= 2 then insertIntoTranspositionTable newBoard newState score depthLeft

                isFirstChild <- false

        bestValue, chosenMoves

let chooseEngineMove board level currentState =
    repStopwatch.Reset()
    repStopwatch.Start()
    repNodes <- 0
    resetTranspositionTable ()
    repMainLine <- []
    let result = negascout board currentState level 1 -2000000000 2000000000
    result

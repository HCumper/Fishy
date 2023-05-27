module LookAhead

open Types
open System
open System.Diagnostics
open GenerateMoves
open Evaluation
open MakeMove
open UCILogger

let mutable nodes = 0
let mutable stopwatch = Stopwatch.StartNew()

// The heart of the matter
let rec negascout board otherState depthLeft currentDepth alpha beta (bestSoFar: Move list) (color: SByte) : int * Move list =

    nodes <- nodes+1
    if depthLeft = 0 then
        ((int color) * (evaluate board otherState), [])
    else
        let mutable bestValue = -2000000000
        let mutable isFirstChild = true
        let mutable betaCutoff = false
        let mutable chosenMoves = []

        for move in generateMoves board otherState do
            if not betaCutoff then

                let newBoard, newState = makeMove (Array2D.copy board) otherState move

                let score, mainLine =
    //                if isFirstChild then
                        // First child search with full window
                        negascout newBoard newState (depthLeft - 1) (currentDepth + 1) -beta -alpha bestSoFar -color
    //                else
    //                    // Null-window search
    //                    let score = -(negascout board otherState (depthLeft - 1) (-alpha - 1) -alpha -color)

    //                    if alpha < score && score < beta && depthLeft > 1 then
    //                        // Perform a re-search with a full window
    //                        -(negascout board otherState (depthLeft - 1) -beta -score -color)
    //                    else
    //                        score

                if currentDepth = 1 then
                    writeInfo (bestValue / 10) (depthLeft+1) nodes stopwatch.ElapsedMilliseconds move (bestSoFar @ chosenMoves)

                if -score > bestValue then
                    bestValue <- -score
                    chosenMoves <- mainLine @ [move]

                if (max alpha -score) >= beta then
                    // Beta cutoff, prune remaining moves
                    betaCutoff <- true

                isFirstChild <- false

        bestValue, bestSoFar @ chosenMoves

let chooseEngineMove board level currentState =
    stopwatch.Reset()
    stopwatch.Start()
    nodes <- 0
    negascout board currentState level 1 -2000000000 2000000000 [] currentState.ToPlay

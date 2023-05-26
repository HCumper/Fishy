module LookAhead

open Types
open System
open GenerateMoves
open Evaluation
open Fishy
open MakeMove

// The heart of the matter
let rec negascout board otherState depthLeft alpha beta (bestSoFar: Move list) (color: SByte) : int * Move list =

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
                        negascout newBoard newState (depthLeft - 1) -beta -alpha bestSoFar -color
    //                else
    //                    // Null-window search
    //                    let score = -(negascout board otherState (depthLeft - 1) (-alpha - 1) -alpha -color)

    //                    if alpha < score && score < beta && depthLeft > 1 then
    //                        // Perform a re-search with a full window
    //                        -(negascout board otherState (depthLeft - 1) -beta -score -color)
    //                    else
    //                        score

                if -score > bestValue then
                    bestValue <- -score
                    chosenMoves <- mainLine @ [move]

                if (max alpha -score) >= beta then
                    // Beta cutoff, prune remaining moves
                    betaCutoff <- true

                isFirstChild <- false

        bestValue, bestSoFar @ chosenMoves

let chooseEngineMove level =
    negascout currentBoard currentState level -2000000000 2000000000 [] White

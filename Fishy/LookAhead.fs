module LookAhead

open System
open GenerateMoves
type GameNode = { Value: int; Children: GameNode list }


type Board = int[,]

let rec negascout board otherState depthLeft alpha beta (color: int) =

    if depthLeft = 0 then
        evaluateBoard board otherState

    let mutable bestValue = Int32.MinValue
    let mutable isFirstChild = true
    let mutable break = false

    for move in generateMoves board otherState do
        if not break then
            makeMove (board, move)

            let score =
                if isFirstChild then
                    // First child search with full window
                    -(negascout board otherState (depthLeft - 1) -beta -alpha -color)
                else
                    // Null-window search with a reduced window
                    let score = -(negascout board otherState (depthLeft - 1) (-alpha - 1) -alpha -color)

                    if alpha < score && score < beta && depthLeft > 1 then
                        // Perform a re-search with a full window
                        -(negascout board otherState (depthLeft - 1) -beta -score -color)
                    else
                        score

            undoMove (board, move)

            bestValue <- max bestValue score

            if (max alpha score) >= beta then
                // Beta cutoff, prune remaining moves
                break <- true

            isFirstChild <- false

    bestValue

module Transpositions

    open System.Collections.Generic

    let transpositionTable = Dictionary<int64, int>()

    let resetTranspositionTable = transpositionTable.Clear ()

    let insertIntoTranspositionTable board state score =
        let hashKey = Zobrist.initializePositionHash board state
        transpositionTable.Remove hashKey |> ignore
        transpositionTable.Add(hashKey, score)
        ()

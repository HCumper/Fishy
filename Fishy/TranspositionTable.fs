module Transpositions

    open System.Collections.Generic
    open Zobrist

    let transpositionTable = Dictionary<int64, int>()

    let mutable cacheHits = 0
    let mutable cacheMisses = 0
    let resetTranspositionTable = transpositionTable.Clear ()

    let insertIntoTranspositionTable board state score =
        let hashKey = initializePositionHash board state
        transpositionTable.Remove hashKey |> ignore
        transpositionTable.Add(hashKey, score)
        ()

    let transpositionTableLookup board state =
        let hashKey = initializePositionHash board state
        let mutable value = 0
        match transpositionTable.TryGetValue(hashKey, &value) with
        | true ->
            cacheHits <- cacheHits + 1
            Some value
        | false ->
            cacheMisses <- cacheMisses + 1
            None

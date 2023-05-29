module Transpositions

    open System.Collections.Generic
    open Zobrist

    let transpositionTable = Dictionary<int64, (int * int)>()

    let mutable cacheHits = 0
    let mutable cacheMisses = 0
    let resetTranspositionTable () = transpositionTable.Clear ()

    let insertIntoTranspositionTable board state score confidence =
        let hashKey = initializePositionHash board state
        transpositionTable.Remove hashKey |> ignore
        transpositionTable.Add(hashKey, (score, confidence))
        ()

    let transpositionTableLookup board state confidenceRequired : (int * int) option =
        None
        // let hashKey = initializePositionHash board state
        // let mutable value = 0, 0
        // match transpositionTable.TryGetValue(hashKey, &value) with
        // | outcome when outcome = true && (snd value >= confidenceRequired) ->
        //     cacheHits <- cacheHits + 1
        //     Some ((fst value), 1)
        // | _ ->
        //     cacheMisses <- cacheMisses + 1
        //     None

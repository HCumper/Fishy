module TranspositionTable

    open System.Collections.Generic
    open Zobrist
    open Types
    open Fishy

    type Key = int64
    type Score = int
    type Confidence = int

    let transpositionTable = Dictionary<Key, Score * Confidence * Move> ()

    let mutable repCacheHits = 0
    let mutable repCacheMisses = 0
    let resetTranspositionTable () = transpositionTable.Clear ()

    let insertIntoTranspositionTable board state score confidence move =
        let hashKey = hashAPosition board state
        let mutable value = 0, 0, defaultMove
        match transpositionTable.TryGetValue(hashKey, &value) with
        | outcome when outcome = true ->
            let _, oldConfidence, _ = value
            if oldConfidence < confidence then
                transpositionTable.Remove hashKey |> ignore
                transpositionTable.Add(hashKey, (score, confidence, move))
        | _ ->
            transpositionTable.Add(hashKey, (score, confidence, move))
        ()

    let transpositionTableLookupByBoard board state age : (Score * Confidence * Move) option =
        let hashKey = hashAPosition board state
        let mutable value = 0, 0, defaultMove
        match transpositionTable.TryGetValue(hashKey, &value) with
        | outcome when outcome = true ->
            repCacheHits <- repCacheHits + 1
            Some value
        | _ ->
            repCacheMisses <- repCacheMisses + 1
            None

    let transpositionTableLookupByHash hashKey : (Score * Confidence * Move) option =
        let mutable value = 0, 0, defaultMove
        match transpositionTable.TryGetValue(hashKey, &value) with
        | outcome when outcome = true ->
            repCacheHits <- repCacheHits + 1
            Some value
        | _ ->
            repCacheMisses <- repCacheMisses + 1
            None

module TranspositionTable

// open System.Collections.Generic
// open Zobrist
// open Types
// open Fishy
//
// type Key = int64
// type Score = int
// type Confidence = int
//
// let transpositionTable = Dictionary<Key, (Score * Confidence * Move)>()
//
// let mutable repCacheHits = 0
// let mutable repCacheMisses = 0
//
// let resetTranspositionTable () = transpositionTable.Clear()
//
// let insertIntoTranspositionTable board state score confidence move =
//     let hashKey = hashAPosition board state
//     match transpositionTable.TryGetValue(hashKey) with
//     | true, (_, oldConfidence, _) when oldConfidence < confidence ->
//         transpositionTable.[hashKey] <- (score, confidence, move)
//     | false, _ ->
//         transpositionTable.Add(hashKey, (score, confidence, move))
//     | _ -> ()
//
// let transpositionTableLookupByBoard board state age : (Score * Confidence * Move) option =
//     let hashKey = hashAPosition board state
//     match transpositionTable.TryGetValue(hashKey) with
//     | true, value ->
//         repCacheHits <- repCacheHits + 1
//         Some value
//     | false, _ ->
//         repCacheMisses <- repCacheMisses + 1
//         None
//
// let transpositionTableLookupByHash hashKey : (Score * Confidence * Move) option =
//     match transpositionTable.TryGetValue(hashKey) with
//     | true, value ->
//         repCacheHits <- repCacheHits + 1
//         Some value
//     | false, _ ->
//         repCacheMisses <- repCacheMisses + 1
//         None
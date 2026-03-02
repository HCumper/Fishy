module Zobrist

open Types
open BoardHelpers.CastlingRights
open BoardHelpers.PieceCode

// This module hashes:
//   - 12 piece types (6 white + 6 black) on 64 squares
//   - 4 castling rights (WK, WQ, BK, BQ)
//   - en-passant file (0..7)
//   - side to move (black-to-move key)
//
// Coordinates are assumed 0-based everywhere: file/rank in 0..7.

// ---------- 64-bit deterministic RNG (SplitMix64) ----------
type private SplitMix64(seed0:uint64) =
    let mutable x = seed0
    member _.NextUInt64() =
        x <- x + 0x9E3779B97F4A7C15UL
        let mutable z = x
        z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        z ^^^ (z >>> 31)
    member this.NextInt64() = int64 (this.NextUInt64())

// ---------- Key tables ----------
let private pieceKeys : int64[,,] = Array3D.zeroCreate 12 8 8
let private castleKeys : int64[] = Array.zeroCreate 4
let private epFileKeys : int64[] = Array.zeroCreate 8

let private blackToMoveKey : int64 =
    // independent deterministic seed
    let rng = SplitMix64(0xDEADBEEFCAFEBABEUL)
    rng.NextInt64()

// Initialize keys deterministically.
// Change the seed if you want a different key set, but keep it constant for reproducible debugging.
do
    let rng = SplitMix64(0xC0FFEE1234ABCDEFUL)

    for pi = 0 to 11 do
        for f = 0 to 7 do
            for r = 0 to 7 do
                pieceKeys[pi, f, r] <- rng.NextInt64()

    for i = 0 to 3 do
        castleKeys[i] <- rng.NextInt64()

    for f = 0 to 7 do
        epFileKeys[f] <- rng.NextInt64()

// ---------- Piece indexing ----------
// Returns Some 0..11 for a piece on board, None for empty or invalid.
// Index layout:
//   0..5  = white pawn..king (by absolute piece code - 1)
//   6..11 = black pawn..king
let inline private pieceIndex (p:sbyte) : int option =
    if p = Empty then None
    else
        let a = int (abs p) // expected 1..6
        if a < 1 || a > 6 then None
        else
            let baseIdx = if p > 0y then 0 else 6
            Some (baseIdx + (a - 1))

// ---------- Hash toggles (XOR in/out) ----------
let togglePiece (p:sbyte) (file:int) (rank:int) (hash:int64) : int64 =
    match pieceIndex p with
    | None -> hash
    | Some idx -> hash ^^^ pieceKeys[idx, file, rank]

let toggleCastleWK (hash:int64) = hash ^^^ castleKeys[0]
let toggleCastleWQ (hash:int64) = hash ^^^ castleKeys[1]
let toggleCastleBK (hash:int64) = hash ^^^ castleKeys[2]
let toggleCastleBQ (hash:int64) = hash ^^^ castleKeys[3]

let toggleEpFile (file:int) (hash:int64) =
    // file must be 0..7
    hash ^^^ epFileKeys[file]

let toggleBlackToMove (hash:int64) =
    hash ^^^ blackToMoveKey

// ---------- EP legality policy ----------
// For strict threefold-repetition semantics, many engines only hash EP when an EP capture is actually legal.
// If you don't want that complexity yet, keep this always true and hash EP whenever EPSquare is set.
let epIsRelevant (_board:Board) (_gs:GameState) : bool =
    true

// ---------- Full recomputation (reference hash) ----------
let hashPosition (board:Board) (gs:GameState) : int64 =
    let mutable h = 0L

    for rank = 0 to 7 do
        for file = 0 to 7 do
            let p = board.[file, rank]
            if p <> Empty then
                h <- togglePiece p file rank h

    if gs.CastlingRights &&& WK <> 0uy then h <- toggleCastleWK h
    if gs.CastlingRights &&& WQ <> 0uy then h <- toggleCastleWQ h
    if gs.CastlingRights &&& BK <> 0uy then h <- toggleCastleBK h
    if gs.CastlingRights &&& BQ <> 0uy then h <- toggleCastleBQ h

    match gs.EPSquare with
    | ValueSome { File = epFile; Rank = _ } when epIsRelevant board gs ->
        h <- toggleEpFile (int epFile) h
    | _ ->
        ()

    if gs.ToPlay = Color.Black then
        h <- toggleBlackToMove h

    h
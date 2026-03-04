module TranspositionTable

open System
open System.Runtime.InteropServices

// ============================================================
// Entry (16 bytes)
// ============================================================

[<Literal>]
let BoundExact = 0uy
[<Literal>]
let BoundLower = 1uy
[<Literal>]
let BoundUpper = 2uy

/// Compact 16-byte TT entry using a 32-bit lock (partial key).
/// Typical: store (lock = upper 32 bits of zobrist) and index by lower bits.
///
/// Size (Pack=1) = 16 bytes exactly:
///   Lock32(4) + Move(4) + Score(2) + Eval(2) + Depth(1) + Bound(1) + Gen(1) + Flags(1)
///
/// Conventions:
/// - Lock32: (uint32)(key >>> 32)
/// - Depth: ply depth stored as sbyte (0..127 is plenty for practical search depths).
/// - Bound: 0=Exact, 1=Lower, 2=Upper
/// - Flags: optional (e.g., PV flag, mate-score flag, etc.). Can be 0 if unused.
[<Struct; StructLayout(LayoutKind.Sequential, Pack=1)>]
type TTEntry =
    val mutable Lock32     : uint32
    val mutable Move       : int32
    val mutable Score      : int16
    val mutable Eval       : int16
    val mutable Depth      : sbyte
    val mutable Bound      : byte
    val mutable Generation : byte
    val mutable Flags      : byte

    new (lock32, mv, score, eval, depth, bound, generation, flags) =
        { Lock32 = lock32
          Move = mv
          Score = score
          Eval = eval
          Depth = depth
          Bound = bound
          Generation = generation
          Flags = flags }

let inline lock32OfKey (key:uint64) : uint32 =
    uint32 (key >>> 32)

// Index uses low bits of the 64-bit key
let inline indexOfKey (mask:int) (key:uint64) : int =
    int (key &&& uint64 mask)

// ============================================================
// Replace policy helpers
// ============================================================

let inline private unsignedByteDiff (a:byte) (b:byte) : int =
    // generation is a byte that wraps; difference in 0..255
    int ((uint32 a - uint32 b) &&& 0xFFu)

let inline private isStale (currentGen:byte) (entryGen:byte) (maxAge:int) : bool =
    unsignedByteDiff currentGen entryGen > maxAge

// Prefer deeper; else prefer newer; else overwrite
let inline private shouldReplace (currentGen:byte) (maxAge:int) (newDepth:sbyte) (old:TTEntry) : bool =
    if old.Lock32 = 0u then
        true
    else
        let oldDepth = int old.Depth
        let nd = int newDepth

        if nd > oldDepth then true
        elif nd < oldDepth then false
        else
            // same depth: replace stale entries, else replace if older gen
            if isStale currentGen old.Generation maxAge then true
            else unsignedByteDiff currentGen old.Generation > 0

// ============================================================
// TT container
// ============================================================

type TTConfig =
    { Mb: int
      BucketSize: int
      MaxAge: int }     // generations before "stale"

type TranspositionTable =
    private
        { mutable Table: TTEntry[]
          mutable Mask: int              // index mask (size-1) where size is power-of-two buckets
          mutable Buckets: int           // number of buckets
          mutable BucketSize: int        // entries per bucket
          mutable Generation: byte
          mutable MaxAge: int }

let private bytesPerEntry = 16

let private nextPow2 (n:int) : int =
    if n <= 1 then 1
    else
        let mutable v = n - 1
        v <- v ||| (v >>> 1)
        v <- v ||| (v >>> 2)
        v <- v ||| (v >>> 4)
        v <- v ||| (v >>> 8)
        v <- v ||| (v >>> 16)
        v + 1

let private clamp (lo:int) (hi:int) (v:int) =
    if v < lo then lo elif v > hi then hi else v

let create (cfg:TTConfig) : TranspositionTable =
    let bucketSize = clamp 1 8 cfg.BucketSize

    // bytes budget
    let bytesBudget = int64 cfg.Mb * 1024L * 1024L
    let entriesBudget = int (bytesBudget / int64 bytesPerEntry)
    let entriesBudget = max entriesBudget bucketSize

    // choose power-of-two bucket count
    let buckets = max 1 (entriesBudget / bucketSize) |> nextPow2
    let totalEntries = buckets * bucketSize

    { Table = Array.zeroCreate totalEntries
      Mask = buckets - 1
      Buckets = buckets
      BucketSize = bucketSize
      Generation = 0uy
      MaxAge = max 0 cfg.MaxAge }

let clear (tt:TranspositionTable) : unit =
    Array.Clear(tt.Table, 0, tt.Table.Length)

let newSearch (tt:TranspositionTable) : unit =
    // advance generation (wraps naturally)
    tt.Generation <- tt.Generation + 1uy

let currentGeneration (tt:TranspositionTable) = tt.Generation

let sizeInfo (tt:TranspositionTable) =
    // convenient for logging
    let entries = tt.Table.Length
    let bytes = int64 entries * int64 bytesPerEntry
    let mb = float bytes / (1024.0 * 1024.0)
    (tt.Buckets, tt.BucketSize, entries, mb)

// ============================================================
// Probe / Store API
// ============================================================

type ProbeResult =
    { Hit: bool
      Entry: TTEntry }   // copy of entry if hit, else default

let inline private bucketBaseIndex (tt:TranspositionTable) (key:uint64) : int =
    let b = indexOfKey tt.Mask key
    b * tt.BucketSize

let probe (tt:TranspositionTable) (key:uint64) : ProbeResult =
    let lock32 = lock32OfKey key
    let baseIdx = bucketBaseIndex tt key

    let mutable i = 0
    let mutable found = false
    let mutable e = Unchecked.defaultof<TTEntry>

    while i < tt.BucketSize && not found do
        let cur = tt.Table[baseIdx + i]
        if cur.Lock32 = lock32 && cur.Lock32 <> 0u then
            found <- true
            e <- cur
        i <- i + 1

    { Hit = found; Entry = e }

/// Choose a replacement slot inside a bucket:
/// - empty if any
/// - else stale
/// - else shallowest depth
/// - else first
let private chooseStoreSlot (tt:TranspositionTable) (key:uint64) (_newDepth:sbyte) : int =
    let baseIdx = bucketBaseIndex tt key

    // 1) empty slot
    let mutable i = 0
    let mutable chosen = -1
    while i < tt.BucketSize && chosen = -1 do
        if tt.Table[baseIdx + i].Lock32 = 0u then
            chosen <- baseIdx + i
        i <- i + 1

    if chosen <> -1 then
        chosen
    else
        // 2) stale slot (first stale found)
        i <- 0
        while i < tt.BucketSize && chosen = -1 do
            let cur = tt.Table[baseIdx + i]
            if isStale tt.Generation cur.Generation tt.MaxAge then
                chosen <- baseIdx + i
            i <- i + 1

        if chosen <> -1 then
            chosen
        else
            // 3) depth-first victim, age tie-break (prefer evicting shallower; if equal depth, evict older)
            let mutable bestIdx = baseIdx
            let mutable bestDepth = Int32.MaxValue
            let mutable bestAge = -1

            i <- 0
            while i < tt.BucketSize do
                let idx = baseIdx + i
                let cur = tt.Table[idx]

                let d = int cur.Depth
                let age = unsignedByteDiff tt.Generation cur.Generation  // 0..255

                // Pick victim with smallest depth; if tie, largest age
                if d < bestDepth || (d = bestDepth && age > bestAge) then
                    bestDepth <- d
                    bestAge <- age
                    bestIdx <- idx

                i <- i + 1

            bestIdx
            
// Store (or update) an entry.
// Typical use in alpha-beta:
// - store only if depth >= existingDepth OR existing is stale.
// - or always store if BoundExact.
// This function applies a general replacement policy.
let store
    (tt:TranspositionTable)
    (key:uint64)
    (bestMove:int32)
    (score:int16)
    (eval:int16)
    (depth:int)
    (bound:byte)
    (flags:byte)
    : unit =

    let depthS =
        // clamp to sbyte
        if depth < -128 then sbyte -128
        elif depth > 127 then sbyte 127
        else sbyte depth

    let lock32 = lock32OfKey key
    let baseIdx = bucketBaseIndex tt key

    // If same lock exists in bucket, consider updating in-place
    let mutable i = 0
    let mutable updated = false
    while i < tt.BucketSize && not updated do
        let idx = baseIdx + i
        let cur = tt.Table[idx]
        if cur.Lock32 = lock32 && cur.Lock32 <> 0u then
            // update if replacement says yes
            if shouldReplace tt.Generation tt.MaxAge depthS cur || bound = BoundExact then
                tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)
            else
                // still refresh generation if you want "recently used" behavior:
                // (optional; comment out if you prefer strict "only on replace")
                let mutable c = cur
                c.Generation <- tt.Generation
                tt.Table[idx] <- c
            updated <- true
        i <- i + 1

    if not updated then
        let idx = chooseStoreSlot tt key depthS
        tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)

/// Optional: report a "hashfull" style metric (0..1000).
/// Counts how many first slots in buckets are non-empty, which is cheap.
let hashFullPermille (tt:TranspositionTable) : int =
    let mutable filled = 0
    let mutable b = 0
    while b < tt.Buckets do
        let idx = b * tt.BucketSize
        if tt.Table[idx].Lock32 <> 0u then filled <- filled + 1
        b <- b + 1
    (filled * 1000) / tt.Buckets
    
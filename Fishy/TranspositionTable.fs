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

let inline indexOfKey (mask:int) (key:uint64) : int =
    int (key &&& uint64 mask)

// ============================================================
// Replace policy helpers
// ============================================================

let inline private unsignedByteDiff (a:byte) (b:byte) : int =
    int ((uint32 a - uint32 b) &&& 0xFFu)

let inline private isStale (currentGen:byte) (entryGen:byte) (maxAge:int) : bool =
    unsignedByteDiff currentGen entryGen > maxAge

let inline private shouldReplace (currentGen:byte) (maxAge:int) (newDepth:sbyte) (old:TTEntry) : bool =
    if old.Lock32 = 0u then true
    else
        let oldDepth = int old.Depth
        let nd = int newDepth
        if nd > oldDepth then true
        elif nd < oldDepth then false
        else
            // same depth: prefer replacing stale; else prefer replacing strictly older gen
            if isStale currentGen old.Generation maxAge then true
            else unsignedByteDiff currentGen old.Generation > 0

// ============================================================
// TT container
// ============================================================

type TTConfig =
    { Mb: int
      BucketSize: int
      MaxAge: int }

type TranspositionTable =
    private
        { mutable Table: TTEntry[]
          mutable Mask: int
          mutable Buckets: int
          mutable BucketSize: int
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

    let bytesBudget = int64 cfg.Mb * 1024L * 1024L
    let entriesBudget = int (bytesBudget / int64 bytesPerEntry) |> max bucketSize

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
    tt.Generation <- tt.Generation + 1uy

let currentGeneration (tt:TranspositionTable) = tt.Generation

let sizeInfo (tt:TranspositionTable) =
    let entries = tt.Table.Length
    let bytes = int64 entries * int64 bytesPerEntry
    let mb = float bytes / (1024.0 * 1024.0)
    (tt.Buckets, tt.BucketSize, entries, mb)

// ============================================================
// Probe / Store API
// ============================================================

type ProbeResult =
    { Hit: bool
      Entry: TTEntry }

let inline private bucketBaseIndex (tt:TranspositionTable) (key:uint64) : int =
    (indexOfKey tt.Mask key) * tt.BucketSize

let probe (tt:TranspositionTable) (key:uint64) : ProbeResult =
    let lock32 = lock32OfKey key
    let baseIdx = bucketBaseIndex tt key

    let mutable i = 0
    let mutable found = false
    let mutable e = Unchecked.defaultof<TTEntry>

    while i < tt.BucketSize && not found do
        let cur = tt.Table[baseIdx + i]
        // empty slots have Lock32=0u and must never be a hit
        if cur.Lock32 <> 0u && cur.Lock32 = lock32 then
            found <- true
            e <- cur
        i <- i + 1

    { Hit = found; Entry = e }

let private chooseStoreSlot (tt:TranspositionTable) (key:uint64) : int =
    let baseIdx = bucketBaseIndex tt key

    let mutable bestIdx = baseIdx
    let mutable bestPriority = Int32.MinValue
    let mutable bestDepth = Int32.MaxValue
    let mutable bestAge = -1

    // Priority: empty >> stale >> normal victim
    // We still compute depth/age tie-break for non-empty.
    let mutable i = 0
    while i < tt.BucketSize do
        let idx = baseIdx + i
        let e = tt.Table[idx]

        if e.Lock32 = 0u then
            // Empty slot is always best possible — return immediately.
            bestIdx <- idx
            i <- tt.BucketSize
        else
            let age = unsignedByteDiff tt.Generation e.Generation
            let stale = age > tt.MaxAge
            let d = int e.Depth

            // Higher is better
            // 2 = stale, 1 = normal
            let priority = if stale then 2 else 1

            // Compare: higher priority wins.
            // If same priority, choose shallower depth.
            // If same depth, choose older.
            if priority > bestPriority then
                bestPriority <- priority
                bestDepth <- d
                bestAge <- age
                bestIdx <- idx
            elif priority = bestPriority then
                if d < bestDepth || (d = bestDepth && age > bestAge) then
                    bestDepth <- d
                    bestAge <- age
                    bestIdx <- idx

            i <- i + 1

    bestIdx
    
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
        if depth < -128 then sbyte -128
        elif depth > 127 then sbyte 127
        else sbyte depth

    let lock32 = lock32OfKey key
    let baseIdx = bucketBaseIndex tt key

    // 1) update in-place if same lock exists in bucket
    let mutable i = 0
    let mutable done_ = false

    while i < tt.BucketSize && not done_ do
        let idx = baseIdx + i
        let cur = tt.Table[idx]
        if cur.Lock32 <> 0u && cur.Lock32 = lock32 then
            if bound = BoundExact || shouldReplace tt.Generation tt.MaxAge depthS cur then
                tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)
            else
                // Optional policy: "touch" on non-replace to keep it young.
                // If you don't want that, delete this block.
                let mutable c = cur
                c.Generation <- tt.Generation
                tt.Table[idx] <- c
            done_ <- true
        i <- i + 1

    // 2) otherwise choose a slot in this bucket
    if not done_ then
        let idx = chooseStoreSlot tt key
        tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)

let hashFullPermille (tt:TranspositionTable) : int =
    let mutable filled = 0
    let mutable b = 0
    while b < tt.Buckets do
        let idx = b * tt.BucketSize
        if tt.Table[idx].Lock32 <> 0u then filled <- filled + 1
        b <- b + 1
    (filled * 1000) / tt.Buckets
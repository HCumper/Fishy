module TranspositionTable

open System
open System.Runtime.InteropServices
open System.Threading

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

type TTStats =
    { Probes: int64
      Hits: int64
      Useful: int64          // exact return or bound-caused cutoff (mark from Search)
      Stores: int64
      Overwrites: int64      // writing into a non-empty slot (incl same key replacement)
      AgeRejects: int64      // stale hits rejected at probe-time
      DepthRejects: int64 }  // depth-insufficient (mark from Search)

// ============================================================
// Thread-safe counters (Interlocked)
// ============================================================

let mutable private probes        = 0L
let mutable private hits          = 0L
let mutable private useful        = 0L
let mutable private stores        = 0L
let mutable private overwrites    = 0L
let mutable private ageRejects    = 0L
let mutable private depthRejects  = 0L

let inline private incProbe ()        = Interlocked.Increment(&probes) |> ignore
let inline private incHit ()          = Interlocked.Increment(&hits) |> ignore
let inline private incUseful ()       = Interlocked.Increment(&useful) |> ignore
let inline private incStore ()        = Interlocked.Increment(&stores) |> ignore
let inline private incOverwrite ()    = Interlocked.Increment(&overwrites) |> ignore
let inline private incAgeReject ()    = Interlocked.Increment(&ageRejects) |> ignore
let inline private incDepthReject ()  = Interlocked.Increment(&depthRejects) |> ignore

let resetStats () =
    Interlocked.Exchange(&probes, 0L) |> ignore
    Interlocked.Exchange(&hits, 0L) |> ignore
    Interlocked.Exchange(&useful, 0L) |> ignore
    Interlocked.Exchange(&stores, 0L) |> ignore
    Interlocked.Exchange(&overwrites, 0L) |> ignore
    Interlocked.Exchange(&ageRejects, 0L) |> ignore
    Interlocked.Exchange(&depthRejects, 0L) |> ignore

let getStats () : TTStats =
    { Probes = Interlocked.Read(&probes)
      Hits = Interlocked.Read(&hits)
      Useful = Interlocked.Read(&useful)
      Stores = Interlocked.Read(&stores)
      Overwrites = Interlocked.Read(&overwrites)
      AgeRejects = Interlocked.Read(&ageRejects)
      DepthRejects = Interlocked.Read(&depthRejects) }

/// Call this from Search when a TT entry actually returns a score
/// (BoundExact, or BoundLower/Upper causes cutoff).
let markUseful () : unit = incUseful ()

/// Call this from Search when pr.Hit but pr.Entry.Depth < requested depth.
let markDepthReject () : unit = incDepthReject ()

// ============================================================
// Key helpers
// ============================================================

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

/// Probe increments:
/// - Probes always
/// - Hits when lock matches and entry is not stale
/// - AgeRejects when lock matches but entry is stale (returned as miss)
let probe (tt:TranspositionTable) (key:uint64) : ProbeResult =
    incProbe ()

    let lock32 = lock32OfKey key
    let baseIdx = bucketBaseIndex tt key

    let mutable i = 0
    let mutable found = false
    let mutable e = Unchecked.defaultof<TTEntry>

    while i < tt.BucketSize && not found do
        let cur = tt.Table[baseIdx + i]
        if cur.Lock32 <> 0u && cur.Lock32 = lock32 then
            if isStale tt.Generation cur.Generation tt.MaxAge then
                // treat as miss
                incAgeReject ()
                found <- false
                // keep searching (in case of duplicates, though normally there aren't)
            else
                incHit ()
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
    let mutable i = 0
    while i < tt.BucketSize do
        let idx = baseIdx + i
        let e = tt.Table[idx]

        if e.Lock32 = 0u then
            bestIdx <- idx
            i <- tt.BucketSize
        else
            let age = unsignedByteDiff tt.Generation e.Generation
            let stale = age > tt.MaxAge
            let d = int e.Depth

            let priority = if stale then 2 else 1

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

    incStore ()

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
            // We are writing into an occupied slot (same key) => overwrite
            incOverwrite ()

            if bound = BoundExact || shouldReplace tt.Generation tt.MaxAge depthS cur then
                tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)
            else
                // Not replacing due to depth/age policy; "touch" to keep young.
                // (DepthReject counter is best recorded by Search, because it knows requested depth;
                // but we *can* record a store-side depth reject too: new depth < old depth.)
                if int depthS < int cur.Depth then
                    incDepthReject ()

                let mutable c = cur
                c.Generation <- tt.Generation
                tt.Table[idx] <- c

            done_ <- true
        i <- i + 1

    // 2) otherwise choose a slot in this bucket
    if not done_ then
        let idx = chooseStoreSlot tt key
        let old = tt.Table[idx]
        if old.Lock32 <> 0u then incOverwrite ()
        tt.Table[idx] <- TTEntry(lock32, bestMove, score, eval, depthS, bound, tt.Generation, flags)

let hashFullPermille (tt:TranspositionTable) : int =
    let mutable filled = 0
    let mutable b = 0
    while b < tt.Buckets do
        let idx = b * tt.BucketSize
        if tt.Table[idx].Lock32 <> 0u then filled <- filled + 1
        b <- b + 1
    (filled * 1000) / tt.Buckets
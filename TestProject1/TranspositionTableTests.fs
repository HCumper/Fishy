module TranspositionTableTests

open NUnit.Framework
open TranspositionTable
open System

[<TestFixture>]
type ``TranspositionTable tests`` () =

    /// Make a key that maps to a specific bucket index `b` (0..mask),
    /// while controlling the upper 32 bits so Lock32 differs.
    let mkKey (upper32:uint32) (bucketIndex:int) : uint64 =
        // indexOfKey uses low bits: int (key &&& uint64 mask)
        // so setting low bits == bucketIndex guarantees same bucket.
        (uint64 upper32 <<< 32) ||| uint64 bucketIndex
        
    let mkCfg bucketSize maxAge =
        { Mb = 1
          BucketSize = bucketSize
          MaxAge = maxAge }

    // ============================================================
    // Basic Functionality Tests
    // ============================================================

    [<Test>]
    member _.``store then probe returns hit with stored payload`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x1234_5678_9ABC_DEF0UL

        store tt key 123 42s 17s 9 BoundLower 5uy

        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Lock32, Is.EqualTo(lock32OfKey key))
        Assert.That(p.Entry.Move, Is.EqualTo(123))
        Assert.That(p.Entry.Score, Is.EqualTo(42s))
        Assert.That(p.Entry.Eval, Is.EqualTo(17s))
        Assert.That(p.Entry.Depth, Is.EqualTo(9y))
        Assert.That(p.Entry.Bound, Is.EqualTo(BoundLower))
        Assert.That(p.Entry.Flags, Is.EqualTo(5uy))

    [<Test>]
    member _.``probe non-existent key returns miss`` () =
        let tt = create (mkCfg 1 8)
        let key = 0xFFFF_FFFF_FFFF_FFFFUL

        let p = probe tt key
        Assert.That(p.Hit, Is.False)

    [<Test>]
    member _.``clear removes all entries and probes miss afterwards`` () =
        let tt = create (mkCfg 2 8)
        let key = 0x1357_2468_1111_2222UL

        store tt key 88 12s 4s 6 BoundUpper 0uy
        Assert.That((probe tt key).Hit, Is.True)

        clear tt

        let p = probe tt key
        Assert.That(p.Hit, Is.False)

    [<Test>]
    member _.``store same key twice updates entry`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x1111_2222_3333_4444UL

        // First store
        store tt key 100 10s 5s 5 BoundLower 0uy
        let p1 = probe tt key
        Assert.That(p1.Hit, Is.True)
        Assert.That(p1.Entry.Move, Is.EqualTo(100))
        Assert.That(p1.Entry.Score, Is.EqualTo(10s))

        // Second store (deeper, should replace)
        store tt key 200 20s 6s 8 BoundUpper 1uy
        let p2 = probe tt key
        Assert.That(p2.Hit, Is.True)
        Assert.That(p2.Entry.Move, Is.EqualTo(200))
        Assert.That(p2.Entry.Score, Is.EqualTo(20s))
        Assert.That(p2.Entry.Depth, Is.EqualTo(8y))

    [<Test>]
    member _.``store multiple different keys all retrievable`` () =
        let tt = create (mkCfg 4 8)
        
        let keys = [| 
            0x0001_0000_0000_0000UL
            0x0002_0000_0000_0001UL
            0x0003_0000_0000_0002UL
            0x0004_0000_0000_0003UL
        |]

        // Store all
        for i = 0 to 3 do
            store tt keys.[i] (i * 100) (int16 i) (int16 i) i BoundExact 0uy

        // Verify all
        for i = 0 to 3 do
            let p = probe tt keys.[i]
            Assert.That(p.Hit, Is.True, $"Key {i} should hit")
            Assert.That(p.Entry.Move, Is.EqualTo(i * 100), $"Move for key {i}")

    // ============================================================
    // Bound Type Tests
    // ============================================================

    [<Test>]
    member _.``all bound types stored and retrieved correctly`` () =
        let tt = create (mkCfg 1 8)
        
        let testCases = [
            (BoundExact, 0x1111_1111_1111_1111UL, "Exact")
            (BoundLower, 0x2222_2222_2222_2222UL, "Lower")
            (BoundUpper, 0x3333_3333_3333_3333UL, "Upper")
        ]

        for (bound, key, name) in testCases do
            store tt key 123 42s 17s 9 bound 0uy
            let p = probe tt key
            Assert.That(p.Hit, Is.True, $"{name} bound should hit")
            Assert.That(p.Entry.Bound, Is.EqualTo(bound), $"{name} bound should match")

    [<Test>]
    member _.``BoundExact always replaces regardless of depth`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x5555_5555_5555_5555UL

        // Store deep entry with BoundLower
        store tt key 100 10s 5s 20 BoundLower 0uy
        
        // Store shallow entry with BoundExact - should replace
        store tt key 200 20s 6s 5 BoundExact 0uy
        
        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Move, Is.EqualTo(200))
        Assert.That(p.Entry.Depth, Is.EqualTo(5y))
        Assert.That(p.Entry.Bound, Is.EqualTo(BoundExact))

    // ============================================================
    // Bucket Collision Tests
    // ============================================================

    [<Test>]
    member _.``bucket holds multiple colliding keys (bucketSize=2)`` () =
        let tt = create (mkCfg 2 8)

        let (buckets, bucketSize, _entries, _mb) = sizeInfo tt
        Assert.That(bucketSize, Is.EqualTo(2))
        let mask = buckets - 1

        let b = 0 &&& mask

        let k1 = mkKey 0x11111111u b
        let k2 = mkKey 0x22222222u b

        store tt k1 101 1s 1s 5 BoundLower 0uy
        store tt k2 202 2s 2s 6 BoundUpper 0uy

        let p1 = probe tt k1
        let p2 = probe tt k2

        Assert.That(p1.Hit, Is.True)
        Assert.That(p2.Hit, Is.True)
        Assert.That(p1.Entry.Move, Is.EqualTo(101))
        Assert.That(p2.Entry.Move, Is.EqualTo(202))

    [<Test>]
    member _.``bucket full: depth-first replacement evicts shallowest`` () =
        let tt = create (mkCfg 2 255)

        let (buckets, bucketSize, _, _) = sizeInfo tt
        Assert.That(bucketSize, Is.EqualTo(2))
        let mask = buckets - 1
        let b = 0 &&& mask

        let kDeep = mkKey 0xAAAABBBBu b
        let kShal = mkKey 0xCCCCDDDDu b
        let kNew  = mkKey 0xEEEEFFFFu b

        store tt kDeep 10 10s 0s 12 BoundLower 0uy
        store tt kShal 20 20s 0s 3  BoundLower 0uy

        store tt kNew  30 30s 0s 7  BoundLower 0uy

        Assert.That((probe tt kDeep).Hit, Is.True)
        Assert.That((probe tt kNew).Hit, Is.True)
        Assert.That((probe tt kShal).Hit, Is.False)

    [<Test>]
    member _.``bucket full: equal depth replacement uses age tie-break`` () =
        let tt = create (mkCfg 2 255)

        let (buckets, bucketSize, _, _) = sizeInfo tt
        Assert.That(bucketSize, Is.EqualTo(2))
        let mask = buckets - 1
        let b = 0 &&& mask

        let kOld = mkKey 0x01020304u b
        let kNew = mkKey 0x05060708u b
        let kIns = mkKey 0x0A0B0C0Du b

        store tt kOld 111 1s 0s 6 BoundLower 0uy

        newSearch tt
        store tt kNew 222 2s 0s 6 BoundLower 0uy

        store tt kIns 333 3s 0s 6 BoundLower 0uy

        Assert.That((probe tt kNew).Hit, Is.True)
        Assert.That((probe tt kIns).Hit, Is.True)
        Assert.That((probe tt kOld).Hit, Is.False)

    [<Test>]
    member _.``bucket size of 4 holds 4 different colliding keys`` () =
        let tt = create (mkCfg 4 8)

        let (buckets, bucketSize, _, _) = sizeInfo tt
        Assert.That(bucketSize, Is.EqualTo(4))
        let mask = buckets - 1
        let b = 5 &&& mask

        let keys = [|
            mkKey 0x11111111u b
            mkKey 0x22222222u b
            mkKey 0x33333333u b
            mkKey 0x44444444u b
        |]

        for i = 0 to 3 do
            store tt keys.[i] (i * 100) (int16 i) 0s (i + 5) BoundExact 0uy

        // All should be present
        for i = 0 to 3 do
            let p = probe tt keys.[i]
            Assert.That(p.Hit, Is.True, $"Key {i} should be present")
            Assert.That(p.Entry.Move, Is.EqualTo(i * 100))

    [<Test>]
    member _.``bucket overflow evicts according to replacement policy`` () =
        let tt = create (mkCfg 2 255)

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 10 &&& mask

        // Fill bucket with 2 entries
        let k1 = mkKey 0xAAAA0001u b
        let k2 = mkKey 0xBBBB0002u b
        store tt k1 100 10s 0s 10 BoundLower 0uy  // deeper
        store tt k2 200 20s 0s 3  BoundLower 0uy  // shallower

        // Try to add third (should evict k2, the shallower one)
        let k3 = mkKey 0xCCCC0003u b
        store tt k3 300 30s 0s 7  BoundLower 0uy

        Assert.That((probe tt k1).Hit, Is.True, "Deep entry should remain")
        Assert.That((probe tt k3).Hit, Is.True, "New entry should be stored")
        Assert.That((probe tt k2).Hit, Is.False, "Shallow entry should be evicted")

    // ============================================================
    // Generation / Aging Tests
    // ============================================================

    [<Test>]
    member _.``newSearch advances generation`` () =
        let tt = create (mkCfg 1 8)
        
        let gen0 = currentGeneration tt
        Assert.That(gen0, Is.EqualTo(0uy))

        newSearch tt
        let gen1 = currentGeneration tt
        Assert.That(gen1, Is.EqualTo(1uy))

        newSearch tt
        let gen2 = currentGeneration tt
        Assert.That(gen2, Is.EqualTo(2uy))

    [<Test>]
    member _.``generation wraps at 255`` () =
        let tt = create (mkCfg 1 8)
        
        // Advance to 255
        for i = 0 to 254 do
            newSearch tt

        Assert.That(currentGeneration tt, Is.EqualTo(255uy))

        // Wrap to 0
        newSearch tt
        Assert.That(currentGeneration tt, Is.EqualTo(0uy))

    [<Test>]
    member _.``stored entries have current generation`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x7777_7777_7777_7777UL

        newSearch tt
        newSearch tt
        let currentGen = currentGeneration tt
        Assert.That(currentGen, Is.EqualTo(2uy))

        store tt key 123 42s 17s 9 BoundLower 0uy

        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Generation, Is.EqualTo(currentGen))

    [<Test>]
    member _.``stale entries are preferred victims once maxAge exceeded when replacement is required`` () =
        let tt = create (mkCfg 2 3 |> fun c -> { c with BucketSize = 1 })  // MaxAge = 3, force replacement

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 1 &&& mask

        // Same bucket (low bits), different lock32 (upper bits)
        let kOld = (uint64 0x1111u <<< 32) ||| uint64 b
        let kNew = (uint64 0x2222u <<< 32) ||| uint64 b

        // Store old entry at generation 0
        store tt kOld 100 10s 0s 10 BoundLower 0uy
        Assert.That((probe tt kOld).Hit, Is.True)

        // Advance 5 generations (age = 5 > maxAge 3 => stale)
        for _ = 1 to 5 do newSearch tt

        // Store new entry into same 1-slot bucket => must replace; stale victim should be overwritten
        store tt kNew 200 20s 0s 5 BoundLower 0uy

        Assert.That((probe tt kOld).Hit, Is.False, "Old entry should miss because it was overwritten")
        Assert.That((probe tt kNew).Hit, Is.True, "New entry should be present")

    [<Test>]
    member _.``entries within maxAge are not stale`` () =
        let tt = create (mkCfg 2 5)  // MaxAge = 5

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 0 &&& mask

        let k1 = mkKey 0xAAAAu b
        let k2 = mkKey 0xBBBBu b

        // Store at generation 0
        store tt k1 100 10s 0s 10 BoundLower 0uy

        // Advance 3 generations (within maxAge)
        for _ = 1 to 3 do
            newSearch tt

        // Try to store another key (both should coexist since not stale)
        store tt k2 200 20s 0s 8 BoundLower 0uy

        Assert.That((probe tt k1).Hit, Is.True, "Entry within maxAge should remain")
        Assert.That((probe tt k2).Hit, Is.True, "New entry should be stored")

    // ============================================================
    // Depth Handling Tests
    // ============================================================

    [<Test>]
    member _.``depth clamped to sbyte range -128 to 127`` () =
        let tt = create (mkCfg 1 8)
        
        // Test extreme values
        let testCases = [
            (200, 127y, "Above max")
            (-200, -128y, "Below min")
            (0, 0y, "Zero")
            (50, 50y, "Normal positive")
            (-50, -50y, "Normal negative")
        ]

        for i, (inputDepth, expectedDepth, name) in List.indexed testCases do
            let key = (uint64 (i + 1) <<< 32)   // any nonzero high 32 bits
            store tt key 0 0s 0s inputDepth BoundExact 0uy
            let p = probe tt key
            Assert.That(p.Hit, Is.True, $"{name} should hit")
            Assert.That(p.Entry.Depth, Is.EqualTo(expectedDepth), $"{name} depth")
    
    [<Test>]
    member _.``deeper entry replaces shallower`` () =
        let tt = create (mkCfg 1 8)
        let key = 0xDEAD_BEEF_CAFE_BABEUL

        // Store shallow
        store tt key 100 10s 5s 3 BoundLower 0uy
        
        // Store deeper - should replace
        store tt key 200 20s 6s 10 BoundLower 0uy

        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Move, Is.EqualTo(200))
        Assert.That(p.Entry.Depth, Is.EqualTo(10y))

    [<Test>]
    member _.``shallower entry does not replace deeper`` () =
        let tt = create (mkCfg 1 8)
        let key = 0xFEED_FACE_DEAD_BEEFUL

        // Store deep
        store tt key 100 10s 5s 15 BoundLower 0uy
        
        // Try to store shallow - should NOT replace
        store tt key 200 20s 6s 5 BoundLower 0uy

        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Move, Is.EqualTo(100), "Original move should remain")
        Assert.That(p.Entry.Depth, Is.EqualTo(15y), "Original depth should remain")

    [<Test>]
    member _.``equal depth updates to newer generation`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x1234_5678_ABCD_EF00UL

        // Store at generation 0
        store tt key 100 10s 5s 7 BoundLower 0uy
        let p1 = probe tt key
        Assert.That(p1.Entry.Generation, Is.EqualTo(0uy))

        // Advance generation
        newSearch tt
        
        // Store same depth - should update generation
        store tt key 200 20s 6s 7 BoundLower 0uy
        let p2 = probe tt key
        Assert.That(p2.Entry.Generation, Is.EqualTo(1uy), "Generation should update")
        Assert.That(p2.Entry.Move, Is.EqualTo(200), "Move should update")

    // ============================================================
    // Score and Eval Tests
    // ============================================================

    [<Test>]
    member _.``score stored and retrieved correctly including negatives`` () =
        let tt = create (mkCfg 1 8)

        let testCases = [
            (int16 32767, "Max positive")
            (int16 -32768, "Max negative")
            (int16 0, "Zero")
            (int16 100, "Positive")
            (int16 -100, "Negative")
        ]

        for i, (score, name) in List.indexed testCases do
            let key = uint64 (i + 1) <<< 32   // lock32 != 0
            store tt key 0 score 0s 5 BoundExact 0uy
            let p = probe tt key
            Assert.That(p.Hit, Is.True, $"{name} should hit")
            Assert.That(p.Entry.Score, Is.EqualTo(score), $"{name} score")

    [<Test>]
    member _.``eval stored and retrieved independently of score`` () =
        let tt = create (mkCfg 1 8)
        let key = 0x9999_9999_9999_9999UL

        store tt key 123 100s 200s 7 BoundExact 0uy

        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Score, Is.EqualTo(100s), "Score")
        Assert.That(p.Entry.Eval, Is.EqualTo(200s), "Eval")

    // ============================================================
    // Move Storage Tests
    // ============================================================

    [<Test>]
    member _.``move stored as int32 including negative and zero`` () =
        let tt = create (mkCfg 1 8)

        let testCases = [
            (0, "Zero/no move")
            (12345, "Positive")
            (-1, "Negative")
            (Int32.MaxValue, "Max int32")
            (Int32.MinValue, "Min int32")
        ]

        for i, (move, name) in List.indexed testCases do
            let key = uint64 (i + 1) <<< 32   // ensures lock32 != 0
            store tt key (int32 move) 0s 0s 5 BoundExact 0uy
            let p = probe tt key
            Assert.That(p.Hit, Is.True, $"{name} should hit")
            Assert.That(p.Entry.Move, Is.EqualTo(int32 move), $"{name} move")
            
    // ============================================================
    // Flags Tests
    // ============================================================

    [<Test>]
    member _.``flags byte stored and retrieved correctly`` () =
        let tt = create (mkCfg 1 8)
        
        let testCases = [
            (0uy, "No flags")
            (1uy, "PV flag")
            (255uy, "All flags")
            (0b10101010uy, "Pattern")
        ]

        for i, (flags, name) in List.indexed testCases do
            let key = uint64 (i + 1) <<< 32   // any nonzero high 32 bits
            store tt key 0 0s 0s 5 BoundExact flags
            let p = probe tt key
            Assert.That(p.Hit, Is.True, $"{name} should hit")
            Assert.That(p.Entry.Flags, Is.EqualTo(flags), $"{name} flags")

    // ============================================================
    // Size and Configuration Tests
    // ============================================================

    [<Test>]
    member _.``sizeInfo returns correct values`` () =
        let tt = create { Mb = 2; BucketSize = 4; MaxAge = 8 }
        
        let (buckets, bucketSize, entries, mb) = sizeInfo tt
        
        Assert.That(bucketSize, Is.EqualTo(4))
        Assert.That(entries, Is.EqualTo(buckets * bucketSize))
        Assert.That(mb, Is.GreaterThan(0.0))
        Assert.That(buckets, Is.GreaterThan(0))
        
        // Buckets should be power of 2
        let isPowerOf2 = buckets > 0 && (buckets &&& (buckets - 1)) = 0
        Assert.That(isPowerOf2, Is.True, "Buckets should be power of 2")

    [<Test>]
    member _.``bucketSize clamped between 1 and 8`` () =
        // Test clamping to minimum
        let tt1 = create { Mb = 1; BucketSize = 0; MaxAge = 8 }
        let (_, bs1, _, _) = sizeInfo tt1
        Assert.That(bs1, Is.GreaterThanOrEqualTo(1))

        // Test clamping to maximum
        let tt2 = create { Mb = 1; BucketSize = 100; MaxAge = 8 }
        let (_, bs2, _, _) = sizeInfo tt2
        Assert.That(bs2, Is.LessThanOrEqualTo(8))

        // Test normal value
        let tt3 = create { Mb = 1; BucketSize = 4; MaxAge = 8 }
        let (_, bs3, _, _) = sizeInfo tt3
        Assert.That(bs3, Is.EqualTo(4))

    [<Test>]
    member _.``different MB sizes create different table sizes`` () =
        let tt1 = create { Mb = 1; BucketSize = 2; MaxAge = 8 }
        let tt2 = create { Mb = 2; BucketSize = 2; MaxAge = 8 }
        let tt4 = create { Mb = 4; BucketSize = 2; MaxAge = 8 }

        let (_, _, entries1, _) = sizeInfo tt1
        let (_, _, entries2, _) = sizeInfo tt2
        let (_, _, entries4, _) = sizeInfo tt4

        Assert.That(entries2, Is.GreaterThan(entries1))
        Assert.That(entries4, Is.GreaterThan(entries2))

    [<Test>]
    member _.``maxAge=0 makes prior generation stale and preferred for replacement when bucket is full`` () =
        let tt = create { Mb = 1; BucketSize = 1; MaxAge = 0 }

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1

        // pick any bucket index
        let b = 1 &&& mask

        // Build two keys that map to the same bucket (low bits) but have different lock32 (upper bits)
        let k1 = (uint64 0x1111u <<< 32) ||| uint64 b
        let k2 = (uint64 0x2222u <<< 32) ||| uint64 b

        store tt k1 100 10s 0s 10 BoundLower 0uy

        // Advance generation: with MaxAge=0, k1 is now stale
        newSearch tt

        // BucketSize=1 forces replacement; stale entry should be overwritten
        store tt k2 200 20s 0s 5 BoundLower 0uy

        Assert.That((probe tt k1).Hit, Is.False, "k1 should miss because it was overwritten")
        Assert.That((probe tt k2).Hit, Is.True)
    
    // ============================================================
    // HashFull Tests
    // ============================================================

    [<Test>]
    member _.``hashFullPermille returns 0 for empty table`` () =
        let tt = create (mkCfg 4 8)
        let hf = hashFullPermille tt
        Assert.That(hf, Is.EqualTo(0))

    [<Test>]
    member _.``hashFullPermille increases as entries added`` () =
        let tt = create (mkCfg 4 8)
        let (buckets, _, _, _) = sizeInfo tt

        let hf0 = hashFullPermille tt
        Assert.That(hf0, Is.EqualTo(0))

        // Fill about 25% of first slots
        let keysToFill = buckets / 4
        for i = 0 to keysToFill - 1 do
            let key = mkKey (uint32 i) i
            store tt key i 0s 0s 5 BoundExact 0uy

        let hf25 = hashFullPermille tt
        Assert.That(hf25, Is.GreaterThan(0))
        Assert.That(hf25, Is.LessThan(500))  // Should be less than 50%

    [<Test>]
    member _.``hashFullPermille returns 1000 when every bucket's first slot is non-empty`` () =
        let tt = create (mkCfg 2 8)
        let (buckets, bucketSize, _, _) = sizeInfo tt
        Assert.That(bucketSize, Is.EqualTo(2))  // optional sanity check if mkCfg sets BucketSize=2

        // Force one store into each bucket, and ensure Lock32 != 0 so it counts as filled.
        for b = 0 to buckets - 1 do
            let lock32 = uint32 (b + 1)                 // never 0
            let key = (uint64 lock32 <<< 32) ||| uint64 b  // low bits pick bucket b
            store tt key (int32 b) 0s 0s 5 BoundExact 0uy

        let hf = hashFullPermille tt
        Assert.That(hf, Is.EqualTo(1000))

    [<Test>]
    member _.``hashFullPermille resets to 0 after clear`` () =
        let tt = create (mkCfg 4 8)
        let (buckets, _, _, _) = sizeInfo tt

        // Fill some entries
        for i = 0 to buckets / 2 do
            let key = mkKey (uint32 i) i
            store tt key i 0s 0s 5 BoundExact 0uy

        let hfBefore = hashFullPermille tt
        Assert.That(hfBefore, Is.GreaterThan(0))

        clear tt

        let hfAfter = hashFullPermille tt
        Assert.That(hfAfter, Is.EqualTo(0))

    // ============================================================
    // Lock32 Collision Tests
    // ============================================================

    [<Test>]
    member _.``different keys with same lock32 produce false-positive hit (wrong payload)`` () =
        let tt = create (mkCfg 1 8 |> fun c -> { c with BucketSize = 1 })

        let upper = 0x12345678u

        // With BucketSize=1, buckets = 65536, mask = 0xFFFF.
        // So force same bucket by keeping low 16 bits identical.
        let key1 = (uint64 upper <<< 32) ||| 0x00000001UL
        let key2 = (uint64 upper <<< 32) ||| 0x00010001UL  // +0x10000 keeps low 16 bits == 0x0001

        Assert.That(lock32OfKey key1, Is.EqualTo(lock32OfKey key2))

        store tt key1 100 10s 0s 5 BoundLower 0uy
        store tt key2 200 20s 0s 6 BoundUpper 0uy   // replaces same-lock entry

        let p1 = probe tt key1
        let p2 = probe tt key2

        Assert.That(p1.Hit, Is.True)
        Assert.That(p2.Hit, Is.True)

        // Both probes will return the *same* entry (key2's payload),
        // so probing key1 is a false positive.
        Assert.That(p1.Entry.Move, Is.EqualTo(200))
        Assert.That(p2.Entry.Move, Is.EqualTo(200))
        
    [<Test>]
    member _.``lock32 of zero is treated as empty slot`` () =
        let tt = create (mkCfg 1 8)
        
        // A key that produces lock32 = 0
        let key = 0x0000_0000_FFFF_FFFFUL
        Assert.That(lock32OfKey key, Is.EqualTo(0u))

        store tt key 123 42s 17s 9 BoundLower 0uy

        // Should not be found because lock32=0 means empty
        let p = probe tt key
        Assert.That(p.Hit, Is.False, "Lock32=0 treated as empty")

    // ============================================================
    // Edge Case Tests
    // ============================================================

    [<Test>]
    member _.``storing many entries in same bucket evicts correctly`` () =
        let tt = create (mkCfg 2 255)  // BucketSize=2
        
        let (buckets, bucketSize, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 7 &&& mask

        // Store 10 entries to same bucket (can only hold 2)
        let keys = [| for i in 0 .. 9 -> mkKey (uint32 (0x1000 + i)) b |]
        
        for i = 0 to 9 do
            store tt keys.[i] (i * 100) (int16 i) 0s (10 - i) BoundLower 0uy

        // Only the 2 deepest should remain
        let results = keys |> Array.map (fun k -> probe tt k)
        let hits = results |> Array.filter (fun r -> r.Hit)
        
        Assert.That(hits.Length, Is.LessThanOrEqualTo(bucketSize))

    [<Test>]
    member _.``probe and store work with maximum uint64 key`` () =
        let tt = create (mkCfg 1 8)
        let maxKey = UInt64.MaxValue

        store tt maxKey 999 99s 77s 12 BoundExact 0uy

        let p = probe tt maxKey
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Move, Is.EqualTo(999))

    [<Test>]
    member _.``probe and store work with minimum uint64 key`` () =
        let tt = create (mkCfg 1 8)
        let minKey = UInt64.MinValue  // 0

        store tt minKey 111 11s 22s 8 BoundLower 0uy

        // This should fail because lock32=0 means empty
        let p = probe tt minKey
        Assert.That(p.Hit, Is.False, "Key 0 has lock32=0, treated as empty")

    [<Test>]
    member _.``many stores and probes maintain consistency`` () =
        let tt = create { Mb = 4; BucketSize = 4; MaxAge = 10 }
        let rnd = Random(42)
        
        // Store 1000 random entries
        let keys = [| for _ in 1 .. 1000 -> uint64 (rnd.Next()) |]
        
        for i = 0 to 999 do
            store tt keys.[i] i (int16 i) 0s (i % 20) BoundExact 0uy

        // Probe all keys - some may have been evicted but none should have wrong data
        for i = 0 to 999 do
            let p = probe tt keys.[i]
            if p.Hit then
                Assert.That(p.Entry.Move, Is.EqualTo(i), $"Entry {i} has wrong move")
                Assert.That(p.Entry.Score, Is.EqualTo(int16 i), $"Entry {i} has wrong score")

    [<Test>]
    member _.``generation wraparound: stale entries still hit, but are preferred victims for replacement`` () =
        // Force 1-entry buckets so any store into same bucket overwrites
        let tt = create (mkCfg 2 10 |> fun c -> { c with BucketSize = 1 })  // MaxAge=10

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1   // with Mb=2, BucketSize=1 => buckets=131072, mask=0x1FFFF

        // Choose a specific bucket index (any value in 0..mask)
        let bucketIndex = 0x12345 &&& mask

        // Build two keys:
        // - different lock32 (AAAA vs BBBB) so store won't treat them as the same entry
        // - same bucket index: same low bits under mask
        let low1 = uint64 bucketIndex
        let low2 = low1 + (uint64 (mask + 1))   // adds 2^17 here; keeps low 17 bits identical => same bucket

        let k1 = (uint64 0xAAAAu <<< 32) ||| low1
        let k2 = (uint64 0xBBBBu <<< 32) ||| low2

        // Sanity: ensure they really map to the same bucket
        Assert.That(indexOfKey mask k1, Is.EqualTo(indexOfKey mask k2))

        // Advance generation to 250
        for _ = 1 to 250 do
            newSearch tt
        Assert.That(currentGeneration tt, Is.EqualTo(250uy))

        // Store k1 at generation 250
        store tt k1 100 10s 0s 10 BoundLower 0uy

        // Advance 11 generations: 250 -> 5 (wrap)
        for _ = 1 to 11 do
            newSearch tt
        Assert.That(currentGeneration tt, Is.EqualTo(5uy))

        // k1 is now stale by your definition (age = (5-250) mod 256 = 11 > MaxAge=10)
        // BUT probe ignores staleness, so it should still hit.
        Assert.That((probe tt k1).Hit, Is.True, "Probe should still hit stale entries (probe ignores age)")

        // Store k2 into the same 1-entry bucket: chooseStoreSlot should pick the stale slot and overwrite k1
        store tt k2 200 20s 0s 5 BoundLower 0uy

        // After overwrite, k1 should miss (not because stale, but because it was replaced)
        Assert.That((probe tt k1).Hit, Is.False, "k1 should miss because it was overwritten")
        Assert.That((probe tt k2).Hit, Is.True, "k2 should be present after overwrite")

    // ============================================================
    // Replacement Policy Comprehensive Tests
    // ============================================================

    [<Test>]
    member _.``replacement policy: empty slot preferred over eviction`` () =
        let tt = create (mkCfg 4 255)  // BucketSize=4

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 0 &&& mask

        // Store 2 entries (bucket has 4 slots)
        let k1 = mkKey 0x1111u b
        let k2 = mkKey 0x2222u b
        store tt k1 100 10s 0s 10 BoundLower 0uy
        store tt k2 200 20s 0s 5  BoundLower 0uy

        // Store third (should use empty slot, not evict)
        let k3 = mkKey 0x3333u b
        store tt k3 300 30s 0s 3  BoundLower 0uy

        // All three should be present
        Assert.That((probe tt k1).Hit, Is.True)
        Assert.That((probe tt k2).Hit, Is.True)
        Assert.That((probe tt k3).Hit, Is.True)

    [<Test>]
    member _.``replacement policy: stale preferred over depth-based eviction`` () =
        let tt = create (mkCfg 4 3)  // MaxAge=3

        let (buckets, _, _, _) = sizeInfo tt
        let mask = buckets - 1
        let b = 0 &&& mask

        // Fill bucket at generation 0
        let keys = [|
            mkKey 0x1111u b
            mkKey 0x2222u b
            mkKey 0x3333u b
            mkKey 0x4444u b
        |]

        // Store with varying depths
        store tt keys.[0] 100 10s 0s 10 BoundLower 0uy  // Deep
        store tt keys.[1] 200 20s 0s 8  BoundLower 0uy
        store tt keys.[2] 300 30s 0s 6  BoundLower 0uy
        store tt keys.[3] 400 40s 0s 4  BoundLower 0uy  // Shallow

        // Advance 5 generations (all become stale, maxAge=3)
        for _ = 1 to 5 do
            newSearch tt

        // Store new entry - should evict first stale found
        let kNew = mkKey 0x5555u b
        store tt kNew 500 50s 0s 7 BoundLower 0uy

        // New entry should be present
        Assert.That((probe tt kNew).Hit, Is.True)
        
        // At least one old entry should be evicted
        let oldHits = keys |> Array.filter (fun k -> (probe tt k).Hit) |> Array.length
        Assert.That(oldHits, Is.LessThan(4), "At least one stale entry should be evicted")

    [<Test>]
    member _.``entry size is exactly 16 bytes`` () =
        // Verify the struct packing
        let entrySize = sizeof<TTEntry>
        Assert.That(entrySize, Is.EqualTo(16), "TTEntry should be exactly 16 bytes")

    [<Test>]
    member _.``lock32OfKey extracts upper 32 bits correctly`` () =
        let testCases = [
            (0x1234_5678_9ABC_DEF0UL, 0x1234_5678u)
            (0xFFFF_FFFF_0000_0000UL, 0xFFFF_FFFFu)
            (0x0000_0000_FFFF_FFFFUL, 0x0000_0000u)
            (0xDEAD_BEEF_CAFE_BABEUL, 0xDEAD_BEEFu)
        ]

        for (key, expectedLock) in testCases do
            let lock32 = lock32OfKey key
            Assert.That(lock32, Is.EqualTo(expectedLock), $"Key {key:X}")

    [<Test>]
    member _.``indexOfKey uses lower bits with mask`` () =
        let mask = 0xFF  // 256 buckets
        
        let testCases = [
            (0x1234_5678_9ABC_DE00UL, 0x00)
            (0x1234_5678_9ABC_DE10UL, 0x10)
            (0x1234_5678_9ABC_DEFFUL, 0xFF)
        ]

        for (key, expectedIndex) in testCases do
            let index = indexOfKey mask key
            Assert.That(index, Is.EqualTo(expectedIndex), $"Key {key:X}")

    // ============================================================
    // Real-World Scenario Tests
    // ============================================================

    [<Test>]
    member _.``simulated search: entries from deeper searches replace shallower`` () =
        let tt = create { Mb = 1; BucketSize = 2; MaxAge = 8 }
        let key = 0xABCD_1234_5678_9ABCUL

        // Simulate iterative deepening
        for depth = 1 to 10 do
            let score = int16 (depth * 10)
            store tt key (depth * 100) score score depth BoundExact 0uy

        // Final entry should be deepest
        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Depth, Is.EqualTo(10y))
        Assert.That(p.Entry.Move, Is.EqualTo(1000))

    [<Test>]
    member _.``simulated quiescence search: shallow searches don't overwrite deep`` () =
        let tt = create { Mb = 1; BucketSize = 1; MaxAge = 8 }
        let key = 0x1111_2222_3333_4444UL

        // Store deep search result
        store tt key 1000 100s 50s 15 BoundExact 0uy

        // Simulate quiescence searches (shallow)
        for qDepth = 0 to 3 do
            store tt key (qDepth + 1) (int16 qDepth) 0s qDepth BoundLower 0uy

        // Deep entry should remain
        let p = probe tt key
        Assert.That(p.Hit, Is.True)
        Assert.That(p.Entry.Depth, Is.EqualTo(15y), "Deep search result should not be overwritten")
        Assert.That(p.Entry.Move, Is.EqualTo(1000))

    [<Test>]
    member _.``typical chess engine usage pattern`` () =
        let tt = create { Mb = 16; BucketSize = 4; MaxAge = 8 }
        
        // Simulate 5 moves in a game
        for moveNum = 1 to 5 do
            newSearch tt  // New search for each move
            
            // Iterative deepening from depth 1 to 8
            for depth = 1 to 8 do
                // Generate some positions at this depth
                let rnd = Random(moveNum * 1000 + depth)
                
                for _ = 1 to 100 do
                    let key = uint64 (rnd.Next()) <<< 32 ||| uint64 (rnd.Next())
                    let move = rnd.Next()
                    let score = int16 (rnd.Next(-1000, 1000))
                    let bound = byte (rnd.Next(0, 3))
                    
                    store tt key move score score depth bound 0uy

        // Verify table has reasonable fill
        let hf = hashFullPermille tt
        Assert.That(hf, Is.GreaterThan(0), "Table should have some entries")
        Assert.That(hf, Is.LessThanOrEqualTo(1000), "Hashfull should be <= 1000")

        printfn "Hashfull after simulated game: %d/1000" hf
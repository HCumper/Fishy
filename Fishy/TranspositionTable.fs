module TranspositionTable

open System.Runtime.InteropServices

/// Compact 16-byte TT entry using a 32-bit lock (partial key).
/// Typical: store (lock = upper 32 bits of zobrist) and index by lower bits.
///
/// Size (Pack=1) = 16 bytes exactly:
///   Lock32(4) + Move(4) + Score(2) + Eval(2) + Depth(1) + Bound(1) + Gen(1) + Flags(1)
///
/// Conventions:
/// - Lock32: usually (uint32)(key >>> 32) or a mixed 32-bit hash.
/// - Depth: ply depth stored as sbyte (0..127 is plenty for practical search depths).
/// - Bound: 0=Exact, 1=Lower, 2=Upper (you choose).
/// - Flags: optional (e.g., PV flag, mate-score flag, etc.). Can be 0 if unused.
[<Struct; StructLayout(LayoutKind.Sequential, Pack=1)>]
type TTEntry =
    val mutable Lock32     : uint32  // 4
    val mutable Move       : int32   // 4  => 8
    val mutable Score      : int16   // 2  => 10
    val mutable Eval       : int16   // 2  => 12
    val mutable Depth      : sbyte   // 1  => 13
    val mutable Bound      : byte    // 1  => 14
    val mutable Generation : byte    // 1  => 15
    val mutable Flags      : byte    // 1  => 16

    new (lock32, mv, score, eval, depth, bound, gen, flags) =
        { Lock32 = lock32
          Move = mv
          Score = score
          Eval = eval
          Depth = depth
          Bound = bound
          Generation = gen
          Flags = flags }

/// Helper: derive the 32-bit lock from a 64-bit zobrist key (common choice).
let inline lock32OfKey (key:uint64) : uint32 =
    uint32 (key >>> 32)
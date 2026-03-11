# Fishy Design Overview

## tl:dr
Fishy is a classical chess engine written in F# and organized as a set of focused modules rather than a single monolithic system. Its design balances two competing goals: clarity of structure and efficiency in the parts of the engine that run millions of times during search. The engine therefore uses explicit types and clean module boundaries where possible, while accepting compact encodings, cached state, and controlled mutation in hot paths.

At the center of the design is the Position representation, which combines a flat board array, game-state fields, and cached king locations. The board stores piece placement; the game state stores side to move, castling rights, en passant information, clocks, and the current Zobrist hash key; and the king cache allows fast legality and check testing. These components must remain exactly synchronized. The whole engine depends on a small set of invariants: board and king cache must agree, the hash key must match the current position exactly, special-state fields must be legally consistent, and make/unmake must restore all of them precisely.

Move generation translates static state into legal search choices. The design distinguishes local movement rules from full chess legality. Piece motion can often be described locally, but legality depends on global king safety, which makes check evasions, castling, and en passant especially important. The engine therefore treats move generation as both a correctness subsystem and a performance-critical subsystem. Full-width search requires all legal moves, while quiescence search can use a narrower tactical set except when the king is in check.

Search is built around iterative deepening, negamax, alpha-beta pruning, quiescence search, and a transposition table. Iterative deepening provides a completed best move at every finished depth, improves move ordering, and makes practical time management possible. Negamax keeps the recursive logic uniform by expressing scores from the viewpoint of the side to move. Alpha-beta reduces the tree by proving that some branches cannot affect the result. Quiescence extends tactically unstable leaf positions so that static evaluation is applied to quieter nodes. The transposition table both reuses prior work and supplies likely strong moves for ordering.

The engine reports analysis only from completed root iterations. This is a deliberate design choice: GUI-visible depth, score, and principal variation should reflect a meaningful root result, not transient interior-node activity. Root search also owns time management. Fishy uses a soft/hard time budget model and can decide not to begin a deeper iteration if the previous ones suggest that it is unlikely to finish in time.

Evaluation is classical and hand-crafted rather than neural. It uses tapered midgame/endgame scoring, material values, piece-square tables, pseudo-mobility, and pawn-structure terms such as passed-pawn bonuses and isolated or doubled pawn penalties. Internally, many terms are accumulated in white-view and then converted to side-to-move view for negamax. This keeps the evaluator explainable and easy to extend while still giving search meaningful positional guidance.

Throughout the design, Fishy accepts a practical tradeoff between elegance and efficiency. A more purely functional model might rebuild positions instead of mutating and undoing them, derive king locations and hash keys on demand, or use richer piece and board structures. Fishy instead uses make/unmake, compact encodings, cached derived state, and array-based storage because these choices are better suited to search-heavy engine work. Bitboards would push still further toward efficiency, especially for attack and occupancy operations, but at the cost of greater opacity. Fishy currently occupies a middle ground: explicit enough to document and reason about, but efficient enough to support a serious classical engine architecture.

In summary, Fishy is structured around a compact, invariant-driven position model; legality-aware move generation; exact make/unmake state transitions; a classical alpha-beta search with quiescence and TT support; and a transparent tapered evaluator. The design is intentionally modular so that features such as stronger move ordering, richer evaluation terms, pawn hashing, or more advanced search reductions can be added incrementally without changing the overall architecture.


## Full documentation

Purpose

## Purpose

Fishy is a classical chess engine written in F#. Its current design emphasizes clarity, correctness, and incremental improvement while still using the standard building blocks of a modern alpha-beta engine:

* compact board representation
* legal move generation
* make/unmake search workflow
* tapered static evaluation
* transposition table support
* iterative deepening at the root
* quiescence search to reduce horizon effects
* basic time management

The engine is structured as a collection of focused modules rather than one monolithic subsystem. Each module owns a specific concern: board representation, move generation, move execution, evaluation, search, hashing, and UCI communication.

## High-level architecture

At a high level, a search proceeds like this:

1. A position is represented by a board array plus game-state fields such as side to move, castling rights, en passant square, clocks, king squares, and Zobrist hash key.
2. The UCI layer receives a `position` and `go` command and constructs a `SearchRequest`.
3. The search module performs iterative deepening from depth 1 upward.
4. Each root move is searched with negamax alpha-beta.
5. When nominal depth reaches zero, the engine continues with quiescence search rather than stopping at raw static evaluation.
6. The evaluation module scores positions using tapered midgame/endgame material, piece-square tables, mobility, and pawn-structure terms.
7. The transposition table is probed before expansion and updated after search results are known.
8. At the end of each completed root iteration, UCI info is emitted with depth, score, nodes, nps, and principal variation.
9. Time management decides whether another full iteration should be started.

## Core design principles

### 1. Correctness first, then speed

The codebase uses explicit types and separate modules to make correctness easier to reason about. Search depends on `makeMove`/`unmakeMove` restoring every part of position state exactly, so that invariant is central to the entire design.

### 2. Side-to-move evaluation convention

Evaluation scores are always returned from the viewpoint of the side to move. Search therefore uses a standard negamax formulation where child scores are negated when returning to the parent.

### 3. Incremental state where it matters

The engine keeps king squares and a Zobrist hash key inside the position state so that expensive recomputation is avoided during search.

### 4. Root-controlled reporting and time management

Only completed root iterations are reported to the GUI as official search results. This avoids misleading interior-node telemetry and gives stable PV/score reporting.

## Main modules and responsibilities

## `Types`

This module defines the engine’s core data structures.

Key ideas:

* `Board` is a flat `sbyte[]` array for efficient indexing and low overhead.
* `Coordinates` represent file/rank pairs explicitly.
* `GameState` stores side to move, castling rights, en passant square, clocks, and the current hash key.
* `KingSquares` stores king locations outside the board for faster check detection.
* `Move` is a lightweight representation used during generation and search.
* `MoveUndo` stores exactly the state needed to reverse a move.
* `Position` combines board, state, and king locations into the full search position.

This module is foundational: every other subsystem depends on these representations.

## `BoardHelpers` and piece-code support

These helper modules provide low-level operations such as:

* piece encoding/decoding
* square access helpers
* color and kind checks
* utility functions used across move generation, evaluation, and search

They act as the engine’s low-level vocabulary.

## `GenerateMoves`

This module is responsible for generating moves from a position.

Typical responsibilities include:

* pseudo-legal movement logic for each piece
* legal move generation with king safety checks
* capture-only generation for quiescence
* support for special moves such as promotion, castling, and en passant

The search module relies on this code for both full-width search and quiescence.

## `MakeMove`

This module applies and reverses moves.

This is one of the most critical parts of the engine because search repeatedly does:

* generate move
* make move
* search child
* unmake move

Responsibilities include:

* updating board contents
* updating side to move
* updating castling rights
* updating en passant square
* updating move clocks
* updating king-square cache
* updating hash key
* returning a `MoveUndo` record for exact restoration

The search code assumes this module is fully reliable. If any field is restored incorrectly, TT probing, legality, repetition-related behavior, and evaluation can all become invalid.

## `Evaluation`

The evaluation module computes a static score for a position.

Current design:

* tapered evaluation between midgame and endgame
* material values by piece type
* piece-square tables for MG and EG
* phase calculation based on remaining non-pawn material
* pseudo-mobility term
* pawn-structure terms such as passed pawn bonuses and isolated/doubled pawn penalties

The evaluation is expressed in white-view internally and then converted to side-to-move view before returning, so it is directly usable by negamax.

This module is intentionally classical and transparent rather than neural or data-driven.

## `TranspositionTable`

This module stores previously searched positions keyed by Zobrist hash.

Responsibilities include:

* probing by current hash key
* storing score, depth, bound type, and best move
* tracking useful hits and replacement statistics
* aging or generation-based freshness across searches

The transposition table serves two main purposes:

1. avoid re-searching already solved positions
2. improve move ordering by retrieving previously good moves

## `Search`

This is the engine’s central control module.

Current search design includes:

* negamax alpha-beta
* iterative deepening at root
* quiescence search at leaf frontier
* transposition-table probe/store logic
* move ordering with TT move and MVV-LVA capture ordering
* PV extraction from the transposition table
* root-only UCI reporting
* time-budget management with soft/hard stopping

### Search flow

A root search begins in `chooseBestMove`.

* The root position is prepared.
* TT generation is advanced for the new search.
* Legal root moves are generated.
* The engine iterates from depth 1 up to target depth or time stop.
* Each completed iteration may update the best move, score, and PV.
* The engine predicts whether another full iteration is likely to fit in the remaining budget before starting it.

### Negamax

`negamax` is the full-width recursive search.

Responsibilities:

* probe TT before expansion
* stop at terminal nodes
* hand off to quiescence at depth 0
* search moves with alpha-beta pruning
* store result back in TT with exact/lower/upper bound semantics

### Quiescence

Quiescence is used instead of stopping at raw evaluation when the nominal depth reaches zero.

Responsibilities:

* compute stand-pat score when not in check
* search forcing continuations, primarily captures
* search all legal evasions if side to move is in check
* reduce horizon effects from unstable tactical positions

## `Uci`

This module handles the engine protocol layer.

Responsibilities include:

* parsing incoming UCI commands
* exposing engine identity and options
* translating search results into UCI `info` and `bestmove` lines
* formatting principal variation moves in UCI notation

A key design choice is that GUI-visible info output is emitted only from completed root iterations, not from interior nodes.

## Position representation

The current board representation is a 1D array rather than a map or 2D board abstraction.

Reasons for this choice:

* lower overhead during search
* simple direct indexing
* better cache locality
* efficient make/unmake operations
* easier integration with hashing and compact move generation

This is a practical engine-oriented representation rather than a domain-modeling representation.

## Hashing strategy

The engine uses Zobrist hashing so each position has a compact incremental key.

The hash key depends on:

* piece placement
* side to move
* castling rights
* en passant state

This key is stored in `GameState` and is expected to be kept synchronized by move make/unmake.

## Search-time control

The engine uses a soft/hard time budget model.

* `HardMs`: absolute stop threshold used deep in the tree
* `SoftMs`: preferred stopping threshold used to avoid starting work unlikely to finish

The root also uses a prediction rule based on prior completed iterations to decide whether starting the next depth is worthwhile.

This is important because iterative deepening is most useful when the engine returns the best move from the deepest fully completed iteration.

## Principal variation handling

The engine reconstructs PV by following TT best-move links from the root position.

This approach is:

* simple
* sufficient for GUI reporting
* compatible with current TT design

A future alternative would be an explicit PV table, but TT-based reconstruction is a reasonable first implementation.

## Current strengths of the design

* clear separation of responsibilities
* standard and understandable search architecture
* efficient enough board/state model for classical engine work
* exact make/unmake workflow rather than rebuilding positions
* explicit root-only reporting model
* extensible evaluation framework

## Likely future enhancements

The current design leaves room for several natural improvements:

* killer/history move ordering
* aspiration windows
* null-move pruning
* late move reductions
* pawn hash table for pawn-structure caching
* stronger king safety evaluation
* rook/file and bishop-pair terms
* repetition detection and draw logic refinement
* more sophisticated time management
* stronger TT replacement policy and packing

## Elegance versus efficiency in the design

A chess engine is a good example of software where elegant modeling and raw efficiency often pull in different directions. Fishy’s design sits in that tension deliberately. The code aims to remain understandable and modular, but some parts of the engine necessarily favor compactness and speed over the most expressive or “beautiful” domain model.

## The core tradeoff

There are two different design ideals in play.

The first is a domain-driven style:

* rich types
* descriptive data structures
* explicit invariants
* straightforward composition
* code that mirrors how a human describes chess

The second is an engine-oriented style:

* compact in-memory structures
* direct indexing
* low allocation
* tight loops
* incremental state updates
* representation chosen for search speed rather than expressiveness

Both are legitimate, but they optimize for different things.

A user-facing chess application or rules engine can prioritize the first. A search-heavy chess engine must increasingly prioritize the second as performance becomes important.

## Where Fishy chooses efficiency over elegance

### Flat board array instead of richer square map structures

A more descriptive design might model the board as a map from squares to pieces, or as a richer immutable structure with domain-specific operations.

Fishy instead uses a flat `sbyte[]` board. That choice is less self-documenting, but it has major practical advantages:

* constant-time indexed access
* low overhead per lookup
* no tree or hash-map indirection
* better cache locality
* simpler make/unmake performance characteristics

This is a classic engine choice. It is less elegant in the domain-modeling sense, but much more appropriate for repeated search-node access.

### Compact piece encoding instead of algebraic data types for pieces

A very expressive design might represent pieces as discriminated unions or records with explicit color and kind fields.

Fishy instead uses compact signed byte encoding. That is harder to read at first glance, but it makes several hot-path operations cheaper:

* color tests become simple sign checks or compact helper calls
* piece kind extraction is cheap
* board storage remains dense
* hashing and move-generation logic become easier to implement efficiently

The cost is that some meaning moves out of the type system and into helper functions and conventions.

### Make/unmake instead of persistent position rebuilding

In a purely functional style, one might prefer to construct a fresh position value for each child node. That is elegant, safe, and easy to reason about locally.

Fishy instead uses make/unmake with an explicit `MoveUndo`. This is less aesthetically pleasing to many F# programmers, but it is the standard engine choice because it avoids large amounts of allocation and copying during search.

The tradeoff is clear:

* rebuilding positions is conceptually cleaner
* make/unmake is far faster in deep search

For an engine, the performance win is usually decisive.

### Incremental state instead of recomputation

Fishy stores king squares and the current Zobrist hash key directly in position state rather than recomputing them from the board whenever needed.

That is slightly less elegant because it introduces representational redundancy: the same “truth” exists both in the board and in cached state.

However, this redundancy is deliberate. It allows:

* faster check detection
* cheaper TT probing
* less repeated board scanning

The price is that move execution must preserve stricter invariants.

## Where Fishy still tries to preserve elegance

### Module boundaries

Even where internal representations are compact and engine-oriented, the codebase still tries to keep responsibilities separated cleanly.

Examples:

* `Types` owns the core state structures
* `GenerateMoves` owns move generation
* `MakeMove` owns state transitions
* `Evaluation` owns static scoring
* `Search` owns tree search and root control
* `Uci` owns protocol formatting and parsing

This is an important kind of elegance: architectural clarity rather than local data-structure purity.

### Explicit types for important concepts

Although the board and pieces are encoded compactly, the design still uses explicit named types for concepts like:

* `Coordinates`
* `Move`
* `MoveUndo`
* `GameState`
* `Position`
* `KingSquares`

This keeps the code from collapsing into unstructured arrays and tuples everywhere.

### Search/evaluation separation

The engine avoids mixing positional scoring logic directly into search logic. That separation is important for maintainability and for future tuning.

### Root-only reporting

The choice to emit GUI-facing search info only from completed root iterations is also a kind of design elegance. It keeps the meaning of `info` output aligned with actual search state instead of leaking interior-node noise into the protocol layer.

## Typical examples of elegance-efficiency tension

### 1. `Coordinates` versus raw square indices

`Coordinates` are more expressive than a raw `int` square index. They communicate intent clearly and make many operations easier to understand.

However, a raw 0..63 square index is often faster and simpler in hot paths. Fishy uses explicit coordinates in some public-facing and structural places, but many engine operations still effectively reduce to indexed square arithmetic.

This is a compromise between readability and speed.

### 2. Value-rich `Move` representation versus packed move integers

A record like `Move = { From; To; Piece; PromoteTo }` is easy to inspect, log, test, and reason about.

But search and TT logic often prefer a compact packed move identity, because:

* comparisons are cheaper
* storage is denser
* TT integration is simpler
* ordering structures become smaller and faster

Fishy therefore keeps a descriptive `Move` structure while also using packed forms where they are useful. This is a pragmatic hybrid.

### 3. General-purpose containers versus fixed arrays

General-purpose containers are often elegant because they are flexible and safe. But in engine internals, flexibility is usually not the right optimization target.

Examples include:

* transposition tables wanting fixed-size arrays instead of dictionaries
* board representation favoring arrays over maps
* evaluation tables favoring precomputed arrays over more abstract lookup layers

These choices may look lower-level, but they usually pay off directly in nodes per second.

## Why engine code often looks less elegant than application code

Search code is dominated by a small number of hot operations repeated millions of times:

* board reads
* move generation
* make/unmake
* attack checks
* hash probes
* move ordering
* evaluation lookups

At that scale, small representation choices matter a great deal. Engine code therefore tends to accept:

* mutation in controlled places
* cached derived state
* numeric encodings
* array indexing
* compact structs

These choices would be overkill or distasteful in many business applications, but they are normal and justified here.

## Where over-optimizing too early would be a mistake

The opposite danger is writing everything in its most compressed and micro-optimized form before the engine is stable.

That usually makes the code:

* harder to debug
* harder to test
* harder to document
* harder to extend
* easier to subtly break

Fishy’s current design avoids that trap by keeping the overall structure clear even when some internal representations are performance-oriented.

That is the right compromise at this stage.

## A practical design rule for Fishy

A useful principle for this engine is:

* prefer clarity in cold code and module interfaces
* prefer efficiency in hot paths and repeatedly executed state transitions

That means:

* search, move generation, hashing, and make/unmake may justifiably look lower-level
* orchestration, protocol handling, documentation, and higher-level structure should remain explicit and readable

This rule helps decide when an “elegant” abstraction is worth keeping and when it is getting in the way.

## A note on bitboards

An even more efficiency-oriented design than Fishy’s current array-based approach would use bitboards.

A bitboard represents a set of squares as a 64-bit integer, with one bit per square. Engines often maintain separate bitboards for:

* each piece type
* each color
* occupancy as a whole
* sometimes derived attack or mask sets

The main advantage is efficiency. Bitboards can make many operations extremely fast:

* attack generation
* occupancy tests
* sliding-piece ray calculations with precomputed tables or magic bitboards
* mobility counting
* pawn-structure analysis
* set-style operations such as union, intersection, and masking

They also tend to be compact and cache-friendly.

The tradeoff is that bitboard code is usually more opaque than array-based code. A board array naturally mirrors “what piece is on this square?” while bitboard logic often expresses chess state in terms of masks, shifts, bit scans, and precomputed attack patterns. That can make the code:

* harder to read initially
* harder to debug without specialized tooling
* less obviously connected to ordinary chess-language concepts

For that reason, bitboards are often stronger on raw efficiency but weaker on immediate clarity. Fishy’s current design stops short of that level of representation complexity, but bitboards remain an important reference point when discussing the broader elegance-versus-efficiency spectrum in chess-engine design.

## Future pressure points

As Fishy grows stronger, the pressure toward efficiency will likely increase in these areas:

* transposition table packing and replacement policy
* pawn-hash caching
* move ordering heuristics
* attack generation
* evaluation caching or incremental features
* more aggressive pruning/reduction logic

Each of those can improve strength and speed, but each also risks making the code less transparent.

The design challenge is not to avoid that pressure entirely, but to contain it within well-defined modules and documented invariants.

## Summary

Fishy’s design reflects a deliberate compromise between elegant F# structure and the practical efficiency demands of chess-engine search. It does not pursue maximal purity or maximal micro-optimization. Instead it uses expressive module boundaries and explicit domain types where possible, while accepting compact encodings, mutation, and incremental state in the hot parts of the engine where performance matters most.

That balance is not a weakness. It is the normal shape of a serious classical engine design.

## Position representation and invariants

The position representation is the most important shared contract in the engine. Search, move generation, evaluation, hashing, and legality checking all assume that a `Position` value is internally consistent. If that consistency is broken, the symptoms may appear far away from the real cause: illegal moves, incorrect check detection, broken transposition-table hits, or unstable search results.

For that reason, it is useful to think of the position layer not just as data storage, but as a set of invariants that every state transition must preserve.

## Main components of a position

A position in Fishy is represented by three closely related structures:

* `Board`
* `GameState`
* `KingSquares`

These are combined in the `Position` record.

### `Board`

`Board` is a flat `sbyte[]` of length 64. Each entry represents the contents of one square.

Reasons for this representation:

* compact storage
* direct indexed access
* predictable performance in hot paths
* straightforward integration with move generation and hashing

The board is the primary source of truth for piece placement.

### `GameState`

`GameState` stores the parts of a chess position that are not captured by raw piece placement alone:

* side to move
* castling rights
* en passant square
* halfmove clock
* fullmove number
* Zobrist hash key

These fields are essential because two positions with identical piece placement can still differ legally and strategically if any of these values differ.

### `KingSquares`

The engine stores king locations separately in `KingSquares` rather than scanning the board whenever king positions are needed.

This is a performance-oriented cache. It speeds up:

* check detection
* legality filtering
* move application involving the king

This separate king cache must always agree with the board.

## Board layout conventions

The board is stored as a 1D array rather than a 2D array. This keeps indexing simple and efficient, but it means the code depends on a consistent square-mapping convention.

The important point is not which exact mapping is used, but that all modules agree on it:

* board indexing helpers
* move generation
* make/unmake
* evaluation PST lookup
* UCI move formatting
* hashing

Any disagreement about square numbering will corrupt behavior immediately.

## Why the engine stores redundant state

Some position information is redundant by design.

Examples:

* the board contains both kings, but king squares are also cached separately
* the board and state together imply a hash key, but the current hash key is stored explicitly

This is a deliberate performance tradeoff. Recomputing these derived values at every node would be more elegant in a purely functional sense, but significantly slower.

The cost of this optimization is that move execution must maintain stronger invariants. Every make/unmake operation must keep board, state, king cache, and hash key synchronized.

## Core invariants

The engine relies on the following invariants being true for every valid `Position`.

### 1. Board and king cache agree

The square recorded in `Kings.WhiteKingSq` must contain the white king.
The square recorded in `Kings.BlackKingSq` must contain the black king.

Conversely, there must not be another king of the same color elsewhere on the board.

If this invariant breaks, check detection and legal move generation become unreliable.

### 2. Hash key matches the current position exactly

`State.HashKey` must correspond to the current:

* piece placement
* side to move
* castling rights
* en passant state

If the hash key is stale or partially updated, then:

* TT probes may hit the wrong positions
* TT stores become misleading
* PV extraction may follow invalid move chains
* move ordering quality degrades

This is one of the highest-impact invariants in the engine.

### 3. En passant square is legal in context

If `State.EPSquare` is present, it must describe a valid current en passant target square, not merely a square that was once relevant in move history.

That means it should be set only when the immediately preceding move created a legal en passant opportunity. Otherwise it should be `ValueNone`.

This matters for both legal move generation and Zobrist hashing.

### 4. Castling rights reflect actual game history state

Castling rights are not derivable from current board placement alone. A king or rook may return to its original square after having moved earlier, but castling would still be illegal.

Therefore `State.CastlingRights` must be updated by move history rules, not by board appearance alone.

This state must also be included in hashing.

### 5. Side to move is correct

`State.ToPlay` determines:

* evaluation viewpoint conversion
  n- legal move generation
* check status interpretation
* time-budget ownership
* Zobrist key side bit

A wrong side-to-move flag corrupts nearly everything downstream.

### 6. Halfmove and fullmove counters remain internally consistent

These clocks do not usually affect core search directly as strongly as other fields, but they still matter for:

* rule correctness
* protocol/state reporting
* potential draw-rule support

They must be updated and restored correctly during make/unmake.

### 7. Board contents use valid piece encodings only

Each board entry must be either:

* `Empty`, or
* a valid piece code consistent with the engine’s encoding scheme

This matters because many hot-path helpers assume that piece codes are well-formed and do not defensively validate them.

## Why `MoveUndo` exists

The design uses make/unmake search rather than rebuilding positions from scratch. That makes exact state restoration essential.

`MoveUndo` exists to capture every state element that may need to be restored after searching a child:

* captured piece
* castling rights
* en passant square
* clocks
* king squares
* hash key

This is not just a convenience record. It is part of the engine’s correctness model.

If `MoveUndo` is incomplete, then `unmakeMove` must recompute or guess missing information, which increases both complexity and risk.

## Position transitions as invariant-preserving operations

A useful way to think about `makeMove` and `unmakeMove` is that they are not merely board mutations. They are invariant-preserving transformations.

A correct `makeMove` must:

* update board occupancy
* remove captured pieces correctly
* handle promotions and en passant correctly
* update king cache when needed
* update castling rights when needed
* update en passant state when needed
* update clocks
* toggle side to move
* update hash key consistently with all of the above

A correct `unmakeMove` must restore all of those fields exactly.

Search performance depends on this process being cheap. Search correctness depends on it being exact.

## Why this layer matters more than it first appears

Many engine bugs that seem to belong to search or evaluation are actually position-invariant bugs.

Examples:

* a transposition-table “bug” may really be a stale hash key
* a move-generation “bug” may really be a bad king-square cache
* a quiescence error may really be an incorrect en passant state
* an illegal castle may come from bad castling-right restoration

That is why the position layer deserves explicit documentation and strong tests.

## Testing implications

The most valuable tests for this layer are invariant-oriented rather than just example-oriented.

Useful categories include:

* make/unmake round-trip tests
* hash restoration tests
* king-square restoration tests
* castling-right transition tests
* en passant creation and clearing tests
* promotion and capture restoration tests
* legal-move consistency after make/unmake cycles

A strong invariant test suite pays for itself because every higher-level module depends on these guarantees.

## Relationship to elegance and efficiency

This section also illustrates the broader design philosophy of the engine.

A more elegant model might derive everything from an immutable board state on demand. Fishy instead accepts:

* cached king squares
* stored hash key
* explicit undo records
* mutable make/unmake workflow

These choices make the representation less minimal, but they are appropriate for search-intensive engine code. The important design discipline is not to avoid redundancy entirely, but to document and enforce the invariants that make the redundancy safe.

## Summary

The position representation in Fishy is intentionally compact and partially incremental. Its power comes from a small set of strong invariants tying board, game-state fields, king cache, and hash key together. Every major engine subsystem depends on those invariants, which makes the position layer the central correctness boundary of the whole design.

## Move generation and legality

Move generation is the bridge between static board state and dynamic search. It answers the question: given the current position, what moves may the side to move actually consider?

In a chess engine, move generation has to satisfy two competing goals:

* generate moves fast enough for large search trees
* respect the full legality rules of chess

Those goals are related but not identical. Fast generation often begins with pseudo-legal movement rules, while full legality requires additional filtering, especially around king safety.

## Two levels of move reasoning

It is useful to distinguish two different concepts.

### Pseudo-legal moves

A pseudo-legal move obeys the movement rules of the piece and local occupancy constraints.

Examples:

* a bishop moves diagonally through empty squares
* a knight jumps in an L-shape
* a pawn captures diagonally
* castling shape rules are satisfied locally

A pseudo-legal move may still be illegal overall if it leaves the moving side in check.

### Legal moves

A legal move is a pseudo-legal move that also preserves king safety and all full chess rules.

Examples of pseudo-legal moves that may still be illegal:

* moving a pinned piece and exposing the king
* castling through check
* moving the king onto an attacked square
* an en passant capture that exposes a rook or bishop attack on the king

Search ultimately requires legal moves, not merely pseudo-legal ones.

## Why legality is a special problem

Chess move rules are mostly local, but legality is global.

A rook move can be checked locally for line clearance. A knight move can be checked locally for shape. But whether the move is allowed may depend on a distant line attack against the king after the board changes.

This means legality cannot be handled purely by piece-wise movement rules. It requires a board-level notion of attack and king exposure.

## Likely move-generation structure

A practical classical engine usually works in one of these ways:

1. generate pseudo-legal moves, then filter illegal ones by make-and-test
2. generate mostly legal moves directly, using check/pin logic to restrict candidates
3. use a hybrid approach depending on the piece and situation

Fishy’s current architecture is most naturally aligned with the hybrid style:

* movement logic belongs in `GenerateMoves`
* board transitions belong in `MakeMove`
* king attack detection belongs in attack helpers
* final legality is tied to the current king state

This separation keeps move generation understandable while still allowing search to consume legal moves directly.

## Special cases that define correctness

### Check evasions

When the side to move is in check, move generation is no longer about ordinary freedom of movement. Only moves that remove check are legal.

Those may include:

* king moves
* capturing the checking piece
* interposing a piece on a checking line

Double check is even more restrictive: in that case only king moves are legal.

This is one of the most important branching-reduction cases in the engine.

### Castling

Castling combines several independent conditions:

* king and rook must retain castling rights
* path squares must be empty
* king must not currently be in check
* king must not pass through an attacked square
* king must not end on an attacked square

This means castling is never just a geometric move. It is a rights-and-attack-sensitive move.

### En passant

En passant is often a source of subtle bugs because legality depends on the transient en passant state and the resulting board after the capture.

In particular, an en passant capture can remove a pawn from one square while moving to another, which may expose a discovered attack on the king. That makes it a classic case where pseudo-legality and legality differ.

### Promotion

Promotion expands one pawn move into multiple move outcomes. A single push or capture to the last rank may produce several legal moves, one per promotion piece.

This affects:

* move generation
* move ordering
* TT move identity
* UCI formatting

## Move ordering and generation are related but distinct

The move generator’s job is to produce valid candidates. The search module then decides in what order to search them.

Fishy keeps these concerns separate. That is important because:

* legality should not depend on heuristic ordering
* ordering heuristics can change without changing move correctness
* capture-only generation can be reused in quiescence

This separation helps search evolve independently from rules correctness.

## Full-width versus quiescence generation

The engine needs at least two move-generation modes.

### Full-width generation

Used in ordinary negamax nodes. Produces the full legal move set.

### Tactical or capture generation

Used in quiescence when the position is not in check. Produces a smaller forcing subset, usually captures and possibly certain promotions.

When the side to move is in check, quiescence cannot remain capture-only. It must generate full legal evasions because check is itself a non-quiet tactical condition.

This distinction is one of the main ways the engine balances tactical fidelity against search cost.

## Why move generation is performance-critical

Move generation is one of the hottest parts of the engine. It is called at nearly every interior node and many quiescence nodes.

That means the code benefits from:

* low-overhead board access
* compact piece encoding
* minimal allocation
* direct attack checks
* inexpensive king-location access

This is one reason the engine’s board and state representation favors practical search speed over a more declarative model.

## What correctness means here

A good move generator is not merely one that avoids illegal moves. It should also avoid missing legal moves.

The two major failure modes are:

* false positives: generating illegal moves
* false negatives: failing to generate legal moves

Both are serious. False positives corrupt search correctness. False negatives silently weaken the engine by removing lines from the tree.

## Testing implications

The best tests for move generation are usually position-based rather than unit-style in isolation.

Useful categories include:

* legal move counts from known positions
* check-evasion positions
* castling legality positions
* en passant legality cases
* promotion branching cases
* pinned-piece cases
* perft-style validation against known counts

Perft remains especially important because it tests move generation and make/unmake together under exhaustive expansion.

## Summary

Move generation in Fishy is the subsystem that turns position state into legal search choices. Its design must reconcile local piece movement rules with global king-safety legality, while remaining efficient enough for deep tree search. Special moves and check-related edge cases define much of its real complexity.

## Make and unmake design

The make/unmake workflow is the operational core of the engine. Search depends on the ability to descend into a child position cheaply and then restore the parent exactly.

This is one of the central places where engine design clearly favors efficiency over a more purely functional style.

## Why make/unmake exists

A search engine explores a very large number of related positions. Those positions usually differ by only one move.

Rebuilding an entire new position object for every child is conceptually clean, but it creates large amounts of repeated copying and allocation. Make/unmake avoids that by reusing a mutable position snapshot and recording just enough information to reverse the change.

The basic search pattern is:

1. choose a move
2. apply it to the current position
3. search the child
4. restore the previous position

That sequence is executed extremely often, so even small inefficiencies matter.

## What make/unmake must handle

A chess move can affect much more than the piece on the from- and to-squares.

Depending on the move, make/unmake may need to update:

* moved piece location
* captured piece removal
* side to move
* castling rights
* en passant square
* halfmove clock
* fullmove number
* king-square cache
* Zobrist hash key
* special rook movement during castling
* special capture/removal behavior during en passant
* promoted piece replacement

That is why move execution is not a trivial board-write operation.

## Why exact reversal matters

Search assumes that after `unmakeMove`, the parent position is bit-for-bit logically identical to what it was before `makeMove`.

Not approximately identical. Exactly identical.

If even one field is restored incorrectly, search may still run, but the corruption can surface later as:

* wrong TT hits
* missed legal moves
* impossible en passant states
* castling rights drift
* inconsistent evaluation
* unstable PV extraction

This is why move execution bugs are among the most damaging engine bugs.

## The role of `MoveUndo`

`MoveUndo` is the compact record of everything needed to reverse a move safely.

Its purpose is not only convenience. It acts as the boundary between forward incremental mutation and exact restoration. By storing prior state explicitly, the engine avoids fragile reconstruction logic during unmake.

A good `MoveUndo` design should prefer restoring old values directly rather than trying to infer them from the post-move position.

## Make/unmake and search performance

The make/unmake path sits directly in the inner search loop, so it needs to be cheap in both directions.

That usually favors:

* direct board mutation
* compact undo data
* constant-time restoration of cached state
* incremental hash updates instead of recomputation

This is also one of the reasons the engine stores king locations and hash key explicitly.

## Interaction with legality

Make/unmake is closely connected to legality testing.

A common legality strategy is:

* generate candidate move
* make it
* test whether own king is attacked
* unmake it

Even when the generator already handles many legal constraints directly, make/unmake remains the clean fallback for difficult cases and for search itself.

This means move execution is not only a search primitive but also part of the legality mechanism.

## Interaction with hashing

Zobrist hashing works best when move execution updates the hash incrementally.

That means make/unmake is responsible for toggling all relevant hash components:

* moved piece from-square and to-square
* captured piece, if any
* side-to-move bit
* castling-rights changes
* en passant changes
* promotion piece replacement

An engine can recompute the hash from scratch for testing, but in real search it should be maintained incrementally.

## Interaction with evaluation

Even without full incremental evaluation, make/unmake shapes the future direction of evaluation design.

If the engine later adds:

* pawn hash key
* incrementally maintained material counts
* piece lists
* attack caches

then move execution becomes the place that preserves those derived structures too.

That is why a disciplined make/unmake design scales well as the engine grows.

## Design discipline in this subsystem

A useful principle is to keep make/unmake simple in interface but strict in internal guarantees.

The interface should answer two questions clearly:

* what changes when a move is made?
* what information must be returned to undo it?

The implementation should avoid hidden side effects outside those guarantees.

That makes the subsystem easier to test and safer to extend.

## Common bug patterns

Typical make/unmake bugs include:

* failing to restore en passant state
* restoring the board but not the hash key
* updating king cache on make but not on unmake
* mishandling rook movement in castling
* removing the wrong pawn in en passant
* restoring clocks incorrectly
* promotion restore bugs where the original pawn is not reinstated

These are precisely the kinds of bugs that can survive casual play testing but fail under deeper search or perft.

## Testing implications

The most valuable tests here are round-trip tests.

Examples:

* make then unmake returns to identical board and state
* make/unmake preserves hash key
* special moves round-trip correctly
* repeated make/unmake over generated legal moves leaves the original position unchanged
* perft positions remain stable under recursive expansion

This subsystem benefits especially from tests that compare the full pre- and post-round-trip position, not just selected fields.

## Relationship to the broader design philosophy

Make/unmake is one of the clearest examples of Fishy’s overall philosophy:

* accept mutation where the performance case is strong
* keep the mutation disciplined and reversible
* document the invariants clearly
* rely on tests to defend the correctness boundary

That is not the most elegant style in a purely functional sense, but it is a standard and justified tradeoff in classical chess-engine implementation.

## Summary

The make/unmake subsystem is the engine’s reversible state-transition mechanism. It exists because search repeatedly explores positions that differ only slightly from one another, and it must do so cheaply. Its design is successful only when it combines speed with exact restoration across all board, state, king-cache, and hash-related effects.

## Search design

The search subsystem is responsible for turning a static position into a concrete move choice. Its job is not merely to rank moves in isolation, but to compare futures: if one move is played and both sides continue to respond optimally within the search horizon, which move leaves the side to move best off?

Fishy uses a classical tree-search design built around negamax, alpha-beta pruning, iterative deepening, quiescence search, and transposition-table support. This is a traditional architecture, but it remains effective because each part solves a different aspect of the search problem.

## Why iterative deepening is the root framework

The engine does not jump directly to a final depth. It searches the root repeatedly:

* first to depth 1
* then to depth 2
* then to depth 3
* and so on

This gives the engine several practical advantages.

First, it always has a completed move to fall back on if time expires.

Second, each shallow search helps the next deeper search. The previous best move often remains strong, so it can be searched first at the next depth.

Third, transposition-table entries from earlier iterations improve move ordering and cutoff quality in deeper searches.

For a timed engine, iterative deepening is not just convenient. It is the basis of usable time control.

## Negamax as the recursive search model

Fishy uses negamax rather than writing separate maximizing and minimizing code paths.

The principle is simple: the score returned from a position is always measured from the viewpoint of the side to move in that position. After making a move, control passes to the opponent, so the child score is negated when returning to the parent.

This keeps the recursive structure compact and uniform. It also fits naturally with the engine’s evaluation convention, where static evaluation is returned in side-to-move terms.

## Alpha-beta pruning

A naïve minimax search examines far too many nodes to be practical. Alpha-beta pruning improves this by eliminating branches that cannot affect the final result.

At each node, the engine maintains a search window:

* `alpha`: the best score already proven for the side to move
* `beta`: the upper threshold beyond which the opponent would already reject the line

If the engine finds a move that proves the position is at least as good as `beta`, it can stop searching the remaining siblings at that node. Those moves are irrelevant because the opponent would never allow the line to be reached.

The effectiveness of alpha-beta depends heavily on move ordering. Good moves searched early produce earlier cutoffs and much smaller trees.

## Root search versus interior search

Root search behaves differently from ordinary interior nodes.

At the root:

* every move is a candidate for the final answer
* the engine reports completed iteration results to the GUI
* time-management decisions are made
* previous root best move is an especially valuable ordering signal

Inside the tree, the focus shifts from reporting to efficiency:

* TT cutoffs are attempted
* move ordering is applied to improve pruning
* the engine stores bounds rather than user-facing analysis output

This difference is why GUI reporting is best handled only at the root rather than in recursive search.

## Quiescence search and the horizon problem

If the engine stopped at fixed depth and immediately evaluated the leaf, it would often misjudge tactical positions. A leaf reached just before an obvious capture, recapture, or checking sequence is not really quiet.

Quiescence search extends such positions beyond the nominal depth limit. Instead of stopping immediately, the engine continues searching forcing continuations until the position becomes stable enough for static evaluation to make sense.

In Fishy’s current design:

* stand-pat evaluation is used when the side to move is not in check
* capture sequences are explored to resolve tactical instability
* if the side to move is in check, quiescence must search legal evasions rather than only captures

This allows the engine to reduce horizon effects without exploding the full-width tree everywhere.

## Transposition-table integration

Many different move orders can lead to the same position. Search strength improves substantially when the engine can recognize that it has already seen and evaluated such a position.

Fishy probes the transposition table before expanding a node. If a stored entry is deep enough and its bound type proves the current node’s result, the search can reuse it immediately.

The TT also contributes to move ordering by storing a previously good move for the position. Searching that move early often improves pruning even when the stored score itself cannot be used directly.

This gives the TT a dual role:

* result reuse
  n- move-ordering guidance

## Bound semantics

A TT entry is not always a fully exact score.

Fishy distinguishes among three kinds of stored information:

* exact values
* lower bounds
* upper bounds

An exact value means the node’s true score is known for the searched depth.

A lower bound usually comes from a fail-high or beta cutoff: the position is at least this good.

An upper bound usually comes from a fail-low: the position is at most this good.

This distinction matters because not every stored value is safe to reuse in every search window.

## Move ordering strategy

The efficiency of alpha-beta depends strongly on the order in which moves are searched.

Fishy’s current ordering strategy is intentionally simple but effective:

* transposition-table move first when available
* captures before quiet moves
* captures ordered by MVV-LVA
* previous root best move promoted at the next iteration

This gives most of the basic benefits of move ordering without introducing more advanced heuristics too early.

The design leaves room for future additions such as killers, history heuristics, aspiration windows, or late move reductions.

## Principal variation reconstruction

After a completed root iteration, the engine reconstructs a principal variation by following stored TT moves from the root position.

This is a practical way to expose a PV to the GUI without maintaining a separate PV table. It is not perfect in every circumstance, but it is simple and works well enough for current reporting needs.

The important design idea is that the PV shown to the GUI should correspond to a completed root result, not a transient interior-node estimate.

## Time management as a search decision

Time management is not a separate concern bolted onto the side of search. It directly affects how deep the engine can search and whether a partially explored deeper iteration is worth starting.

Fishy uses a soft/hard budget model.

* the hard limit is an emergency stop used inside the tree
* the soft limit governs whether another completed iteration should be attempted

At the root, the engine also estimates whether the next iteration is likely to finish in time based on the growth of previous completed depths. This prevents wasting much of the remaining budget on an iteration that is unlikely to complete.

## What makes this search architecture robust

The design is robust because each part is narrowly scoped:

* negamax handles recursive game-tree logic
* alpha-beta reduces the tree
* quiescence stabilizes tactical leaves
* iterative deepening organizes root search
* the TT provides reuse and ordering hints
* root control handles reporting and time decisions

No single technique solves the search problem alone. Their value comes from their interaction.

## Future search extensions

The current architecture is a good platform for additional strength-oriented improvements, including:

* aspiration windows
* killer/history ordering
* null-move pruning
* late move reductions
* more selective quiescence filtering
* stronger TT packing and replacement policies
* repetition and draw-rule integration

Because the search design is already modular, such additions can be made incrementally rather than all at once.

## Summary

Fishy’s search subsystem is a classical depth-first game-tree search organized around iterative deepening. It combines negamax, alpha-beta pruning, quiescence search, transposition-table guidance, and root-based time control to produce a practical move choice under real search constraints.

## Evaluation design

The evaluation subsystem supplies the search with a static judgement of a position when tactical expansion stops. In a classical engine, evaluation is not expected to solve the position outright. Its role is to measure positional features that search cannot always reach directly and to give the search a useful ordering of strategically plausible outcomes.

Fishy currently uses a transparent hand-crafted evaluator rather than a neural one. This keeps the scoring logic understandable, tunable, and closely tied to ordinary chess concepts.

## Tapered evaluation

The evaluator is split into two regimes:

* midgame
* endgame

Rather than switching abruptly from one to the other, Fishy interpolates between them using a phase calculation based on remaining non-pawn material.

This means a knight on a central square, for example, can have one positional value in the middlegame and another in the endgame, with the final score blended according to how much material remains.

This approach is valuable because many chess features do not have fixed importance across the whole game. King placement is the clearest example: king exposure is dangerous in the middlegame, while king activity becomes important in the endgame.

## Material as the base layer

Material remains the foundation of the evaluator. Each piece type has explicit middlegame and endgame values, and those values are part of the precomputed piece-square tables used during evaluation.

This gives the evaluator a stable baseline:

* being up a rook matters everywhere
* being down a queen matters everywhere
* positional features are interpreted relative to this material structure

In a classical evaluator, material is usually not the whole story, but it is the framework within which all other terms operate.

## Piece-square tables

Fishy uses piece-square tables for both middlegame and endgame. These tables encode square preferences for each piece type.

Examples of what PSTs capture well:

* knight centralization
* bishop activity on long diagonals
* rook activity on open or advanced ranks in coarse form
* king shelter in the middlegame
* king centralization in the endgame

PSTs are useful because they are cheap to apply and encode a great deal of positional knowledge compactly. They are especially effective as a first positional layer on top of raw material.

At the same time, PSTs are inherently local. They know where a piece stands, not how that piece interacts with the rest of the board. That is why additional structural terms are still needed.

## Mobility term

Fishy includes a mobility component based on pseudo-mobility rather than legal move generation. This is a deliberate efficiency choice.

The evaluator counts the freedom of non-king pieces to access squares under simple occupancy rules rather than invoking full legal move generation. Pawns are treated specially, with mobility modeled through capture directions rather than quiet forward movement.

Mobility matters because active pieces usually correlate with stronger positions. A cramped side with restricted minor and major pieces is often strategically worse even before any concrete tactical loss appears.

The evaluator uses mobility as a general activity signal rather than a precise legal-move count.

## Pawn-structure terms

Pawn structure changes more slowly than piece placement and often determines the long-term character of a position. Fishy therefore supplements PSTs and mobility with explicit pawn-structure terms.

Current examples include:

* passed pawn bonuses
* isolated pawn penalties
* doubled pawn penalties

These features are especially important because search does not always reach the long-term consequences of pawn weaknesses or advancing passers. A passed pawn several ranks from promotion may already deserve significant credit even if promotion lies beyond the current search horizon.

Pawn structure is also a natural candidate for future caching or dedicated pawn hashing because it is relatively stable across many nodes.

## White-view internal scoring

Internally, the evaluator accumulates many terms from White’s perspective. Piece-square tables for black pieces are stored as mirrored negatives so that board traversal can build a single white-view total.

At the end, that score is converted to side-to-move viewpoint before returning to search.

This separation is useful:

* white-view accumulation makes many static terms simple to express
* side-to-move conversion keeps search negamax-ready

It preserves both implementation convenience and search correctness.

## Precomputation as an efficiency technique

Fishy precomputes combined material-plus-PST tables for each piece type and square. That means evaluation can look up a square contribution directly rather than repeatedly recomputing:

* piece material value
* square-table offset
* black-side mirroring logic

This is a small but worthwhile optimization because evaluation is called at many nodes, including quiescence leaves.

It also keeps the evaluation loop compact and predictable.

## What the current evaluator does well

The present design has several strengths.

It is:

* classical and explainable
* phase-aware
* cheap enough to use frequently
* modular enough to extend
* strong enough to give search nontrivial positional guidance

Most importantly, it is not just a pile of ad hoc bonuses. It has a clear structure:

* material and PSTs as the base
* phase interpolation across game stages
* mobility as an activity signal
* pawn structure as a medium- and long-term structural signal

## Current limits of the evaluator

Like most early classical evaluators, the present design still omits several important relational features.

Examples include:

* explicit king-safety modelling
* bishop-pair bonus
* rook open-file and 7th-rank terms
* outpost bonuses
* more refined passed-pawn logic
* drawishness or endgame scaling rules

These omissions do not make the evaluator unusable. They simply define its current horizon of positional understanding.

In practice, this means search still carries a large share of the burden in positions where king safety, file control, or long-term piece coordination matter strongly.

## Evaluation and search are partners

A static evaluator should not be judged in isolation from the search.

A stronger evaluator helps search by:

* ordering positions more sensibly at leaves
* reducing horizon-related distortion
* improving the quality of quiet-position judgement

A stronger search helps the evaluator by:

* resolving short tactical issues before static judgement is applied
* letting the evaluator focus on medium-term features rather than immediate tactics

This partnership is especially visible in Fishy’s use of quiescence: the evaluator is trusted most after the tactical surface of the position has been partially stabilized.

## Why the evaluator remains hand-crafted

A hand-crafted evaluator is easier to document, inspect, and tune feature by feature. For a project like Fishy, that has several advantages:

* changes are explainable in chess terms
* regressions can often be traced to a specific feature
* new ideas can be added incrementally
* the design remains educational rather than opaque

This does not imply that a neural approach is inferior in absolute strength. It simply means the current design prioritizes transparency and controlled growth.

## Future evaluation directions

The evaluation framework is well suited to incremental refinement. Natural next steps include:

* explicit king-safety terms
* bishop-pair bonus
* rook open/semi-open file bonuses
* protected and connected passed pawns
* outpost logic for knights and bishops
* endgame scaling and drawishness rules
* pawn-hash caching for structural terms

These would deepen the evaluator’s positional understanding without requiring a change in its overall architecture.

## Summary

Fishy’s evaluation subsystem is a classical tapered evaluator built from material, piece-square tables, mobility, and pawn-structure features. Its role is to provide a fast, interpretable positional judgement that complements rather than replaces tactical search.

## Summary

Fishy is designed as a classical F# chess engine with a practical balance between clarity and performance. It uses a compact position representation, recursive alpha-beta search with quiescence and TT support, a classical tapered evaluator, and a UCI-facing root search loop that controls reporting and time usage. The system is modular enough to document, test, and improve incrementally while remaining close to standard engine architecture.

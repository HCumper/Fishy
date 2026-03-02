module MakeMove

open System
open Types
open BoardHelpers
open PieceCode
open Board
open CastlingRights
open Zobrist

// Coordinates are 0-based throughout: File/Rank = 0..7
// White pawns move Rank+1, black pawns move Rank-1

let inline private absInt (x:int) = if x < 0 then -x else x
let inline private isPawnCode (p:sbyte) = absKind p = Pawn
let inline private isKingCode (p:sbyte) = absKind p = King

let inline private sameSq (a:Coordinates) (b:Coordinates) =
    a.File = b.File && a.Rank = b.Rank

let inline private otherColor (c:Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

// 0-based byte file/rank -> 0-based int indices for Zobrist tables
let inline private f0 (f:byte) = int f
let inline private r0 (r:byte) = int r

let inline private toggleSq (p:sbyte) (sq:Coordinates) (h:int64) : int64 =
    togglePiece p (f0 sq.File) (r0 sq.Rank) h

let inline private toggleCastleMask (cr:byte) (h:int64) : int64 =
    let mutable hh = h
    if hasWK cr then hh <- toggleCastleWK hh
    if hasWQ cr then hh <- toggleCastleWQ hh
    if hasBK cr then hh <- toggleCastleBK hh
    if hasBQ cr then hh <- toggleCastleBQ hh
    hh

let inline private toggleEPFileFrom (ep: ValueOption<Coordinates>) (h:int64) : int64 =
    match ep with
    | ValueSome sq -> toggleEpFile (f0 sq.File) h  // EP hashed by file only
    | ValueNone -> h

// If you want strict repetition semantics, only hash EP when an EP capture is actually legal.
// For now: hash EP whenever EPSquare is set.
let inline private epRelevant (_board:Board) (_st:GameState) (_ep:Coordinates) = true

let inline private toggleEPRelevant (board:Board) (st:GameState) (ep:ValueOption<Coordinates>) (h:int64) : int64 =
    match ep with
    | ValueSome sq when epRelevant board st sq -> toggleEPFileFrom ep h
    | _ -> h

/// Make a move on `pos` (mutates board; returns undo info).
/// Caller is responsible for ensuring `mv` is pseudo-legal.
let makeMove (pos: byref<Position>) (mv: Move) : MoveUndo =
    let board = pos.Board
    let st = pos.State

    // Read pieces from board
    let movingPiece = getC board mv.From
    let destPiece   = getC board mv.To

    // Determine EP capture (pawn diagonal to empty square onto current EPSquare)
    let isPawn = isPawnCode movingPiece
    let isDiag = mv.From.File <> mv.To.File
    let isEP =
        isPawn
        && isDiag
        && destPiece = Empty
        && (match st.EPSquare with ValueSome ep -> sameSq ep mv.To | ValueNone -> false)

    // Determine captured piece and EP captured square (if any)
    let capturedPiece, epCapturedSq =
        if isEP then
            let capSq = { File = mv.To.File; Rank = mv.From.Rank } // captured pawn sits on (toFile, fromRank)
            let capP  = getC board capSq
            capP, ValueSome capSq
        else
            destPiece, ValueNone

    // Save undo info (previous state)
    let undo : MoveUndo =
        { CapturedPiece = capturedPiece
          PreviousCastlingRights = st.CastlingRights
          PreviousEPSquare = st.EPSquare
          PreviousHalfMoveClock = st.HalfMoveClock
          PreviousFullMoveNumber = st.FullMoveNumber
          PreviousKingSquares = pos.Kings
          PreviousHashKey = st.HashKey }

    // --- Update castling rights (must use pre-move capturedPiece and squares) ---
    let rights' =
        updateOnMove movingPiece mv.From mv.To capturedPiece st.CastlingRights

    // --- Update clocks ---
    let halfMove' =
        if isPawn || capturedPiece <> Empty then 0uy
        else byte (int st.HalfMoveClock + 1)

    let fullMove' =
        match st.ToPlay with
        | Color.Black -> st.FullMoveNumber + 1us
        | _ -> st.FullMoveNumber

    // --- Update EP square (only for double pawn pushes) ---
    let ep' =
        if isPawn && mv.From.File = mv.To.File then
            let r0v = int mv.From.Rank
            let r1v = int mv.To.Rank
            if absInt (r1v - r0v) = 2 then
                let midRank = (r0v + r1v) / 2
                ValueSome { File = mv.From.File; Rank = byte midRank }
            else
                ValueNone
        else
            ValueNone

    // --- Detect castling (0-based geometry) ---
    // King e-file is 4; O-O to g-file 6; O-O-O to c-file 2; back ranks 0/7.
    let isCastle =
        isKingCode movingPiece
        && mv.From.File = 4uy
        && (mv.To.File = 6uy || mv.To.File = 2uy)
        && ((mv.From.Rank = 0uy && mv.To.Rank = 0uy) || (mv.From.Rank = 7uy && mv.To.Rank = 7uy))

    // Place piece on destination (handle promotion)
    let placedPiece =
        if mv.PromoteTo <> 0y then mv.PromoteTo else movingPiece

    // =========================
    // Zobrist incremental update
    // =========================
    let mutable h = st.HashKey

    // Remove old EP, remove old castling, flip side-to-move
    h <- toggleEPRelevant board st st.EPSquare h
    h <- toggleCastleMask st.CastlingRights h
    h <- toggleBlackToMove h

    // Moving piece leaves from-square
    h <- toggleSq movingPiece mv.From h

    // Captures
    match epCapturedSq with
    | ValueSome capSq ->
        // EP capture: captured pawn removed from capSq
        h <- toggleSq capturedPiece capSq h
    | ValueNone ->
        if capturedPiece <> Empty then
            // Normal capture: captured piece removed from destination
            h <- toggleSq capturedPiece mv.To h

    // Castling rook move in hash (read rook from board BEFORE board mutation)
    if isCastle then
        let rank = mv.From.Rank
        if mv.To.File = 6uy then
            // kingside: rook h->f (7 -> 5)
            let rookFrom = { File = 7uy; Rank = rank }
            let rookTo   = { File = 5uy; Rank = rank }
            let rookPiece = getC board rookFrom
            h <- toggleSq rookPiece rookFrom h
            h <- toggleSq rookPiece rookTo h
        else
            // queenside: rook a->d (0 -> 3)
            let rookFrom = { File = 0uy; Rank = rank }
            let rookTo   = { File = 3uy; Rank = rank }
            let rookPiece = getC board rookFrom
            h <- toggleSq rookPiece rookFrom h
            h <- toggleSq rookPiece rookTo h

    // Placed piece arrives at destination (promotion handled)  **always**
    h <- toggleSq placedPiece mv.To h

    // Add new castling rights
    h <- toggleCastleMask rights' h

    // Add new EP (policy must match hashPosition)
    let stAfterForEp =
        { st with
            CastlingRights = rights'
            EPSquare = ep'
            HalfMoveClock = halfMove'
            FullMoveNumber = fullMove'
            ToPlay = otherColor st.ToPlay
            HashKey = 0L }
    h <- toggleEPRelevant board stAfterForEp ep' h

    // --- Apply board changes (mutate board) ---
    setC board mv.From Empty

    // EP: remove the captured pawn from its square
    match epCapturedSq with
    | ValueSome capSq -> setC board capSq Empty
    | ValueNone -> ()

    // Castling: move the rook as well (0-based)
    if isCastle then
        let rank = mv.From.Rank
        if mv.To.File = 6uy then
            // kingside: rook h->f (7 -> 5)
            let rookFrom = { File = 7uy; Rank = rank }
            let rookTo   = { File = 5uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece
        else
            // queenside: rook a->d (0 -> 3)
            let rookFrom = { File = 0uy; Rank = rank }
            let rookTo   = { File = 3uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece

    // Place piece on destination
    setC board mv.To placedPiece

    // --- Update king squares if king moved ---
    let kings' =
        if isKingCode movingPiece then
            if isWhite movingPiece then { pos.Kings with WhiteKingSq = mv.To }
            else { pos.Kings with BlackKingSq = mv.To }
        else
            pos.Kings

    // --- Update state ---
    let st' =
        { st with
            CastlingRights = rights'
            EPSquare = ep'
            HalfMoveClock = halfMove'
            FullMoveNumber = fullMove'
            ToPlay = otherColor st.ToPlay
            HashKey = h }

    pos <- { pos with State = st'; Kings = kings' }
    undo

/// Unmake a move previously applied by makeMove.
/// Restores board, state, and king squares using MoveUndo.
/// Coordinates are 0-based (File/Rank = 0..7).
let unmakeMove (pos: byref<Position>) (mv: Move) (undo: MoveUndo) : unit =
    let board = pos.Board

    // After a move, ToPlay was flipped. The mover is the opposite of current.
    let moverColor = otherColor pos.State.ToPlay

    // Determine the piece that must return to mv.From.
    // Promotions revert to a pawn of the mover's color.
    let movingBackPiece : sbyte =
        if mv.PromoteTo <> 0y then
            make moverColor Pawn
        else
            getC board mv.To

    // Detect castling by king move e->g/c on back rank (0-based)
    let isCastle =
        isKingCode movingBackPiece
        && mv.From.File = 4uy
        && (mv.To.File = 6uy || mv.To.File = 2uy)
        && ((mv.From.Rank = 0uy && mv.To.Rank = 0uy) || (mv.From.Rank = 7uy && mv.To.Rank = 7uy))

    // Detect EP capture using the previous EP square stored in undo
    let isEP =
        isPawnCode movingBackPiece
        && mv.From.File <> mv.To.File
        && undo.CapturedPiece <> Empty
        && (match undo.PreviousEPSquare with
            | ValueSome ep -> sameSq ep mv.To
            | ValueNone -> false)

    // Undo castling rook move first
    if isCastle then
        let rank = mv.From.Rank
        if mv.To.File = 6uy then
            // kingside undo: rook f->h (5 -> 7)
            let rookFrom = { File = 5uy; Rank = rank }
            let rookTo   = { File = 7uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece
        else
            // queenside undo: rook d->a (3 -> 0)
            let rookFrom = { File = 3uy; Rank = rank }
            let rookTo   = { File = 0uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece

    // Restore mover's piece to from-square
    setC board mv.From movingBackPiece

    // Restore captured piece and destination square
    if isEP then
        // EP capture: destination square was empty before; captured pawn sits on (toFile, fromRank)
        setC board mv.To Empty
        let capSq = { File = mv.To.File; Rank = mv.From.Rank }
        setC board capSq undo.CapturedPiece
    else
        // Normal move/capture (including promotion captures)
        setC board mv.To undo.CapturedPiece

    // Restore state and king squares exactly (including HashKey)
    let prevState =
        { pos.State with
            CastlingRights = undo.PreviousCastlingRights
            EPSquare = undo.PreviousEPSquare
            HalfMoveClock = undo.PreviousHalfMoveClock
            FullMoveNumber = undo.PreviousFullMoveNumber
            HashKey = undo.PreviousHashKey
            ToPlay = moverColor }

    pos <- { pos with State = prevState; Kings = undo.PreviousKingSquares }
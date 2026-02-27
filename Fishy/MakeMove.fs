module MakeMove

open Types
open BoardHelpers
open PieceCode
open Board
open CastlingRights

// Assumes 1-based Coordinates: File/Rank = 1..8
// Assumes white pawns move Rank+1, black pawns move Rank-1

let inline private absInt (x:int) = if x < 0 then -x else x
let inline private isPawnCode (p:sbyte) = absKind p = Pawn
let inline private isKingCode (p:sbyte) = absKind p = King

let inline private sameSq (a:Coordinates) (b:Coordinates) =
    a.File = b.File && a.Rank = b.Rank

let inline private coord (file:int) (rank:int) : Coordinates =
    { File = byte file; Rank = byte rank }

let inline private toInt (b:byte) = int b

let inline private otherColor (c:Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

/// Make a move on `pos` (mutates board; returns undo info).
/// Caller is responsible for ensuring `mv` is pseudo-legal (piece exists, correct side, etc.).
let makeMove (pos: byref<Position>) (mv: Move) : MoveUndo =
    let board = pos.Board
    let st = pos.State

    // Read pieces from board (trust board over mv.Piece if mismatch)
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
            let capSq = { File = mv.To.File; Rank = mv.From.Rank } // EP captured pawn sits on to-file, from-rank
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
            let r0 = toInt mv.From.Rank
            let r1 = toInt mv.To.Rank
            if absInt (r1 - r0) = 2 then
                // square passed over
                let midRank = (r0 + r1) / 2
                ValueSome { File = mv.From.File; Rank = byte midRank }
            else
                ValueNone
        else
            ValueNone

    // --- Detect castling (king e1->g1/c1 or e8->g8/c8) ---
    let isCastle =
        isKingCode movingPiece
        && mv.From.File = 5uy
        && (mv.To.File = 7uy || mv.To.File = 3uy)
        && ((mv.From.Rank = 1uy && mv.To.Rank = 1uy) || (mv.From.Rank = 8uy && mv.To.Rank = 8uy))

    // --- Apply board changes ---
    // Remove moving piece from from-square
    setC board mv.From Empty

    // EP: remove the captured pawn from its square
    match epCapturedSq with
    | ValueSome capSq -> setC board capSq Empty
    | ValueNone -> ()

    // Castling: move the rook as well
    if isCastle then
        let rank = mv.From.Rank
        if mv.To.File = 7uy then
            // kingside: rook h->f
            let rookFrom = { File = 8uy; Rank = rank }
            let rookTo   = { File = 6uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece
        else
            // queenside: rook a->d
            let rookFrom = { File = 1uy; Rank = rank }
            let rookTo   = { File = 4uy; Rank = rank }
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece

    // Place piece on destination (handle promotion)
    let placedPiece =
        if mv.PromoteTo <> 0y then mv.PromoteTo else movingPiece

    setC board mv.To placedPiece

    // --- Update king squares if king moved ---
    let kings' =
        if isKingCode movingPiece then
            if isWhite movingPiece then { pos.Kings with WhiteKingSq = mv.To }
            else { pos.Kings with BlackKingSq = mv.To }
        else
            pos.Kings

    // --- Update state ---
    // HashKey: left unchanged here (wire in your zobrist updater when available)
    let st' =
        { st with
            CastlingRights = rights'
            EPSquare = ep'
            HalfMoveClock = halfMove'
            FullMoveNumber = fullMove'
            ToPlay = otherColor st.ToPlay
            HashKey = st.HashKey }

    // Write back updated position (board already mutated)
    pos <- { pos with State = st'; Kings = kings' }

    undo
    

/// Unmake a move previously applied by makeMove.
/// Restores board, state, and king squares using MoveUndo.
/// Assumes Coordinates are 1-based (File/Rank = 1..8).
let unmakeMove (pos: byref<Position>) (mv: Move) (undo: MoveUndo) : unit =
    let board = pos.Board

    // After a move, ToPlay was flipped. The side who originally moved is the opposite of current.
    let moverColor = otherColor pos.State.ToPlay

    // Determine the piece code that must return to mv.From.
    // Promotions must revert to a pawn of the mover's color.
    let movingBackPiece : sbyte =
        if mv.PromoteTo <> 0y then
            make moverColor Pawn
        else
            // Normal: piece currently on mv.To is the mover's piece
            getC board mv.To

    // Detect castling by king move e-file to g/c on back rank.
    let isCastle =
        isKingCode movingBackPiece
        && mv.From.File = 5uy
        && (mv.To.File = 7uy || mv.To.File = 3uy)
        && ((mv.From.Rank = 1uy && mv.To.Rank = 1uy) || (mv.From.Rank = 8uy && mv.To.Rank = 8uy))

    // Detect EP capture using the *previous* EP square stored in undo.
    let isEP =
        isPawnCode movingBackPiece
        && mv.From.File <> mv.To.File
        && undo.CapturedPiece <> Empty
        && (match undo.PreviousEPSquare with
            | ValueSome ep -> sameSq ep mv.To
            | ValueNone -> false)
        && getC board mv.To <> Empty // pawn is currently on destination after makeMove

    // Undo castling rook move first (so squares are consistent)
    if isCastle then
        let rank = mv.From.Rank
        if mv.To.File = 7uy then
            // kingside: rook f->h (it was moved h->f)
            let rookFrom = { File = 6uy; Rank = rank }  // f-file
            let rookTo   = { File = 8uy; Rank = rank }  // h-file
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece
        else
            // queenside: rook d->a (it was moved a->d)
            let rookFrom = { File = 4uy; Rank = rank }  // d-file
            let rookTo   = { File = 1uy; Rank = rank }  // a-file
            let rookPiece = getC board rookFrom
            setC board rookFrom Empty
            setC board rookTo rookPiece

    // Restore the mover's piece to the from-square
    setC board mv.From movingBackPiece

    // Restore captured piece and destination square
    if isEP then
        // EP capture: destination square was empty before move; captured pawn sits on (toFile, fromRank)
        setC board mv.To Empty
        let capSq = { File = mv.To.File; Rank = mv.From.Rank }
        setC board capSq undo.CapturedPiece
    else
        // Normal move/capture (including promotion captures)
        setC board mv.To undo.CapturedPiece

    // Restore state and king squares exactly as they were
    let prevState =
        { pos.State with
            CastlingRights = undo.PreviousCastlingRights
            EPSquare = undo.PreviousEPSquare
            HalfMoveClock = undo.PreviousHalfMoveClock
            FullMoveNumber = undo.PreviousFullMoveNumber
            HashKey = undo.PreviousHashKey
            ToPlay = moverColor }

    pos <- { pos with State = prevState; Kings = undo.PreviousKingSquares }
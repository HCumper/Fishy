module Fen

open System
open Types
open BoardHelpers.CastlingRights
open BoardHelpers.Coordinates

/// Split a FEN string into its 6 space-separated fields, trimming and removing extra spaces.
let private splitFen (fen: string) =
    fen.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

/// Parse the "side to move" FEN field ("w"/"b") into a Color.
let private parseSideToMove (s: string) : ValueOption<Color> =
    match s with
    | "w" -> ValueSome Color.White
    | "b" -> ValueSome Color.Black
    | _   -> ValueNone

/// Try to parse an unsigned 16-bit integer from a string, returning ValueNone on failure.
let private tryParseUInt16 (s: string) : ValueOption<uint16> =
    match UInt16.TryParse(s) with
    | true, v -> ValueSome v
    | _       -> ValueNone

/// Try to parse a byte (0..255) from a string, returning ValueNone on failure.
let private tryParseByte (s: string) : ValueOption<byte> =
    match Byte.TryParse(s) with
    | true, v -> ValueSome v
    | _       -> ValueNone

/// Parse the en-passant target square field:
/// "-" => ValueSome ValueNone; "e3" style => ValueSome (ValueSome Coordinates); invalid => ValueNone.
let private tryParseEPSquare (s: string) : ValueOption<ValueOption<Coordinates>> =
    // Returns ValueSome(ValueNone) for "-" (valid, no ep)
    // Returns ValueSome(ValueSome coord) for a square
    // Returns ValueNone for invalid
    if s = "-" then ValueSome ValueNone
    elif s.Length = 2 then
        let fileChar = s.[0]
        let rankChar = s.[1]

        let file =
            match fileChar with
            | c when c >= 'a' && c <= 'h' -> int c - int 'a' + 1
            | c when c >= 'A' && c <= 'H' -> int c - int 'A' + 1
            | _ -> -1
        let rank =
            match rankChar with
            | c when c >= '1' && c <= '8' -> int c - int '0'
            | _ -> -1

        match BoardHelpers.Coordinates.tryCreate file rank with
        | ValueSome c -> ValueSome (ValueSome c)
        | ValueNone -> ValueNone
    else
        ValueNone

/// Set every square in the board array to Empty.
let private clearBoard (board: Board) =
    for f = 0 to 7 do
        for r = 0 to 7 do
            board.[f, r] <- BoardHelpers.PieceCode.Empty

/// Parse the piece placement field (ranks 8..1) into the provided board and return king squares if valid.
let private tryParsePlacement (placement: string) (board: Board) : ValueOption<KingSquares> =
    // FEN placement runs from rank 8 down to rank 1
    // Our Coordinates are 1..8 with Rank=1 as first rank.
    clearBoard board

    let mutable file = 1
    let mutable rank = 8

    let mutable wkOpt : ValueOption<Coordinates> = ValueNone
    let mutable bkOpt : ValueOption<Coordinates> = ValueNone

    /// Write a piece code into the 0-based board array using 1-based file/rank.
    let inline setSq (f:int) (r:int) (p:sbyte) =
        // board is 0-based array, coords are 1-based
        board.[f - 1, r - 1] <- p

    /// Track white/black king coordinates while parsing placement.
    let inline recordKing (p:sbyte) (f:int) (r:int) =
        if p = 6y then wkOpt <- ValueSome { File = byte f; Rank = byte r }
        elif p = -6y then bkOpt <- ValueSome { File = byte f; Rank = byte r }

    let mutable ok = true

    for ch in placement do
        if not ok then () else

        match ch with
        | '/' ->
            if file <> 9 then ok <- false
            else
                file <- 1
                rank <- rank - 1
                if rank < 1 then ok <- false

        | c when c >= '1' && c <= '8' ->
            let n = int ch - int '0'
            if file + n - 1 > 8 then ok <- false
            else
                // skip n empties
                file <- file + n

        | _ ->
            match BoardHelpers.PieceCode.tryOfFenChar ch with
            | ValueNone -> ok <- false
            | ValueSome p ->
                if file > 8 || rank < 1 then ok <- false
                else
                    setSq file rank p
                    recordKing p file rank
                    file <- file + 1

    // After reading, must end exactly at file=9 and rank=1 (or rank>=1 with correct slashes)
    if ok && file = 9 && rank = 1 then
        match wkOpt, bkOpt with
        | ValueSome wk, ValueSome bk ->
            ValueSome { WhiteKingSq = wk; BlackKingSq = bk }
        | _ ->
            // invalid position if a king is missing
            ValueNone
    else
        ValueNone

/// Parse a full FEN string into a Position (board + state + kings).
/// HashKey is set to 0L here - compute after load
let tryLoadPositionFromFen (board: Board) (fen: string) : ValueOption<Position> =
    let parts = splitFen fen
    if parts.Length <> 6 then ValueNone
    else
        let placement, stm, castling, ep, halfmove, fullmove =
            parts.[0], parts.[1], parts.[2], parts.[3], parts.[4], parts.[5]

        match tryParsePlacement placement board with
        | ValueNone -> ValueNone
        | ValueSome kings ->

            match parseSideToMove stm with
            | ValueNone -> ValueNone
            | ValueSome toPlay ->

                match BoardHelpers.PieceCode.tryOfFen  castling with
                | ValueNone -> ValueNone
                | ValueSome cr ->

                    match tryParseEPSquare ep with
                    | ValueNone -> ValueNone
                    | ValueSome epSq ->

                        match tryParseByte halfmove with
                        | ValueNone -> ValueNone
                        | ValueSome hmc ->

                            match tryParseUInt16 fullmove with
                            | ValueNone -> ValueNone
                            | ValueSome fmn ->

                                let state =
                                    { HashKey = 0L
                                      EPSquare = epSq
                                      FullMoveNumber = fmn
                                      CastlingRights = cr
                                      HalfMoveClock = hmc
                                      ToPlay = toPlay }

                                ValueSome { Board = board; State = state; Kings = kings }
                                
/// Convert board piece placement to FEN rank format (8..1 with digit compression).
let private placementToFen (board: Board) : string =
    let sb = Text.StringBuilder(90)

    for rank = 8 downto 1 do
        let mutable empties = 0

        for file = 1 to 8 do
            let p = board.[file - 1, rank - 1]
            if p = BoardHelpers.PieceCode.Empty then
                empties <- empties + 1
            else
                if empties > 0 then
                    sb.Append(empties) |> ignore
                    empties <- 0
                sb.Append(BoardHelpers.PieceCode.toFenChar p) |> ignore

        if empties > 0 then
            sb.Append(empties) |> ignore

        if rank > 1 then
            sb.Append('/') |> ignore

    sb.ToString()

/// Convert castling rights bitmask to FEN castling field.
let private castlingToFen (rights: byte) : string =
    let sb = Text.StringBuilder(4)
    if hasWK rights then sb.Append('K') |> ignore
    if hasWQ rights then sb.Append('Q') |> ignore
    if hasBK rights then sb.Append('k') |> ignore
    if hasBQ rights then sb.Append('q') |> ignore
    if sb.Length = 0 then "-" else sb.ToString()

/// Convert en-passant optional square to FEN field.
let private epToFen (ep: ValueOption<Coordinates>) : string =
    match ep with
    | ValueNone -> "-"
    | ValueSome c ->
        let fileChar = char (int 'a' + int c.File - 1)
        let rankChar = char (int '0' + int c.Rank)
        String.Concat(string fileChar, string rankChar)

/// Serialize a full Position to FEN.
let positionToFen (position: Position) : string =
    let placement = placementToFen position.Board
    let stm = if position.State.ToPlay = Color.White then "w" else "b"
    let castling = castlingToFen position.State.CastlingRights
    let ep = epToFen position.State.EPSquare
    let halfmove = string position.State.HalfMoveClock
    let fullmove = string position.State.FullMoveNumber
    String.Join(" ", [| placement; stm; castling; ep; halfmove; fullmove |])


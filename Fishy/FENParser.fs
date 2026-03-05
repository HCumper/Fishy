module Fen

open System
open Types
open BoardHelpers.CastlingRights
open BoardHelpers.Coordinates

let private splitFen (fen: string) =
    fen.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let private parseSideToMove (s: string) : ValueOption<Color> =
    match s with
    | "w" -> ValueSome Color.White
    | "b" -> ValueSome Color.Black
    | _   -> ValueNone

let private tryParseUInt16 (s: string) : ValueOption<uint16> =
    match UInt16.TryParse(s) with
    | true, v -> ValueSome v
    | _       -> ValueNone

let private tryParseByte (s: string) : ValueOption<byte> =
    match Byte.TryParse(s) with
    | true, v -> ValueSome v
    | _       -> ValueNone

/// EP field: "-" => ValueSome ValueNone; "e3" => ValueSome(ValueSome coords); invalid => ValueNone.
/// Coordinates are 0-based: a1=(0,0), h8=(7,7).
let private tryParseEPSquare (s: string) : ValueOption<ValueOption<Coordinates>> =
    if s = "-" then ValueSome ValueNone
    elif s.Length = 2 then
        let fileChar = s.[0]
        let rankChar = s.[1]

        let file =
            match fileChar with
            | c when c >= 'a' && c <= 'h' -> int c - int 'a'
            | c when c >= 'A' && c <= 'H' -> int c - int 'A'
            | _ -> -1

        let rank =
            match rankChar with
            | c when c >= '1' && c <= '8' -> int c - int '1'
            | _ -> -1

        match BoardHelpers.Coordinates.tryCreate file rank with
        | ValueSome c -> ValueSome (ValueSome c)
        | ValueNone -> ValueNone
    else
        ValueNone

let private clearBoard (b:Board) =
    Array.Fill(b, 0y)

let inline private idxFR (file:int) (rank:int) : int =
    (rank <<< 3) ||| file

/// Parse placement (ranks 8..1) into 0-based board with rank 7..0.
/// Returns king squares in 0-based Coordinates.
let private tryParsePlacement (placement: string) (board: Board) : ValueOption<KingSquares> =
    clearBoard board

    let mutable file = 0   // 0..8 while filling
    let mutable rank = 7   // 7..0

    let mutable wkOpt : ValueOption<Coordinates> = ValueNone
    let mutable bkOpt : ValueOption<Coordinates> = ValueNone
    let mutable ok = true

    let inline recordKing (p:sbyte) (f:int) (r:int) =
        if p = 6y then wkOpt <- ValueSome { File = byte f; Rank = byte r }
        elif p = -6y then bkOpt <- ValueSome { File = byte f; Rank = byte r }

    for ch in placement do
        if ok then
            match ch with
            | '/' ->
                if file <> 8 then ok <- false
                else
                    file <- 0
                    rank <- rank - 1
                    if rank < 0 then ok <- false

            | c when c >= '1' && c <= '8' ->
                let n = int c - int '0'
                if file + n > 8 then ok <- false
                else file <- file + n

            | _ ->
                match BoardHelpers.PieceCode.tryOfFenChar ch with
                | ValueNone -> ok <- false
                | ValueSome p ->
                    if file >= 8 || rank < 0 then ok <- false
                    else
                        board.[idxFR file rank] <- p
                        recordKing p file rank
                        file <- file + 1

    if ok && rank = 0 && file = 8 then
        match wkOpt, bkOpt with
        | ValueSome wk, ValueSome bk -> ValueSome { WhiteKingSq = wk; BlackKingSq = bk }
        | _ -> ValueNone
    else
        ValueNone

/// Parse a full FEN string into a Position. HashKey remains 0L; compute after load if needed.
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
                match BoardHelpers.PieceCode.tryOfFen castling with
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

let private placementToFen (board: Board) : string =
    let sb = Text.StringBuilder(90)

    for rank = 7 downto 0 do
        let mutable empties = 0

        for file = 0 to 7 do
            let p = board.[idxFR file rank]   // FIXED for 1D
            if p = BoardHelpers.PieceCode.Empty then
                empties <- empties + 1
            else
                if empties > 0 then
                    sb.Append(empties) |> ignore
                    empties <- 0
                sb.Append(BoardHelpers.PieceCode.toFenChar p) |> ignore

        if empties > 0 then
            sb.Append(empties) |> ignore

        if rank > 0 then
            sb.Append('/') |> ignore

    sb.ToString()

let private castlingToFen (rights: byte) : string =
    let sb = Text.StringBuilder(4)
    if hasWK rights then sb.Append('K') |> ignore
    if hasWQ rights then sb.Append('Q') |> ignore
    if hasBK rights then sb.Append('k') |> ignore
    if hasBQ rights then sb.Append('q') |> ignore
    if sb.Length = 0 then "-" else sb.ToString()

/// Coordinates are 0-based: (0,0) => "a1"
let private epToFen (ep: ValueOption<Coordinates>) : string =
    match ep with
    | ValueNone -> "-"
    | ValueSome c ->
        let fileChar = char (int 'a' + int c.File)
        let rankChar = char (int '1' + int c.Rank)
        String.Concat(string fileChar, string rankChar)

let positionToFen (position: Position) : string =
    let placement = placementToFen position.Board
    let stm = if position.State.ToPlay = Color.White then "w" else "b"
    let castling = castlingToFen position.State.CastlingRights
    let ep = epToFen position.State.EPSquare
    let halfmove = string position.State.HalfMoveClock
    let fullmove = string position.State.FullMoveNumber
    String.Join(" ", [| placement; stm; castling; ep; halfmove; fullmove |])
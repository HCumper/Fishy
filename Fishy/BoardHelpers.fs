namespace BoardHelpers

module PieceCode =

    open System
    
    // PieceCode: sbyte piece encoding and conversions.
    // 0y=empty, >0 white, <0 black; abs(code): 1 pawn,2 knight,3 bishop,4 rook,5 queen,6 king.
    // Provides predicates, Color/Kind conversions, and FEN char mapping.

    [<Literal>]
    let Empty  : sbyte = 0y
    [<Literal>]
    let Pawn   : sbyte = 1y
    [<Literal>]
    let Knight : sbyte = 2y
    [<Literal>]
    let Bishop : sbyte = 3y
    [<Literal>]
    let Rook   : sbyte = 4y
    [<Literal>]
    let Queen  : sbyte = 5y
    [<Literal>]
    let King   : sbyte = 6y

    type Color = Types.Color
    type PieceKind = Types.Piece

    [<Literal>]
    let private MaxKind : sbyte = 6y

    let inline isEmpty p = p = Empty
    let inline isWhite p = p > 0y
    let inline isBlack p = p < 0y

    /// Returns piece color, or ValueNone for empty.
    let inline colorOf (p:sbyte) =
        if p = Empty then ValueNone
        elif p > 0y then ValueSome Color.White
        else ValueSome Color.Black

    /// Absolute piece kind code (1..6), ignoring color.
    let inline absKind (p:sbyte) = if p >= 0y then p else -p

    /// True if piece matches given kind code (1..6).
    let inline isKind kind p = absKind p = kind

    let inline isPawn   p = isKind Pawn p
    let inline isKnight p = isKind Knight p
    let inline isBishop p = isKind Bishop p
    let inline isRook   p = isKind Rook p
    let inline isQueen  p = isKind Queen p
    let inline isKing   p = isKind King p

    /// Create signed piece code from color and kind (1..6).
    let inline make color kind =
        match color with
        | Color.White -> kind
        | _ -> -kind


    /// Create signed piece code from Color and PieceKind.
    let inline makeFromKind color kind =
        let k =
            match kind with
            | Types.Pawn   -> Pawn
            | Types.Knight -> Knight
            | Types.Bishop -> Bishop
            | Types.Rook   -> Rook
            | Types.Queen  -> Queen
            | Types.King   -> King
        make color k

    /// Convert piece code to PieceKind, or ValueNone if empty/invalid.
    let inline kindOf p =
        match absKind p with
        | 1y -> ValueSome Types.Pawn
        | 2y -> ValueSome Types.Knight
        | 3y -> ValueSome Types.Bishop
        | 4y -> ValueSome Types.Rook
        | 5y -> ValueSome Types.Queen
        | 6y -> ValueSome Types.King
        | _  -> ValueNone

    /// Material value for evaluation (pawn=100, etc.).
    let inline materialValue p =
        match absKind p with
        | 1y -> 100
        | 2y -> 320
        | 3y -> 330
        | 4y -> 500
        | 5y -> 900
        | _  -> 0

    /// True if both pieces are non-empty and same color.
    let inline sameColor a b =
        a <> Empty && b <> Empty && (isWhite a = isWhite b)

    /// True if both pieces are non-empty and opposite colors.
    let inline oppositeColor a b =
        a <> Empty && b <> Empty && (isWhite a <> isWhite b)

    /// Flip piece color (white↔black), empty unchanged.
    let inline flipColor p =
        if p = Empty then Empty else -p
        
    /// Convert FEN character to piece code. Returns ValueNone if invalid.
    let inline tryOfFenChar (c: char) : ValueOption<sbyte> =
        match c with
        | 'P' -> ValueSome (make Color.White Pawn)
        | 'N' -> ValueSome (make Color.White Knight)
        | 'B' -> ValueSome (make Color.White Bishop)
        | 'R' -> ValueSome (make Color.White Rook)
        | 'Q' -> ValueSome (make Color.White Queen)
        | 'K' -> ValueSome (make Color.White King)
        | 'p' -> ValueSome (make Color.Black Pawn)
        | 'n' -> ValueSome (make Color.Black Knight)
        | 'b' -> ValueSome (make Color.Black Bishop)
        | 'r' -> ValueSome (make Color.Black Rook)
        | 'q' -> ValueSome (make Color.Black Queen)
        | 'k' -> ValueSome (make Color.Black King)
        | _   -> ValueNone    
    
    /// Parse castling-rights FEN field into a bitmask (WK=1, WQ=2, BK=4, BQ=8).
    /// Accepts "-" for no rights; returns ValueNone for invalid input.
    let tryOfFen (s: string) : ValueOption<byte> =
        let WK : byte = 0b0001uy
        let WQ : byte = 0b0010uy
        let BK : byte = 0b0100uy
        let BQ : byte = 0b1000uy

        if String.IsNullOrWhiteSpace s then
            ValueNone
        elif s = "-" then
            ValueSome 0uy
        else
            let mutable rights = 0uy
            let mutable ok = true

            for ch in s do
                if not ok then () else
                match ch with
                | 'K' -> rights <- rights ||| WK
                | 'Q' -> rights <- rights ||| WQ
                | 'k' -> rights <- rights ||| BK
                | 'q' -> rights <- rights ||| BQ
                | _ -> ok <- false

            if ok then ValueSome rights else ValueNone
    
    /// Convert piece code to FEN character. '.' for empty, '?' for invalid.
    let inline toFenChar (p: sbyte) : char =
        match p with
        | 0y -> '.'
        | _ ->
            let white = isWhite p
            match absKind p with
            | 1y -> if white then 'P' else 'p'
            | 2y -> if white then 'N' else 'n'
            | 3y -> if white then 'B' else 'b'
            | 4y -> if white then 'R' else 'r'
            | 5y -> if white then 'Q' else 'q'
            | 6y -> if white then 'K' else 'k'
            | _  -> '?'
/////////////////////////////////////////////////////////////////////

// CastlingRights: byte bitmask.
// Bits: WK=1, WQ=2, BK=4, BQ=8. Includes FEN mapping and update helpers.
module CastlingRights =
    open Types
    [<Literal>]
    let WK : byte = 0b0001uy
    [<Literal>]
    let WQ : byte = 0b0010uy
    [<Literal>]
    let BK : byte = 0b0100uy
    [<Literal>]
    let BQ : byte = 0b1000uy

    let inline hasWK r = (r &&& WK) <> 0uy
    let inline hasWQ r = (r &&& WQ) <> 0uy
    let inline hasBK r = (r &&& BK) <> 0uy
    let inline hasBQ r = (r &&& BQ) <> 0uy

    let inline clearWK r = r &&& (~~~WK)
    let inline clearWQ r = r &&& (~~~WQ)
    let inline clearBK r = r &&& (~~~BK)
    let inline clearBQ r = r &&& (~~~BQ)

    let inline clearWhiteAll r = r |> clearWK |> clearWQ
    let inline clearBlackAll r = r |> clearBK |> clearBQ

    // 1-based coordinates (File/Rank = 1..8)
    let a1 = { File = 1uy; Rank = 1uy }
    let e1 = { File = 5uy; Rank = 1uy }
    let h1 = { File = 8uy; Rank = 1uy }
    let a8 = { File = 1uy; Rank = 8uy }
    let e8 = { File = 5uy; Rank = 8uy }
    let h8 = { File = 8uy; Rank = 8uy }

    let inline sameSq a b =
        a.File = b.File && a.Rank = b.Rank

    let inline private absSByte x = if x >= 0y then x else -x
    let inline private isRook p = absSByte p = 4y
    let inline private isKing p = absSByte p = 6y
    let inline private isWhitePiece p = p > 0y
    let inline private isBlackPiece p = p < 0y

    /// Updates castling rights after a move.
    /// 
    /// Rules enforced:
    /// - If a king moves from its original square (e1/e8), both castling rights
    ///   for that side are cleared.
    /// - If a rook moves from its original square (a1/h1/a8/h8),
    ///   the corresponding castling side is cleared.
    /// - If a rook is captured on its original square,
    ///   the corresponding castling right is cleared.
    ///
    /// Must be called during makeMove *before* the destination square
    /// is overwritten, so the capturedPiece reflects the pre-move board.
    let updateOnMove movingPiece fromSq toSq capturedPiece rights =
        let mutable r = rights

        if isKing movingPiece then
            if isWhitePiece movingPiece && sameSq fromSq e1 then
                r <- clearWhiteAll r
            elif isBlackPiece movingPiece && sameSq fromSq e8 then
                r <- clearBlackAll r

        if isRook movingPiece then
            if isWhitePiece movingPiece then
                if sameSq fromSq h1 then r <- clearWK r
                elif sameSq fromSq a1 then r <- clearWQ r
            elif isBlackPiece movingPiece then
                if sameSq fromSq h8 then r <- clearBK r
                elif sameSq fromSq a8 then r <- clearBQ r

        if capturedPiece <> 0y && isRook capturedPiece then
            if isWhitePiece capturedPiece then
                if sameSq toSq h1 then r <- clearWK r
                elif sameSq toSq a1 then r <- clearWQ r
            elif isBlackPiece capturedPiece then
                if sameSq toSq h8 then r <- clearBK r
                elif sameSq toSq a8 then r <- clearBQ r

        r    
    
////////////////////////////////////////////////////////////////
    
// Coordinates: 1-based (File/Rank = 1..8). Provides validated and unchecked creation.
module Coordinates =
    open Types

    /// True if coordinate is within 1..8.
    let inline isValid (c: Coordinates) =
        int c.File >= MinFileRank && int c.File <= MaxFileRank &&
        int c.Rank >= MinFileRank && int c.Rank <= MaxFileRank

    let inline isValidFileRank (file : int) (rank : int) =
        file >= MinFileRank && file <= MaxFileRank &&
        rank >= MinFileRank && rank <= MaxFileRank

    /// Try create from int file/rank (1..8).
    let inline tryCreate (file:int) (rank:int) : Coordinates voption =
        if file >= MinFileRank && file <= MaxFileRank &&
           rank >= MinFileRank && rank <= MaxFileRank then
            ValueSome { File = byte file; Rank = byte rank }
        else
            ValueNone

    /// Construct from two bytes (no validation).
    let inline createBytes (file:byte) (rank:byte) : Coordinates =
        { File = file; Rank = rank }

    /// Construct from two ints (no validation).
    let inline createInts (file:int) (rank:int) : Coordinates =
        { File = byte file; Rank = byte rank }

    /// Unsafe constructor when caller guarantees validity (hot path).
    let inline createUnchecked (file:byte) (rank:byte) : Coordinates =
        { File = file; Rank = rank }
        
//////////////////////////////////////////////////////////////////        
    // Board: sbyte[,] storage using 0-based array internally.
    // Coordinates are 1-based; conversion subtracts 1 for indexing.
    
module Board =
    open Types

    let inline isOnBoard file rank =
        file >= MinFileRank && file <= MaxFileRank &&
        rank >= MinFileRank && rank <= MaxFileRank

    let inline get (board: Board) file rank =
        board.[file - 1, rank - 1]

    let inline set (board: Board) file rank piece =
        board.[file - 1, rank - 1] <- piece

    let inline getC (board: Board) (c: Coordinates) : sbyte =
        board.[int c.File - 1, int c.Rank - 1]

    let inline setC (board: Board) (c: Coordinates) (piece: sbyte) : unit =
        board.[int c.File - 1, int c.Rank - 1] <- piece

    let inline tryGet (board: Board) (file: int) (rank: int) : sbyte voption =
        if isOnBoard file rank then
            ValueSome board[file - 1, rank - 1]
        else
            ValueNone
            
module Attacks =
    open Types
    open Board
    open PieceCode
    
    let inline private otherColor (c: Color) =
        match c with
        | Color.White -> Color.Black
        | _ -> Color.White

    let inline private onBoard (file:int) (rank:int) =
        file >= MinFileRank && file <= MaxFileRank &&
        rank >= MinFileRank && rank <= MaxFileRank

    let inline private tryCoord (file:int) (rank:int) : Coordinates voption =
        if onBoard file rank then
            ValueSome { File = byte file; Rank = byte rank }
        else
            ValueNone

    let inline private isAttacker (attacker: Color) (p: sbyte) =
        p <> Empty &&
        ((attacker = Color.White && isWhite p) ||
         (attacker = Color.Black && isBlack p))

    let private knightOffsets =
        [|
            ( 1,  2); ( 2,  1); (-1,  2); (-2,  1)
            ( 1, -2); ( 2, -1); (-1, -2); (-2, -1)
        |]

    let private kingOffsets =
        [|
            ( 1,  0); ( 1,  1); ( 0,  1); (-1,  1)
            (-1,  0); (-1, -1); ( 0, -1); ( 1, -1)
        |]

    let private bishopDirs = [| ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
    let private rookDirs   = [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1) |]

    /// True if `sq` is attacked by any piece of `attacker`.
    /// Uses 1-based Coordinates and Board.getC.
    let isSquareAttacked (pos: Position) (sq: Coordinates) (attacker: Color) : bool =
        let board = pos.Board
        let f = int sq.File
        let r = int sq.Rank

        // --- pawn attacks (check from target square backwards to pawn sources) ---
        let pawnSourceRank = if attacker = Color.White then r - 1 else r + 1
        match tryCoord (f - 1) pawnSourceRank with
        | ValueSome c ->
            let p = getC board c
            if isAttacker attacker p && absKind p = Pawn then true else
            match tryCoord (f + 1) pawnSourceRank with
            | ValueSome c2 ->
                let p2 = getC board c2
                isAttacker attacker p2 && absKind p2 = Pawn
            | ValueNone -> false
        | ValueNone ->
            match tryCoord (f + 1) pawnSourceRank with
            | ValueSome c2 ->
                let p2 = getC board c2
                isAttacker attacker p2 && absKind p2 = Pawn
            | ValueNone -> false
        |> fun pawnHit ->
            if pawnHit then true else

            // --- knight attacks ---
            let mutable hit = false
            for (df, dr) in knightOffsets do
                if not hit then
                    match tryCoord (f + df) (r + dr) with
                    | ValueSome c ->
                        let p = getC board c
                        if isAttacker attacker p && absKind p = Knight then hit <- true
                    | ValueNone -> ()
            if hit then true else

            // --- king adjacency ---
            for (df, dr) in kingOffsets do
                if not hit then
                    match tryCoord (f + df) (r + dr) with
                    | ValueSome c ->
                        let p = getC board c
                        if isAttacker attacker p && absKind p = King then hit <- true
                    | ValueNone -> ()
            if hit then true else

            // --- sliding rays ---
            let inline scanRay (df:int) (dr:int) (matches: sbyte -> bool) =
                let mutable nf = f + df
                let mutable nr = r + dr
                let mutable blocked = false
                while not hit && not blocked && onBoard nf nr do
                    let p = getC board { File = byte nf; Rank = byte nr }
                    if p = Empty then
                        nf <- nf + df
                        nr <- nr + dr
                    else
                        if matches p then hit <- true
                        blocked <- true

            // bishops/queens (diagonals)
            for (df, dr) in bishopDirs do
                if not hit then
                    scanRay df dr (fun p ->
                        isAttacker attacker p && (absKind p = Bishop || absKind p = Queen))

            // rooks/queens (orthogonals)
            for (df, dr) in rookDirs do
                if not hit then
                    scanRay df dr (fun p ->
                        isAttacker attacker p && (absKind p = Rook || absKind p = Queen))

            hit

    /// True if `side`'s king is in check.
    let inCheck (pos: Position) (side: Color) : bool =
        let kingSq =
            if side = Color.White then pos.Kings.WhiteKingSq
            else pos.Kings.BlackKingSq
        isSquareAttacked pos kingSq (otherColor side)            
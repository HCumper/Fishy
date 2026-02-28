module GenerateMoves

open Types
open BoardHelpers
open PieceCode
open Board
open MakeMove
open Attacks

let inline private otherColor (c: Color) =
    match c with
    | Color.White -> Color.Black
    | _ -> Color.White

let inline private getAt (board: Board) (file:int) (rank:int) : sbyte =
    // 1-based -> 0-based internal array
    board.[file - 1, rank - 1]

let inline private isAttackerColor (attacker: Color) (p: sbyte) =
    p <> Empty &&
    ((attacker = Color.White && isWhite p) ||
     (attacker = Color.Black && isBlack p))

let inline private isAttackerPawn (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Pawn

let inline private isAttackerKnight (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Knight

let inline private isAttackerBishop (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Bishop

let inline private isAttackerRook (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Rook

let inline private isAttackerQueen (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = Queen

let inline private isAttackerKing (attacker: Color) (p: sbyte) =
    isAttackerColor attacker p && absKind p = King

let inline private tryAddLegalMove
    (pos: Position)
    (side: Color)
    (inCheck: Position -> Color -> bool)
    (moves: ResizeArray<Move>)
    (mv: Move) : unit =

    let mutable p = pos
    let undo = makeMove &p mv
    if not (inCheck p side) then
        moves.Add mv
    unmakeMove &p mv undo
    
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

let generateLegalKnightMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq

    let side = pos.State.ToPlay
    // Must be a knight of side to move
    if not (isKnight piece) then
        []
    else
        let moves = ResizeArray<Move>(8)

        for (df, dr) in knightOffsets do
            let newFile = int fromSq.File + df
            let newRank = int fromSq.Rank + dr

            if Coordinates.isValid {File = byte newFile; Rank = byte newRank} then

                let toSq =
                    { File = byte newFile
                      Rank = byte newRank }

                let target = getC board toSq

                // empty square OR opponent piece
                if target = Empty || oppositeColor piece target then

                    let move =
                        { From = fromSq
                          To = toSq
                          Piece = piece
                          PromoteTo = 0y }

                    tryAddLegalMove pos side inCheck moves move
        List.ofSeq moves

// Generic helper for bishops/rooks/queens (sliding pieces)
let private generateLegalSlidingMoves
    (pos: Position)
    (fromSq: Coordinates)
    (dirs: (int * int)[])
    (pieceOk: sbyte -> bool)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq
    let side = pos.State.ToPlay

    // Must be correct piece type AND belong to side to move
    if not (pieceOk piece) then
        []
    else
        // Upper bound: queen from center can have up to 27 pseudo-legal moves.
        let moves = ResizeArray<Move>(32)

        for (df, dr) in dirs do
            let mutable newFile = int fromSq.File + df
            let mutable newRank = int fromSq.Rank + dr
            let mutable blocked = false

            while not blocked && Coordinates.isValidFileRank newFile newRank do

                let toSq = { File = byte newFile; Rank = byte newRank }
                let target = getC board toSq

                // empty OR capture opponent
                if target = Empty || oppositeColor piece target then
                    let move =
                        { From = fromSq
                          To = toSq
                          Piece = piece
                          PromoteTo = 0y }

                    tryAddLegalMove pos side inCheck moves move

                // stop ray on any occupied square (capture or blocked by friendly)
                if target <> Empty then
                    blocked <- true
                else
                    newFile <- newFile + df
                    newRank <- newRank + dr

        List.ofSeq moves


let generateLegalBishopMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let bishopDirs = [| ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
    generateLegalSlidingMoves pos fromSq bishopDirs isBishop inCheck


let generateLegalRookMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let rookDirs = [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1) |]
    generateLegalSlidingMoves pos fromSq rookDirs isRook inCheck


let generateLegalQueenMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let queenDirs =
        [| ( 1,  0); (-1,  0); ( 0,  1); ( 0, -1)
           ( 1,  1); ( 1, -1); (-1,  1); (-1, -1) |]
    generateLegalSlidingMoves pos fromSq queenDirs isQueen inCheck
    
let generateLegalPawnMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq
    let side  = pos.State.ToPlay

    // Must be a pawn of side to move
    if not (isPawn piece) then
        []
    else
        let moves = ResizeArray<Move>(12)

        let forward = if side = Color.White then 1 else -1
        let startRank = if side = Color.White then 2 else 7
        let promoRank = if side = Color.White then 8 else 1

        let file0 = int fromSq.File
        let rank0 = int fromSq.Rank

        let promoKinds = [| Queen; Rook; Bishop; Knight |]  // kind codes (5,4,3,2)

        let inline addLegalMove (mv: Move) =
            let mutable p = pos
            let undo = makeMove &p mv
            if not (inCheck p side) then moves.Add mv
            unmakeMove &p mv undo

        // ---------- Forward moves ----------
        let oneRank = rank0 + forward
        if Coordinates.isValidFileRank file0 oneRank then
            let oneSq = { File = byte file0; Rank = byte oneRank }
            if getC board oneSq = Empty then
                if oneRank = promoRank then
                    // promotion (quiet)
                    for k in promoKinds do
                        let promoCode = make side k
                        addLegalMove
                            { From = fromSq
                              To = oneSq
                              Piece = piece
                              PromoteTo = promoCode }
                else
                    // normal quiet pawn push
                    addLegalMove
                        { From = fromSq
                          To = oneSq
                          Piece = piece
                          PromoteTo = 0y }

                    // double push from start rank if both squares empty
                    if rank0 = startRank then
                        let twoRank = rank0 + 2 * forward
                        if Coordinates.isValidFileRank file0 twoRank then
                            let twoSq = { File = byte file0; Rank = byte twoRank }
                            if getC board twoSq = Empty then
                                addLegalMove
                                    { From = fromSq
                                      To = twoSq
                                      Piece = piece
                                      PromoteTo = 0y }

        // ---------- Captures (including promotion captures and en passant) ----------
        let targetRank = rank0 + forward
        if Coordinates.isValidFileRank file0 targetRank then
            for df in [| -1; 1 |] do
                let tf = file0 + df
                if Coordinates.isValidFileRank tf targetRank then
                    let toSq = { File = byte tf; Rank = byte targetRank }
                    let target = getC board toSq

                    let isEP =
                        target = Empty &&
                        match pos.State.EPSquare with
                        | ValueSome ep -> ep.File = toSq.File && ep.Rank = toSq.Rank
                        | ValueNone -> false

                    if (target <> Empty && oppositeColor piece target) || isEP then
                        if targetRank = promoRank && not isEP then
                            // promotion capture (EP can never be a promotion)
                            for k in promoKinds do
                                let promoCode = make side k
                                addLegalMove
                                    { From = fromSq
                                      To = toSq
                                      Piece = piece
                                      PromoteTo = promoCode }
                        else
                            // normal capture or en passant capture
                            addLegalMove
                                { From = fromSq
                                  To = toSq
                                  Piece = piece
                                  PromoteTo = 0y }

        List.ofSeq moves
        
let inline private kingAdjacent (a: Coordinates) (b: Coordinates) : bool =
      let df = abs (int a.File - int b.File)
      let dr = abs (int a.Rank - int b.Rank)
      df <= 1 && dr <= 1
      
let generateLegalKingMoves
    (pos: Position)
    (fromSq: Coordinates)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let piece = getC board fromSq
    let side  = pos.State.ToPlay

    if not (isKing piece) then
        []
    else
        let moves = ResizeArray<Move>(8)

        let oppKingSq =
            if side = Color.White then pos.Kings.BlackKingSq
            else pos.Kings.WhiteKingSq

        let inline tryAdd (move: Move) =
            let mutable p = pos
            let undo = makeMove &p move
            if not (inCheck p side) then
                moves.Add move
            unmakeMove &p move undo

        let f0 = int fromSq.File
        let r0 = int fromSq.Rank

        // ---------------------------
        // Normal king moves
        // ---------------------------
        for df in -1 .. 1 do
            for dr in -1 .. 1 do
                if not (df = 0 && dr = 0) then
                    let nf = f0 + df
                    let nr = r0 + dr

                    if Coordinates.isValidFileRank nf nr then
                        let toSq = { File = byte nf; Rank = byte nr }

                        // NEW: prohibit king adjacency
                        if not (kingAdjacent toSq oppKingSq) then
                            let target = getC board toSq
                            if target = Empty || oppositeColor piece target then
                                let move =
                                    { From = fromSq
                                      To = toSq
                                      Piece = piece
                                      PromoteTo = 0y }
                                tryAdd move

        // ---------------------------
        // Castling
        // ---------------------------
        if not (inCheck pos side) then
            let rights = pos.State.CastlingRights
            let rankB  = byte (if side = Color.White then 1 else 8)

            // Kingside: e -> g, passes through f
            let canCastleK =
                if side = Color.White then CastlingRights.hasWK rights
                else CastlingRights.hasBK rights

            if canCastleK then
                let fSq = { File = 6uy; Rank = rankB }
                let gSq = { File = 7uy; Rank = rankB }
                let hSq = { File = 8uy; Rank = rankB } 
                
                let rookPresent = 
                    let rook = getC board hSq
                    (side = Color.White && rook = Rook) ||
                    (side = Color.Black && rook = -Rook)
                    
                if rookPresent && getC board fSq = Empty && getC board gSq = Empty then
                    // NEW: prohibit adjacency on transit/destination squares
                    if not (kingAdjacent fSq oppKingSq) &&
                       not (kingAdjacent gSq oppKingSq) then

                        if not (isSquareAttacked pos fSq (otherColor side)) &&
                           not (isSquareAttacked pos gSq (otherColor side)) then

                            let move =
                                { From = fromSq
                                  To = gSq
                                  Piece = piece
                                  PromoteTo = 0y }
                            tryAdd move

            // Queenside: e -> c, passes through d, destination c (b must be empty too)
            let canCastleQ =
                if side = Color.White then CastlingRights.hasWQ rights
                else CastlingRights.hasBQ rights

            if canCastleQ then
                let bSq = { File = 2uy; Rank = rankB }
                let dSq = { File = 4uy; Rank = rankB }
                let cSq = { File = 3uy; Rank = rankB }
                let aSq = { File = 1uy; Rank = rankB } 
                
                let rookPresent = 
                    let rook = getC board aSq
                    (side = Color.White && rook = Rook) ||
                    (side = Color.Black && rook = -Rook)
                    
                if rookPresent && getC board bSq = Empty && getC board dSq = Empty && getC board cSq = Empty then
                    // NEW: prohibit adjacency on transit/destination squares
                    if not (kingAdjacent dSq oppKingSq) &&
                       not (kingAdjacent cSq oppKingSq) then

                        if not (isSquareAttacked pos dSq (otherColor side)) &&
                           not (isSquareAttacked pos cSq (otherColor side)) then

                            let move =
                                { From = fromSq
                                  To = cSq
                                  Piece = piece
                                  PromoteTo = 0y }
                            tryAdd move

        List.ofSeq moves
        
let inline private isCaptureMove (pos: Position) (mv: Move) : bool =
    let target = getC pos.Board mv.To

    // Normal capture
    if target <> Empty then true
    else
        // En passant capture
        absKind mv.Piece = Pawn
        && int mv.From.File <> int mv.To.File
        && match pos.State.EPSquare with
           | ValueSome ep -> ep.File = mv.To.File && ep.Rank = mv.To.Rank
           | ValueNone -> false
           
/// Generate all legal moves for the side to move, using the piece-type generators.
let generateAllLegalMoves
    (pos: Position)
    (inCheck: Position -> Color -> bool)
    : Move list =

    let board = pos.Board
    let side  = pos.State.ToPlay

    let moves = ResizeArray<Move>(64)

    // iterate every square on the board (1..8 coords)
    for rank = MinFileRank to MaxFileRank do
        for file = MinFileRank to MaxFileRank do
            let sq = { File = byte file; Rank = byte rank }
            let p = Board.getC board sq

            // skip empty and opponent pieces
            if p <> PieceCode.Empty then
                let mine =
                    (side = Color.White && PieceCode.isWhite p) ||
                    (side = Color.Black && PieceCode.isBlack p)

                if mine then
                    // Dispatch by piece kind
                    match PieceCode.absKind p with
                    | PieceCode.Pawn ->
                        generateLegalPawnMoves pos sq inCheck |> List.iter moves.Add
                    | PieceCode.Knight ->
                        generateLegalKnightMoves pos sq inCheck |> List.iter moves.Add
                    | PieceCode.Bishop ->
                        generateLegalBishopMoves pos sq inCheck |> List.iter moves.Add
                    | PieceCode.Rook ->
                        generateLegalRookMoves pos sq inCheck |> List.iter moves.Add
                    | PieceCode.Queen ->
                        generateLegalQueenMoves pos sq inCheck |> List.iter moves.Add
                    | PieceCode.King ->
                        generateLegalKingMoves pos sq inCheck |> List.iter moves.Add
                    | _ ->
                        () // defensive; should not occur

    List.ofSeq moves
    
let generateAllLegalCaptures
    (pos: Position)
    (inCheck: Position -> Color -> bool)
    : Move list =

    generateAllLegalMoves pos inCheck
    |> List.filter (isCaptureMove pos)
    
// For debugging only
// Simplistic brute force search for all possible positions.
let rec perft (pos: Position) (depth: int) : uint64 =
    if depth = 0 then 1UL
    else
        let moves = generateAllLegalMoves pos inCheck
        moves 
        |> List.sumBy (fun mv ->
            let mutable p = pos
            let undo = makeMove &p mv
            let nodes = perft p (depth - 1)
            unmakeMove &p mv undo
            nodes)
module GenerateMoves
open Chess
open Types

let private convertToMove fromFile fromRank toFile toRank promoteTo capturedPiece : Move =
    {
        fromFile = fromFile
        fromRank = fromRank
        toFile = toFile
        toRank = toRank
        promoteTo = promoteTo
        capturedPiece = capturedPiece
    }

let toPieceOption file rank =
    if board[file, rank] = None then
        None
    else
        Some (fst board[file, rank].Value)

let private isSquareOnBoard (file, rank) = file >= 1 && file <= 8 && rank >= 1 && rank <= 8

let private isSquareEmptyOrOpponentPiece (board: Board) movingColor coordinates =
    match board[fst coordinates, snd coordinates] with
    | None -> true
    | Some (_, color) -> color <> movingColor

let private isSquareOpponentPiece movingColor coordinates =
    match board[fst coordinates, snd coordinates] with
    | None -> false
    | Some (_, color) -> color <> movingColor

let private isSquareEmpty (board: Board) (file, rank) : bool = board[file, rank] = None

// can move into check
let generateKingMoves board (gameState: OtherState) (file, rank) =
    let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
    let mutable (kingMoves: Move list) = []

    // ordinary king moves
    let isOppositionAllows (deltaFile, deltaRank) : bool =
        if isSquareEmptyOrOpponentPiece board gameState.ToPlay (deltaFile, deltaRank) then
            let availableDirections = List.filter (fun x -> isSquareOnBoard ((fst x) + deltaFile, (snd x) + deltaRank)) directions
            not (List.exists (fun x -> board[(fst x) + deltaFile, (snd x) + deltaRank] = Some (King, Black)) availableDirections)
        else
            false

    kingMoves <- directions
        |> List.map (fun (x,y) -> (x + file, y + rank))
        |> List.filter isSquareOnBoard
        |> List.filter (isSquareEmptyOrOpponentPiece board gameState.ToPlay)
        |> List.filter isOppositionAllows
        |> List.map ( fun x -> convertToMove file rank (fst x) (snd x) None (toPieceOption (fst x) (snd x)))

    // castling
    if gameState.ToPlay = White then
        if not gameState.WhiteKingMoved then
            if not gameState.WhiteKRMoved then
                if board[6, 1] = None && board[7, 1] = None then
                    kingMoves <- convertToMove 5 1 7 1 None None:: kingMoves

            if not gameState.WhiteQRMoved then
                if board[4, 1] = None && board[3, 1] = None && board[2, 1] = None then
                    kingMoves <- convertToMove 5 1 3 1 None None:: kingMoves
    else
        if not gameState.BlackKingMoved then
            if not gameState.BlackKRMoved then
                if board[6, 1] = None && board[7, 1] = None then
                    kingMoves <- convertToMove 5 1 7 1 None None :: kingMoves

            if not gameState.BlackQRMoved then
                if board[4, 1] = None && board[3, 1] = None && board[2, 1] = None then
                    kingMoves <- convertToMove 5 1 3 1 None None:: kingMoves

    kingMoves

let generatePawnMoves board (gameState: OtherState) (file, rank) =
    let direction = if gameState.ToPlay = Black then -1 else 1

    let singleAdvanceMoves =
        if isSquareOnBoard (file, rank + direction) && isSquareEmpty board (file, rank + direction) then
            if rank + direction = 1 || rank + direction = 8 then
                [convertToMove file rank file (rank + direction) (Some Queen);
                 convertToMove file rank file (rank + direction) (Some Rook);
                 convertToMove file rank file (rank + direction) (Some Bishop);
                 convertToMove file rank file (rank + direction) (Some Knight)]
            else
                [convertToMove file rank file (rank + direction) None]
        else
            []

    let ordinaryCaptureMoves =
        let leftCapture =
            if isSquareOnBoard (file - 1, rank + direction) && isSquareOpponentPiece board gameState.ToPlay (file - 1, rank + direction) then
                [convertToMove file rank (file - 1) (rank + direction) None]
            else
                []
        let rightCapture =
            if isSquareOnBoard (file + 1, rank + direction) && isSquareOpponentPiece board gameState.ToPlay (file + 1, rank + direction) then
                [convertToMove file rank (file + 1) (rank + direction) None]
            else
                []
        leftCapture @ rightCapture

    let epCaptureMoves =
        let epLeftCapture =
            if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file - 1, rank) then
                [convertToMove file rank (file - 1) (rank + direction) None]
            else
                []
        let epRightCapture =
            if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file + 1, rank) then
                [convertToMove file rank (file + 1) (rank + direction) None]
            else
                []
        epLeftCapture @ epRightCapture

    let doubleAdvanceMove =
        if isSquareOnBoard (file, rank + direction) && isSquareEmpty board (file, rank + direction) &&
           isSquareOnBoard (file, rank + direction * 2) && isSquareEmpty board (file, rank + direction * 2) then
            [convertToMove file rank file (rank + direction * 2) None]
        else
            []

    singleAdvanceMoves @ ordinaryCaptureMoves @ epCaptureMoves @ doubleAdvanceMove

let generateKnightMoves board gameState (file, rank) : Move list =
    [ (file + 2, rank + 1)
      (file + 2, rank - 1)
      (file - 2, rank + 1)
      (file - 2, rank - 1)
      (file + 1, rank + 2)
      (file + 1, rank - 2)
      (file - 1, rank + 2)
      (file - 1, rank - 2) ]
    |> List.filter isSquareOnBoard
    |> List.filter (isSquareEmptyOrOpponentPiece board gameState.ToPlay)
    |> List.map (fun (x, y) -> convertToMove file rank x y None)

let generateVectorMoves board gameState (file, rank) directions =

    let generateMoves deltaFile deltaRank =
        let rec loop newFile newRank (moves: Move list) =
            let nFile = newFile + deltaFile
            let nRank = newRank + deltaRank

            if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece board gameState.ToPlay (nFile, nRank) then
                let updatedMoves = (convertToMove file rank nFile nRank None) :: moves
                if board[nFile, nRank] = None then
                    loop nFile nRank updatedMoves
                else
                    updatedMoves
            else
                moves

        loop file rank []

    List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions

// Create list of all available moves on the board
let generateMoves (board: Board) gameState : Move list =
    let mutable availableMoves = []

    for rank = 1 to 8 do
        for file = 1 to 8 do
            match board[file, rank] with
            | Some (piece, color) when color = gameState.ToPlay ->
                let pieceMoves =
                    match piece with
                    | Knight -> generateKnightMoves board gameState (file, rank)
                    | Bishop -> generateVectorMoves board gameState (file, rank) [(1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | Rook -> generateVectorMoves board gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1)]
                    | Queen -> generateVectorMoves board gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | Pawn -> generatePawnMoves board gameState (file, rank)
                    | King -> generateKingMoves board gameState (file, rank)

                availableMoves <- pieceMoves @ availableMoves
            | _ -> ()

    availableMoves

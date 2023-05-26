module GenerateMoves

open Fishy
open Types
open System

// Create list of all available moves on the supplied board
let generateMoves (board: Board) gameState : Move list =

    let isSquareOnBoard (file, rank) = board[file, rank] <> offBoard

    let isSquareOpponentPiece (movingPiece: sbyte) coordinates =
        board[fst coordinates, snd coordinates] * movingPiece < 0y

    let isSquareEmptyOrOpponentPiece (movingPiece: sbyte) coordinates =
        board[fst coordinates, snd coordinates] * movingPiece <= 0y

    let convertToMove fromFile fromRank toFile toRank promoteTo capturedPiece : Move =
        {
            fromFile = fromFile
            fromRank = fromRank
            toFile = toFile
            toRank = toRank
            promoteTo = promoteTo
            capturedPiece = capturedPiece
        }

        // can move into check
    let generateKingMoves (gameState: OtherState) (file, rank) =
        let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
        let mutable (kingMoves: Move list) = []

        // ordinary king moves
        let isOppositionAllows (deltaFile, deltaRank) : bool =
            if isSquareEmptyOrOpponentPiece gameState.ToPlay (deltaFile, deltaRank) then
                let availableDirections = List.filter (fun x -> board[(fst x) + deltaFile, (snd x) + deltaRank] <> offBoard) directions
                List.exists (fun x -> abs board[(fst x) + deltaFile, (snd x) + deltaRank] = WhiteKing) availableDirections
            else
                false

        kingMoves <- directions
            |> List.map (fun (x,y) -> (x + file, y + rank))
            |> List.filter isSquareOnBoard
            |> List.filter (isSquareEmptyOrOpponentPiece gameState.ToPlay)
            |> List.filter isOppositionAllows
            |> List.map ( fun x -> convertToMove file rank (fst x) (snd x) Empty board[(fst x), (snd x)])

        // castling
        if gameState.ToPlay = White then
            if not gameState.WhiteKingMoved then
                if not gameState.WhiteKRMoved then
                    if board[6, 1] = Empty && board[7, 1] = Empty then
                        kingMoves <- convertToMove 5 1 7 1 Empty Empty :: kingMoves

                if not gameState.WhiteQRMoved then
                    if board[4, 1] = Empty && board[3, 1] = Empty && board[2, 1] = Empty then
                        kingMoves <- convertToMove 5 1 3 1 Empty Empty:: kingMoves
        else
            if not gameState.BlackKingMoved then
                if not gameState.BlackKRMoved then
                    if board[6, 1] = Empty && board[7, 1] = Empty then
                        kingMoves <- convertToMove 5 1 7 1 Empty Empty :: kingMoves

                if not gameState.BlackQRMoved then
                    if board[4, 1] = Empty && board[3, 1] = Empty && board[2, 1] = Empty then
                        kingMoves <- convertToMove 5 1 3 1 Empty Empty:: kingMoves

        kingMoves

    let generatePawnMoves (gameState: OtherState) (file, rank) =
        let direction = if gameState.ToPlay = Black then -1 else 1

        let singleAdvanceMoves =
            if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = Empty then
                if rank + direction = 1 || rank + direction = 8 then
                    [convertToMove file rank file (rank + direction) (WhiteQueen * (sbyte direction)) Empty;
                     convertToMove file rank file (rank + direction) (WhiteRook * (sbyte direction)) Empty;
                     convertToMove file rank file (rank + direction) (WhiteBishop * (sbyte direction)) Empty;
                     convertToMove file rank file (rank + direction) (WhiteKnight * (sbyte direction)) Empty]
                else
                    [convertToMove file rank file (rank + direction) Empty Empty]
            else
                []

        let ordinaryCaptureMoves =
            let leftCapture =
                if isSquareOnBoard (file - 1, rank + direction) && isSquareOpponentPiece board[file, rank] (file - 1, rank + direction) then
                    if rank + direction = 1 || rank + direction = 8 then
                        [convertToMove file rank (file-1) (rank + direction) (WhiteQueen * (sbyte direction)) Empty;
                         convertToMove file rank (file-1) (rank + direction) (WhiteRook * (sbyte direction)) Empty;
                         convertToMove file rank (file-1) (rank + direction) (WhiteBishop * (sbyte direction)) Empty;
                         convertToMove file rank (file-1) (rank + direction) (WhiteKnight * (sbyte direction)) Empty]
                    else
                        [convertToMove file rank (file - 1) (rank + direction) Empty Empty]
                else
                    []
            let rightCapture =
                if isSquareOnBoard (file + 1, rank + direction) && isSquareOpponentPiece board[file, rank] (file + 1, rank + direction) then
                    if rank + direction = 1 || rank + direction = 8 then
                        [convertToMove file rank (file+1) (rank + direction) (WhiteQueen * (sbyte direction)) Empty;
                         convertToMove file rank (file+1) (rank + direction) (WhiteRook * (sbyte direction)) Empty;
                         convertToMove file rank (file+1) (rank + direction) (WhiteBishop * (sbyte direction)) Empty;
                         convertToMove file rank (file+1) (rank + direction) (WhiteKnight * (sbyte direction)) Empty]
                    else
                        [convertToMove file rank (file + 1) (rank + direction) Empty Empty]
                else
                    []
            leftCapture @ rightCapture

        let epCaptureMoves =
            let epLeftCapture =
                if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file - 1, rank) then
                    [convertToMove file rank (file - 1) (rank + direction) Empty Empty]
                else
                    []
            let epRightCapture =
                if isSquareOnBoard (file, rank + direction) && gameState.EPSquare = Some (file + 1, rank) then
                    [convertToMove file rank (file + 1) (rank + direction) Empty Empty]
                else
                    []
            epLeftCapture @ epRightCapture

        let doubleAdvanceMove =
            if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = 0y &&
               isSquareOnBoard (file, rank + direction * 2) && board[file, rank + direction * 2] = 0y && (rank = 2 || rank = 7) then
                [convertToMove file rank file (rank + direction * 2) Empty Empty]
            else
                []

        singleAdvanceMoves @ ordinaryCaptureMoves @ epCaptureMoves @ doubleAdvanceMove

    let generateKnightMoves gameState (file, rank) : Move list =
        [ (file + 2, rank + 1)
          (file + 2, rank - 1)
          (file - 2, rank + 1)
          (file - 2, rank - 1)
          (file + 1, rank + 2)
          (file + 1, rank - 2)
          (file - 1, rank + 2)
          (file - 1, rank - 2) ]
        |> List.filter isSquareOnBoard
        |> List.filter (isSquareEmptyOrOpponentPiece gameState.ToPlay)
        |> List.map (fun (x, y) -> convertToMove file rank x y Empty board[x, y])

    let generateVectorMoves gameState (file, rank) directions =

        let generateMoves deltaFile deltaRank =
            let rec loop newFile newRank (moves: Move list) =
                let nFile = newFile + deltaFile
                let nRank = newRank + deltaRank

                if isSquareOnBoard (nFile, nRank) && isSquareEmptyOrOpponentPiece gameState.ToPlay (nFile, nRank) then
                    let updatedMoves = (convertToMove file rank nFile nRank Empty board[nFile, nRank]) :: moves
                    if board[nFile, nRank] = Empty then
                        loop nFile nRank updatedMoves
                    else
                        updatedMoves
                else
                    moves

            loop file rank []

        List.collect (fun (deltaFile, deltaRank) -> generateMoves deltaFile deltaRank) directions


    // generate moves body
    let mutable availableMoves = []
    for rank = 1 to 8 do
        for file = 1 to 8 do
            match board[file, rank] with
            | x when x * gameState.ToPlay > 0y ->
                let pieceMoves =
                    match abs x with
                    | WhiteKnight -> generateKnightMoves gameState (file, rank)
                    | WhiteBishop -> generateVectorMoves gameState (file, rank) [(1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | WhiteRook -> generateVectorMoves gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1)]
                    | WhiteQueen -> generateVectorMoves gameState (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | WhitePawn -> generatePawnMoves gameState (file, rank)
                    | WhiteKing -> generateKingMoves gameState (file, rank)
                    | _ -> []

                availableMoves <- pieceMoves @ availableMoves
            | _ -> ()

    availableMoves

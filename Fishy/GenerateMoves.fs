module GenerateMoves

open Fishy
open Types

// Create list of all available moves on the supplied board
let generateMoves (board: Board) gameState : Move list =

    let isSquareOnBoard (file, rank) = file >= 1 && file <= 8 && rank >= 1 && rank <= 8

    let isSquareOpponentPiece (movingPiece: sbyte) (file, rank) = board[file, rank] * movingPiece < 0y
    let isSquareEmptyOrOpponentPiece (movingPiece: sbyte) (file, rank) = board[file, rank] * movingPiece <= 0y && file >= 1 && file <= 8 && rank >= 1 && rank <= 8
    
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
    let generateKingMoves (file, rank) =
        let directions = [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
        let mutable kingMoves = []

        // ordinary king moves
        let isOppositionAllows (deltaFile, deltaRank) =
            if isSquareEmptyOrOpponentPiece gameState.ToPlay (deltaFile, deltaRank) then
                let availableDirections = List.filter (fun x -> isSquareOnBoard ((fst x) + deltaFile, (snd x) + deltaRank)) directions
                List.exists (fun x -> abs board[(fst x) + deltaFile, (snd x) + deltaRank] = WhiteKing) availableDirections
            else
                false

        kingMoves <- directions
            |> List.map (fun (x,y) -> (x + file, y + rank))
            |> List.filter isSquareOnBoard
            |> List.filter (isSquareEmptyOrOpponentPiece gameState.ToPlay)
            |> List.filter isOppositionAllows
            |> List.map (fun (x, y) -> convertToMove file rank x y Empty board[x, y])
                     
        // castling
        if gameState.ToPlay = White then
            if gameState.WhiteCanCastleKingside then
                if board[6, 1] = Empty && board[7, 1] = Empty then
                    kingMoves <- convertToMove 5 1 7 1 Empty Empty :: kingMoves

            if gameState.WhiteCanCastleQueenside then
                if board[4, 1] = Empty && board[3, 1] = Empty && board[2, 1] = Empty then
                    kingMoves <- convertToMove 5 1 3 1 Empty Empty:: kingMoves
        else
            if gameState.BlackCanCastleKingside then
                if board[6, 8] = Empty && board[7, 8] = Empty then
                    kingMoves <- convertToMove 5 8 7 8 Empty Empty :: kingMoves

                if not gameState.BlackCanCastleQueenside then
                    kingMoves <- convertToMove 5 8 3 8 Empty Empty:: kingMoves

        kingMoves

    let generatePawnMoves (file, rank) =
        let direction = if gameState.ToPlay = Black then -1 else 1

        let singleAdvanceMoves =
            if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = Empty then
                if rank + direction = 1 || rank + direction = 8 then
                    [WhiteQueen; WhiteRook; WhiteBishop; WhiteKnight]
                    |> List.map (fun promoteTo -> convertToMove file rank file (rank + direction) (promoteTo * sbyte direction) Empty)
                else [convertToMove file rank file (rank + direction) Empty Empty]
            else []

        let ordinaryCaptureMoves =
            let captureMoves deltaFile =
                if isSquareOnBoard (file + deltaFile, rank + direction) && isSquareOpponentPiece board[file, rank] (file + deltaFile, rank + direction) then
                    if rank + direction = 1 || rank + direction = 8 then
                        [WhiteQueen; WhiteRook; WhiteBishop; WhiteKnight]
                        |> List.map (fun promoteTo -> convertToMove file rank (file + deltaFile) (rank + direction) (promoteTo * sbyte direction) Empty)
                    else [convertToMove file rank (file + deltaFile) (rank + direction) Empty Empty]
                else []
            captureMoves -1 @ captureMoves 1

        let epCaptureMoves =
            let epCapture deltaFile =
                if gameState.EPSquare = Some (file + deltaFile, rank + direction) then
                    [convertToMove file rank (file + deltaFile) (rank + direction) Empty Empty]
                else []
            epCapture -1 @ epCapture 1

        let doubleAdvanceMove =
            if isSquareOnBoard (file, rank + direction) && board[file, rank + direction] = Empty &&
               isSquareOnBoard (file, rank + direction * 2) && board[file, rank + direction * 2] = Empty && (rank = 2 || rank = 7) then
                [convertToMove file rank file (rank + direction * 2) Empty Empty]
            else []

        singleAdvanceMoves @ ordinaryCaptureMoves @ epCaptureMoves @ doubleAdvanceMove

    let generateKnightMoves (file, rank) : Move list =
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

    let generateVectorMoves (file, rank) directions =

        let generateMoves deltaFile deltaRank =
            let rec loop newFile newRank (moves: Move list) =
                let nFile, nRank = newFile + deltaFile, newRank + deltaRank

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
                    | WhiteKnight -> generateKnightMoves (file, rank)
                    | WhiteBishop -> generateVectorMoves (file, rank) [(1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | WhiteRook -> generateVectorMoves (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1)]
                    | WhiteQueen -> generateVectorMoves (file, rank) [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1)]
                    | WhitePawn -> generatePawnMoves (file, rank)
                    | WhiteKing -> generateKingMoves (file, rank)
                    | _ -> []

                availableMoves <- pieceMoves @ availableMoves
            | _ -> ()

    availableMoves

module UCIIntegration 

    open System
    open System.Diagnostics
    open System.Threading
    open BoardHelpers
    open Types
    open Fen
    open UCILogger.Uci
    open MakeMove
    open Search
    
    let private startFen =
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    let mutable private current : Position option = None
    let mutable private stopFlag = false

    let waitForDebuggerIfRequested () =
        match Environment.GetEnvironmentVariable("WAIT_FOR_DEBUGGER") with
        | "1" ->
            while not Debugger.IsAttached do
                Thread.Sleep(100)
        | _ -> ()

    let private loadFen (fen:string) : Position option =
        let board : Board = Array2D.create 8 8 0y
        match tryLoadPositionFromFen board fen with
        | ValueSome p -> Some p
        | ValueNone -> None

    let private newGame () =
        stopFlag <- false
        current <- None

    let moveToUci (mv:Move) : string =
        // Converts your Move (From/To are 1-based Coordinates) to UCI "e2e4" plus promotion.
        let fileChar (f:byte) = char (int 'a' + int f - 1)
        let rankChar (r:byte) = char (int '0' + int r)

        let f1 = fileChar mv.From.File
        let r1 = rankChar mv.From.Rank
        let f2 = fileChar mv.To.File
        let r2 = rankChar mv.To.Rank

        let promo =
            if mv.PromoteTo = 0y then ""
            else
                // PromoteTo is a signed piece code; absKind gives kind code (2..5).
                match PieceCode.absKind mv.PromoteTo with
                | 2y -> "n"
                | 3y -> "b"
                | 4y -> "r"
                | 5y -> "q"
                | _  -> ""  // defensive
        $"{f1}{r1}{f2}{r2}{promo}"

    // Apply a UCI move list to a freshly initialized start position.
    // Returns None if any move is invalid in the current position.
    let private applyUciMovesFromStartPos (moves:string list) : Position option =
        match loadFen startFen with
        | None -> None
        | Some startPos ->
            let mutable p = startPos
            let mutable ok = true

            for uci in moves do
                if ok then
                    let legalMoves = GenerateMoves.generateAllLegalMoves p Attacks.inCheck
                    let wanted = uci.Trim().ToLowerInvariant()

                    match legalMoves |> List.tryFind (fun mv -> moveToUci mv = wanted) with
                    | Some mv ->
                        let _undo = makeMove &p mv
                        ()
                    | None ->
                        ok <- false

            if ok then Some p else None

    let private setPosition (fen:string) (moves:string list) =
        stopFlag <- false

        if fen = "startpos" then
            current <- applyUciMovesFromStartPos moves
        else
            let basePos = loadFen fen
            match basePos with
            | None ->
                current <- None
            | Some pos ->
                // Optional later: apply moves on top of FEN as well.
                current <- Some pos

    let private search (req:SearchRequest) : string * string voption =
        stopFlag <- false

        match current with
        | None ->
            "0000", ValueNone
        | Some pos ->
            match chooseBestMove pos req with
            | ValueSome mv -> moveToUci mv, ValueNone
            | ValueNone -> "0000", ValueNone

    let private stop () =
        stopFlag <- true

    let createApi () : EngineApi =
        { Name = "Fishy"
          Author = "Hugh Cumper"
          Options = []
          NewGame = newGame
          SetPosition = setPosition
          Search = search
          Stop = stop

        }

module UCIIntegration

open Types
open Fen
open MakeMove
open UCI

let mutable currentPosition : Position option = None
let mutable stopRequested = false

let newGame () =
    currentPosition <- None
    stopRequested <- false

let setPosition (fen:string) (moves:string list) =
    let board : Board = Array2D.create 8 8 0y

    let basePos =
        if fen = "startpos" then
            tryLoadPositionFromFen board
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        else
            tryLoadPositionFromFen board fen

    match basePos with
    | ValueNone ->
        currentPosition <- None
    | ValueSome pos ->
        let mutable p = pos
        for mvStr in moves do
            let mv = parseUciMove p mvStr
            let _ = makeMove &p mv
            ()
        currentPosition <- Some p

let private pickAnyLegalMoveUci (pos:Position) : string =
    // Minimal: generate all legal moves and return the first as UCI.
    // This requires you to implement moveToUci below.
    let moves = GenerateMoves.generateAllLegalMoves pos Attacks.inCheck
    match moves with
    | [] -> "0000"
    | mv :: _ -> moveToUci mv

and moveToUci (mv:Move) : string =
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
    
let search (req: SearchRequest) =
    stopRequested <- false

    match currentPosition with
    | None -> "0000", ValueNone
    | Some pos ->

        let depth =
            match req.Depth with
            | ValueSome d -> d
            | ValueNone -> 4   // default depth

        let bestMove =
            findBestMove pos depth (fun () -> stopRequested)

        let bestMoveUci = moveToUci bestMove

        bestMoveUci, ValueNone
        
let stop () =
    stopRequested <- true
    
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
        
let mutable currentPosition : Position option = None
let mutable stopRequested = false

let createApi () : EngineApi =
    { Name = "MyEngine"
      Author = "Your Name"
      Options = []
      NewGame = newGame
      SetPosition = setPosition
      Search = search
      Stop = stop }
    


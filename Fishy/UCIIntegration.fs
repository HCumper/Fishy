module UCIIntegration

open System
open System.Diagnostics
open System.Threading

open Types
open Fen
open Uci
open MakeMove

open BoardHelpers
open PieceCode

open Configuration
open Search

let private startFen =
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let tt =
    TranspositionTable.create
        { Mb = 128
          BucketSize = 4
          MaxAge = 8 }

let mutable private current : Position option = None
let mutable private stopFlag = false

let waitForDebuggerIfRequested () =
    match Environment.GetEnvironmentVariable("WAIT_FOR_DEBUGGER") with
    | "1" ->
        while not Debugger.IsAttached do
            Thread.Sleep(100)
    | _ -> ()

let private loadFen (fen:string) : Position option =
    let board : Board = Array.zeroCreate 64
    match tryLoadPositionFromFen board fen with
    | ValueSome p -> Some p
    | ValueNone -> None

let private newGame () =
    stopFlag <- false
    current <- None

let moveToUci (mv:Move) : string =
    // Internal coordinates are 0-based (0..7)
    let fileChar (f:byte) = char (int 'a' + int f)
    let rankChar (r:byte) = char (int '1' + int r)

    let f1 = fileChar mv.From.File
    let r1 = rankChar mv.From.Rank
    let f2 = fileChar mv.To.File
    let r2 = rankChar mv.To.Rank

    let promo =
        if mv.PromoteTo = 0y then ""
        else
            match PieceCode.absKind mv.PromoteTo with
            | Knight -> "n"
            | Bishop -> "b"
            | Rook   -> "r"
            | Queen  -> "q"
            | _      -> ""

    $"{f1}{r1}{f2}{r2}{promo}"

let private applyUciMoves (basePos:Position) (moves:string list) : Position option =
    let mutable p = basePos
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
            
let private setPosition (commandLine: string) (_moves: string list) =
    stopFlag <- false

    // Tokenize once, normalize for keywords
    let toks =
        commandLine.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

    let toksLower = toks |> List.map (fun s -> s.ToLowerInvariant())

    let tryFindIndex (s:string) (xs:string list) =
        xs |> List.tryFindIndex ((=) s)

    let after i xs = xs |> List.skip (i + 1)

    match toksLower with
    | [] ->
        current <- None

    // UCI "position startpos [moves ...]"
    | "position" :: "startpos" :: rest ->
        match tryFindIndex "moves" rest |> Option.map (fun i -> i) with
        | None ->
            current <- applyUciMovesFromStartPos []
        | Some mi ->
            let moveList = rest |> after mi
            current <- applyUciMovesFromStartPos moveList

    // UCI "position fen <6 fields> [moves ...]"
    | "position" :: "fen" :: rest ->
        // rest begins with 6 FEN fields (placement stm castling ep half full)
        if List.length rest < 6 then
            current <- None
        else
            let fenFields = rest |> List.take 6
            let fen = String.Join(" ", fenFields)

            let movesAfterFen = rest |> List.skip 6
            let moveList =
                match tryFindIndex "moves" movesAfterFen with
                | None -> []
                | Some mi -> movesAfterFen |> after mi

            match loadFen fen with
            | None -> current <- None
            | Some p0 ->
                current <- applyUciMoves p0 moveList

    // If caller passed something else, attempt to treat whole line as a FEN
    | _ ->
        match loadFen commandLine with
        | None -> current <- None
        | Some p0 ->
            current <- Some p0
let private search (req:SearchRequest) : string * string voption =
    stopFlag <- false

    match current with
    | None ->
        "0000", ValueNone
    | Some pos ->
        // NOTE: for stop to work, chooseBestMove must consult stopFlag (or a CancellationToken).
        match chooseBestMove tt pos req with
        | ValueSome mv -> moveToUci mv, ValueNone
        | ValueNone -> "0000", ValueNone

let private stop () =
    stopFlag <- true

let setOption (name:string) (valueOpt:string voption) : unit =
    let n = name.Trim().ToLowerInvariant()

    match n with

    | "debug" ->
        match valueOpt with
        | ValueSome v ->
            debugEnabled <- (v.Trim().ToLowerInvariant() = "true")
        | ValueNone ->
            ()   // ignore malformed

    | "hash" ->
        match valueOpt with
        | ValueSome v ->
            match System.Int32.TryParse(v.Trim()) with
            | true, mb when mb >= 1 && mb <= 4096 ->
                pendingHashMb <- Some mb
            | _ ->
                ()
        | ValueNone ->
            ()

    | _ ->
        ()
        
let createApi () : EngineApi =
    { Name = "Fishy"
      Author = "Hugh Cumper"
      Options =
          [ Uci.Check("Debug", false)
            Uci.Spin("Hash", 128, 1, 4096) ]
      NewGame = newGame
      SetPosition = setPosition
      Search = search
      Stop = stop
      SetOption = setOption }
    

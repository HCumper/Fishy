module UCI

open System
open System.IO
open System.Globalization
open Types
// -----------------------------
// UCI types
// -----------------------------

type UciOption =
    | Spin of name:string * defaultValue:int * minValue:int * maxValue:int
    | Check of name:string * defaultValue:bool
    | Combo of name:string * defaultValue:string * vars:string list
    | String of name:string * defaultValue:string
    | Button of name:string

type UciPosition =
    { Fen: string
      Moves: string list }  // UCI moves like "e2e4", "e7e8q"

// Commands we care about
type UciCommand =
    | Uci
    | IsReady
    | UciNewGame
    | Position of UciPosition
    | Go of raw:string
    | Stop
    | Quit
    | SetOption of name:string * value:string
    | Unknown of raw:string

// -----------------------------
// Engine hooks you plug in
// -----------------------------

type SearchRequest =
    { Depth: int voption
      MoveTimeMs: int voption
      WTimeMs: int voption
      BTimeMs: int voption
      WIncMs: int voption
      BIncMs: int voption
      Nodes: int64 voption
      Mate: int voption
      Infinite: bool }

type EngineApi =
    { Name: string
      Author: string
      Options: UciOption list
      NewGame: unit -> unit
      SetPosition: string -> string list -> unit
      Search: SearchRequest -> string * string voption
      Stop: unit -> unit }
    
// -----------------------------
// Parsing helpers
// -----------------------------

let inline private splitTokens (s:string) =
    s.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let private tryParseInt (s:string) =
    match Int32.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, v -> ValueSome v
    | _ -> ValueNone

let private tryParseInt64 (s:string) =
    match Int64.TryParse(s, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, v -> ValueSome v
    | _ -> ValueNone

let private parseSetOption (tokens:string list) (raw:string) =
    // setoption name <name...> [value <value...>]
    // name can contain spaces; value can contain spaces.
    // We'll parse by locating "name" and optional "value".
    let idxName = tokens |> List.tryFindIndex ((=) "name")
    match idxName with
    | None -> Unknown raw
    | Some i ->
        let afterName = tokens |> List.skip (i + 1)
        let idxValue = afterName |> List.tryFindIndex ((=) "value")
        match idxValue with
        | None ->
            // everything after "name" is name
            let name = String.Join(" ", afterName)
            SetOption(name, "")
        | Some j ->
            let nameParts = afterName |> List.take j
            let valueParts = afterName |> List.skip (j + 1)
            let name = String.Join(" ", nameParts)
            let value = String.Join(" ", valueParts)
            SetOption(name, value)

let private parsePosition (tokens:string list) (raw:string) =
    // position startpos [moves ...]
    // position fen <fen 6 fields> [moves ...]
    match tokens with
    | "position" :: "startpos" :: rest ->
        let moves =
            match rest with
            | "moves" :: ms -> ms
            | _ -> []
        Position { Fen = "startpos"; Moves = moves }
    | "position" :: "fen" :: rest ->
        // FEN is 6 fields, can’t just take until "moves" because fen has spaces.
        // Find "moves" token if present; before it should contain exactly 6 fen tokens.
        let idxMoves = rest |> List.tryFindIndex ((=) "moves")
        let fenTokens, moveTokens =
            match idxMoves with
            | None -> rest, []
            | Some i -> rest |> List.take i, rest |> List.skip (i + 1)

        if fenTokens.Length < 6 then Unknown raw
        else
            let fen = String.Join(" ", fenTokens |> List.take 6)
            Position { Fen = fen; Moves = moveTokens }
    | _ -> Unknown raw

let private parseGo (tokens:string list) (raw:string) =
    // We'll keep raw and parse out common fields.
    // "go" can contain many optional tokens.
    Go raw

let private parseCommand (line:string) : UciCommand =
    let tokens = splitTokens line
    match tokens with
    | [] -> Unknown line
    | "uci" :: _ -> Uci
    | "isready" :: _ -> IsReady
    | "ucinewgame" :: _ -> UciNewGame
    | "quit" :: _ -> Quit
    | "stop" :: _ -> Stop
    | "setoption" :: _ -> parseSetOption tokens line
    | "position" :: _ -> parsePosition tokens line
    | "go" :: _ -> parseGo tokens line
    | _ -> Unknown line

let private parseGoRequest (raw:string) : SearchRequest =
    // Parse most common: depth, movetime, wtime, btime, winc, binc, nodes, mate, infinite
    let tokens = splitTokens raw
    // tokens[0] should be "go"
    let mutable depth = ValueNone
    let mutable movetime = ValueNone
    let mutable wtime = ValueNone
    let mutable btime = ValueNone
    let mutable winc = ValueNone
    let mutable binc = ValueNone
    let mutable nodes = ValueNone
    let mutable mate = ValueNone
    let mutable infinite = false

    let rec loop ts =
        match ts with
        | [] -> ()
        | "depth" :: v :: rest ->
            depth <- tryParseInt v
            loop rest
        | "movetime" :: v :: rest ->
            movetime <- tryParseInt v
            loop rest
        | "wtime" :: v :: rest ->
            wtime <- tryParseInt v
            loop rest
        | "btime" :: v :: rest ->
            btime <- tryParseInt v
            loop rest
        | "winc" :: v :: rest ->
            winc <- tryParseInt v
            loop rest
        | "binc" :: v :: rest ->
            binc <- tryParseInt v
            loop rest
        | "nodes" :: v :: rest ->
            nodes <- tryParseInt64 v
            loop rest
        | "mate" :: v :: rest ->
            mate <- tryParseInt v
            loop rest
        | "infinite" :: rest ->
            infinite <- true
            loop rest
        | _ :: rest ->
            loop rest

    // Skip "go"
    loop (tokens |> List.skip 1)

    { Depth = depth
      MoveTimeMs = movetime
      WTimeMs = wtime
      BTimeMs = btime
      WIncMs = winc
      BIncMs = binc
      Nodes = nodes
      Mate = mate
      Infinite = infinite }

// -----------------------------
// Output helpers
// -----------------------------

let private writeLine (w:TextWriter) (s:string) =
    w.WriteLine(s)
    w.Flush()

let private optionToUci (opt:UciOption) : string =
    match opt with
    | Spin(name, defv, minv, maxv) ->
        $"option name {name} type spin default {defv} min {minv} max {maxv}"
    | Check(name, defv) ->
        let defStr = if defv then "true" else "false"
        $"option name {name} type check default {defStr}"
    | Combo(name, defv, vars) ->
        let varsStr = vars |> List.map (fun v -> $"var {v}") |> String.concat " "
        $"option name {name} type combo default {defv} {varsStr}"
    | String(name, defv) ->
        $"option name {name} type string default {defv}"
    | Button(name) ->
        $"option name {name} type button"

// -----------------------------
// UCI loop
// -----------------------------

let run (api: EngineApi) =
    let input = Console.In
    let output = Console.Out

    let mutable running = true

    while running do
        let line = input.ReadLine()
        if isNull line then
            running <- false
        else
            match parseCommand line with
            | Uci ->
                writeLine output $"id name {api.Name}"
                writeLine output $"id author {api.Author}"
                for opt in api.Options do
                    writeLine output (optionToUci opt)
                writeLine output "uciok"

            | IsReady ->
                writeLine output "readyok"

            | UciNewGame ->
                api.NewGame()

            | SetOption(name, value) ->
                // If you later wire options into the engine, do it here.
                // For now, ignore unknown options.
                ()

            | Position p ->
                api.SetPosition p.Fen p.Moves

            | Go raw ->
                let req = parseGoRequest raw
                let best, ponderOpt = api.Search req
                match ponderOpt with
                | ValueSome pm -> writeLine output $"bestmove {best} ponder {pm}"
                | ValueNone -> writeLine output $"bestmove {best}"

            | Stop ->
                api.Stop()

            | Quit ->
                api.Stop()
                running <- false

            | Unknown _ ->
                // UCI says we should ignore unrecognized commands.
                ()

//
// open System.Threading.Tasks
// open TranspositionTable
// open UCILogger
// open Fishy
// open MakeMove
// open FENParser
// open Evaluation
// open Types
// open LookAhead
// open Zobrist
//
// let engine = "Fishy"
// let version = "0.1"
//
// // must retain state of the game as UCI is stateful
// let mutable sessionBoard = Array2D.createBased 1 1 8 8 0y
// let mutable sessionState =
//     { WhiteCanCastleKingside = true
//       WhiteCanCastleQueenside = true
//       BlackCanCastleKingside = true
//       BlackCanCastleQueenside = true
//       ToPlay = White
//       EPSquare = None
//       HalfMoveClock = 0
//       FullMoveNumber = 0
//       HashKey = 0
//     }
//
// let mutable myColor = White
// let mutable level = 0
//
// let setupPosition (cmd: string) =
//     let applyMoves move =
//         sessionState <- snd (parseAndMakeMove sessionBoard sessionState move)
//
//     let cmdList = cmd.Split [|' '|]
//     let fen =
//         if cmdList[1] = "startpos" then
//             "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
//         else
//             cmdList[1]
//     let parseResult = parseFEN fen
//     sessionBoard <- fst parseResult
//     sessionState <- snd parseResult
//     myColor <- White
//     if cmdList.Length > 2 && cmdList[2] = "moves" then
//         let moves = Array.skip 3 cmdList
//         if moves.Length % 2 = 0 then myColor <- White else myColor <- Black
//         Array.iter (applyMoves) moves
//     writeOutput "position set up"
//
// let go (cmd: string) =
//     let cmdList = Array.toList (cmd.Split [|' '|])
//
//     let time =
//         match myColor with
//         | White -> List.tryFindIndex (fun parm -> parm = "wtime") cmdList
//         | _ -> List.tryFindIndex (fun parm -> parm = "btime") cmdList
//     // let level =
//     //     match time with
//     //     | Some length -> min ((int (cmdList[(int length) + 1])) / 10000) 4
//     //     | None -> 4
//     level <- 4
//     makeLogEntry $"level {level}"
// //    let valuation = chooseEngineMove sessionBoard level sessionState
// //    get value from transposition table
//     // let pv = List.map convertNumbersToCoordinates (List.rev valuation)
//     // writeOutput $"bestmove {List.head pv}"
//     ()
// let rec processCommand () =
//     let mutable exit = false
//
//     while not exit do
//         let cmd = readInput ()
//
//         match cmd with
//         | cmd when cmd.StartsWith("run") ->
//             initializePlacementValues () |> ignore
//             setupPosition "startpos startpos"
//             go ""
//         | cmd when cmd.StartsWith("debug") -> ()
//         | cmd when cmd.StartsWith("go") -> go cmd
//         | cmd when cmd.StartsWith("isready") -> writeOutput "readyok"
//         | cmd when cmd.StartsWith("wac") -> ()
//         | cmd when cmd.StartsWith("position") -> setupPosition cmd
//         | cmd when cmd.StartsWith("quit") -> makeLogEntry "quitting"
//         | cmd when cmd.StartsWith("register") -> ()
//         | cmd when cmd.StartsWith("savefen") -> ()
//         | cmd when cmd.StartsWith("setoption") -> ()
//         | cmd when cmd.StartsWith("startpos") -> ()
//         | cmd when cmd.StartsWith("stop") ->
//             writeOutput("Bestmove e2e4")
//             makeLogEntry "Incoming: stop"
//         | cmd when cmd.StartsWith("test") -> ()
//         | cmd when cmd.StartsWith("uci") ->
//             writeOutput ("id name " + engine)
//             writeOutput "id author Hugh Cumper"
//             writeOutput "option:"
//             writeOutput "uciok"
//             initializePlacementValues () |> ignore
//         | cmd when cmd.StartsWith("ucinewgame") ->
//     //        Transpositions.resetTranspositionTable
//             ()
//         | _ -> writeOutput ("Unrecognized uci command " + cmd)
//
//         if cmd.StartsWith("quit") || cmd.StartsWith("stop") then
//             exit <- true
//
// let reportToUCI () =
//     let rec fetchMovesFromTranspositionTable board state (movesSoFar: Move list) =
//         let hash = hashAPosition board state
//         match transpositionTableLookupByHash hash with
//         | Some (score, confidence, move) ->
//             match confidence with
//             | 0 -> movesSoFar
//             | _ ->
//                 let updatedBoard, updateState = makeMove board state move
//                 move :: fetchMovesFromTranspositionTable updatedBoard updateState movesSoFar
//         | None -> movesSoFar
//
//     let topLevelScore board state =
//         let hash = hashAPosition board state
//         match transpositionTableLookupByHash hash with
//         | Some (score,_ , _) -> score
//         | None -> failwith "no top level node"
//     ()
//     // try
//     //     if repNodes > 0 then
//     //         let pv = fetchMovesFromTranspositionTable sessionBoard sessionState []
//     //     //    let score = topLevelScore sessionBoard sessionState
//     //         let score = 8
//     //         writePV score level repNodes (int repStopwatch.ElapsedMilliseconds) pv
//     //         writeCurrmove repCurrMove repMoveNumber (repCacheHits / (repCacheMisses+1))
//     // with
//     // | ex -> ()
//
// let rec oneSecondReporting () =
//     async {
//         reportToUCI ()
//         do! Task.Delay(1000) |> Async.AwaitTask
//         return! oneSecondReporting ()
//     }
//
// let setupOneSecondReporting () =
//     async {
//         do! oneSecondReporting ()
//     }
//
// let reporting () =
//     Async.StartImmediate(setupOneSecondReporting ())
//
// [<EntryPoint>]
// let main _ =
//     reporting ()
// //    initializeLogging ()
//     processCommand ()
//     0

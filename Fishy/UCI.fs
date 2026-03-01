module UCILogger

open System
open System.IO
open Types

let outputFile = @"I:\\temp\\uci.log"
#if DEBUG
let writer = File.AppendText outputFile
#endif

let initializeLogging () =
#if DEBUG
    writer.AutoFlush <- true
    writer.WriteLine ""
    writer.WriteLine $"New Fishy session started {DateTime.Now}"
    writer.WriteLine ""
#endif
    ()

let convertNumbersToCoordinates (move: Move) =
    let files = " abcdefgh"
    let ranks = " 12345678"
    $"{files[int move.From.File]}{ranks[int move.From.Rank]}{files[int move.To.File]}{ranks[int move.
                                                                                 To.Rank]} "

let writePV evaluation depth nodes time pv =
    let pvString = List.fold (fun acc item -> acc + convertNumbersToCoordinates item) "" pv
    let eval = if evaluation < -1000000 then 0 else evaluation
    let cmdString = $"info depth {depth} score cp {eval} time {time} nodes {nodes} nps {(nodes * 1000 / (time + 1))} pv {pvString}"
    Console.WriteLine cmdString
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay} Outgoing: {cmdString}"
#endif
    ()

let writeCurrmove currmove currmoveNumber hashfull =
    let currMoveString = convertNumbersToCoordinates currmove
    let cmdString = $"info currmove {currMoveString} hashfull {hashfull} currmovenumber {currmoveNumber}"
    Console.WriteLine cmdString
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay} Outgoing: {cmdString}"
#endif
    ()

let writeOutput (cmd: string) =
    Console.WriteLine cmd
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay} Outgoing: {cmd}"
#endif
    ()

let makeLogEntry (cmd: string) =
#if DEBUG
    writer.WriteLine $"Debug only: {cmd}"
#endif
    ()

let rec readInput () =
    let cmd = Console.ReadLine ()
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay} Incoming: {cmd}"
#endif
    cmd
    
    
module Uci =

    open System.Globalization

    type UciOption =
        | Spin of name:string * defaultValue:int * minValue:int * maxValue:int
        | Check of name:string * defaultValue:bool
        | Combo of name:string * defaultValue:string * vars:string list
        | String of name:string * defaultValue:string
        | Button of name:string

    type UciPosition =
        { Fen: string
          Moves: string list }

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

    let private parseGoRequest (raw:string) : SearchRequest =
        let tokens = splitTokens raw
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

        // skip leading "go"
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

    let private writeLine (w:TextWriter) (s:string) =
    //    w.WriteLine(s)
        writeOutput(s)
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

    let private parsePosition (tokens:string list) =
        // position startpos [moves ...]
        // position fen <fen 6 fields> [moves ...]
        match tokens with
        | "position" :: "startpos" :: rest ->
            let moves =
                match rest with
                | "moves" :: ms -> ms
                | _ -> []
            ValueSome { Fen = "startpos"; Moves = moves }
        | "position" :: "fen" :: rest ->
            let idxMoves = rest |> List.tryFindIndex ((=) "moves")
            let fenTokens, moveTokens =
                match idxMoves with
                | None -> rest, []
                | Some i -> rest |> List.take i, rest |> List.skip (i + 1)
            if fenTokens.Length < 6 then ValueNone
            else
                let fen = String.Join(" ", fenTokens |> List.take 6)
                ValueSome { Fen = fen; Moves = moveTokens }
        | _ ->
            ValueNone

    let run (api:EngineApi) =
        let input = Console.In
        let output = Console.Out

        let mutable running = true

        while running do
            let line = readInput ()
            if isNull line then
                running <- false
            else
                let tokens = splitTokens line
                match tokens with
                | [] -> ()
                | "uci" :: _ ->
                    initializeLogging()
                    writeLine output $"id name {api.Name}"
                    writeLine output $"id author {api.Author}"
                    for opt in api.Options do
                        writeLine output (optionToUci opt)
                    writeLine output "uciok"

                | "isready" :: _ ->
                    writeLine output "readyok"

                | "ucinewgame" :: _ ->
                    api.NewGame()

                | "setoption" :: _ ->
                    // Ignore for now.
                    ()
                // this is the GUI's way of telling us its move containing the whole game
                | "position" :: _ ->
                    match parsePosition tokens with
                    | ValueSome p -> api.SetPosition p.Fen p.Moves
                    | ValueNone -> ()

                | "go" :: _ ->
                    let req = parseGoRequest line
                    let best, ponderOpt = api.Search req
                    // its always the engine that sends bestmove
                    match ponderOpt with
                    | ValueSome pm -> writeLine output $"bestmove {best} ponder {pm}"
                    | ValueNone -> writeLine output $"bestmove {best}"

                | "stop" :: _ ->
                    api.Stop()

                | "quit" :: _ ->
                    api.Stop()
                    running <- false

                | _ ->
                    // Ignore unknown commands
                    ()


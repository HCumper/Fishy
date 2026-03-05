module Uci

open System
open System.Globalization
open System.IO

let mutable enableLogging = true

let private logFile = @"i:\temp\uci.log"

let private ensureLogDir () =
    let dir = Path.GetDirectoryName(logFile)
    if not (Directory.Exists(dir)) then
        Directory.CreateDirectory(dir) |> ignore

let private log prefix (line:string) =
    if enableLogging then
        try
            ensureLogDir()

            let entry =
                sprintf "%s %s %s"
                    (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff"))
                    prefix
                    line

            File.AppendAllText(logFile, entry + Environment.NewLine)
        with
        | _ -> ()   // never crash engine because of logging

/// Send a UCI line to GUI
let send (line:string) =
    log "-->" line
    Console.WriteLine(line)
    Console.Out.Flush()

/// Read a UCI line from GUI
let read () =
    let line = Console.ReadLine()
    if not (isNull line) then
        log "<--" line
    line

/// Enable/disable logging at runtime
let setLogging enabled =
    enableLogging <- enabled

/// Convenience helpers for common UCI messages
let sendId name author =
    send $"id name {name}"
    send $"id author {author}"

let sendOption line =
    send $"option {line}"

let sendUciOk () =
    send "uciok"

let sendReadyOk () =
    send "readyok"

let sendBestMove move =
    send $"bestmove {move}"

let sendInfo msg =
    send $"info {msg}"
        
// =============================
// UCI OPTION TYPES
// =============================

type UciOption =
    | Spin of name:string * defaultValue:int * minValue:int * maxValue:int
    | Check of name:string * defaultValue:bool
    | Combo of name:string * defaultValue:string * vars:string list
    | String of name:string * defaultValue:string
    | Button of name:string

// =============================
// SEARCH TYPES
// =============================

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
      Stop: unit -> unit
      SetOption: string -> string voption -> unit }

// =============================
// UTILITIES
// =============================

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

let private tryParseBool (s:string) =
    match s.Trim().ToLowerInvariant() with
    | "true" | "1" | "yes" | "on"  -> ValueSome true
    | "false"| "0" | "no"  | "off" -> ValueSome false
    | _ -> ValueNone

let private clamp minv maxv v =
    if v < minv then minv
    elif v > maxv then maxv
    else v


// =============================
// GO PARSE
// =============================

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
        | "depth" :: v :: rest -> depth <- tryParseInt v; loop rest
        | "movetime" :: v :: rest -> movetime <- tryParseInt v; loop rest
        | "wtime" :: v :: rest -> wtime <- tryParseInt v; loop rest
        | "btime" :: v :: rest -> btime <- tryParseInt v; loop rest
        | "winc" :: v :: rest -> winc <- tryParseInt v; loop rest
        | "binc" :: v :: rest -> binc <- tryParseInt v; loop rest
        | "nodes" :: v :: rest -> nodes <- tryParseInt64 v; loop rest
        | "mate" :: v :: rest -> mate <- tryParseInt v; loop rest
        | "infinite" :: rest -> infinite <- true; loop rest
        | _ :: rest -> loop rest

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

// =============================
// Helpers
// =============================

let private tryFindIndex (x:string) (ts:string list) =
    ts |> List.tryFindIndex ((=) x)

let private joinTokens (ts:string list) =
    String.Join(" ", ts)

/// Use UciIO instead of Console.WriteLine directly.
let writeInfo depth (nodeCount: int64) (nps: int64) now eval =
    // keep your existing format (note your original hard-coded depth 0)
    send $"info depth 0 nodes {nodeCount} nps {nps} time {now} score cp {eval}"

let private parseSetOption (tokens:string list) : string voption * string voption =
    // tokens begins with "setoption"
    // Example: setoption name Hash value 256
    // Example: setoption name Clear Hash
    // Example: setoption name UCI_Elo value 1600
    match tryFindIndex "name" tokens with
    | None -> ValueNone, ValueNone
    | Some nameIdx ->
        let afterName = tokens |> List.skip (nameIdx + 1)
        match tryFindIndex "value" afterName with
        | None ->
            // name is rest; no value
            let name = joinTokens afterName
            if name = "" then ValueNone, ValueNone
            else ValueSome name, ValueNone
        | Some valueIdx ->
            let nameTokens  = afterName |> List.take valueIdx
            let valueTokens = afterName |> List.skip (valueIdx + 1)
            let name = joinTokens nameTokens
            if name = "" then ValueNone, ValueNone
            else
                let v = joinTokens valueTokens
                let value = if v = "" then ValueNone else ValueSome v
                ValueSome name, value

// =============================
// UCI LOOP
// =============================

let run (api:EngineApi) =

    let mutable running = true

    while running do
        // Read via UciIO so inbound traffic is logged
        let line = read()

        if isNull line then
            running <- false
        else
            let tokens = splitTokens line

            match tokens with
            | [] -> ()

            | "uci" :: _ ->
                // Write via UciIO so outbound traffic is logged and flushed
                send $"id name {api.Name}"
                send $"id author {api.Author}"

                for opt in api.Options do
                    match opt with
                    | Spin(n, defv, minv, maxv) ->
                        send $"option name {n} type spin default {defv} min {minv} max {maxv}"
                    | Check(n, defv) ->
                        let d = if defv then "true" else "false"
                        send $"option name {n} type check default {d}"
                    | Combo(n, defv, vars) ->
                        let varsStr = vars |> List.map (fun v -> $"var {v}") |> String.concat " "
                        send $"option name {n} type combo default {defv} {varsStr}"
                    | String(n, defv) ->
                        send $"option name {n} type string default {defv}"
                    | Button(n) ->
                        send $"option name {n} type button"

                send "uciok"

            | "isready" :: _ ->
                send "readyok"

            | "ucinewgame" :: _ ->
                api.NewGame()

            | "setoption" :: _ ->
                let nameOpt, valueOpt = parseSetOption tokens
                match nameOpt with
                | ValueSome name -> api.SetOption name valueOpt
                | ValueNone -> ()

            | "position" :: _ ->
                api.SetPosition line []

            | "go" :: _ ->
                let req = parseGoRequest line
                let best, ponderOpt = api.Search req

                match ponderOpt with
                | ValueSome pm -> send $"bestmove {best} ponder {pm}"
                | ValueNone    -> send $"bestmove {best}"

            | "stop" :: _ ->
                api.Stop()

            | "quit" :: _ ->
                api.Stop()
                running <- false

            | _ -> ()
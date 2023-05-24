module Fish.UCI

open System
open Types
open Logger
open Chess
open MakeMove
let engine = "Fishy"
let version = "0.1"

let player = true
let go = false
let quit = false

let logWriter = UCILogger()

let output (text: string) =
    Console.WriteLine text
    logWriter.makeLogEntry "Outgoing " text
    ()

let startGame (cmd: string) =
    let cmdList = cmd.Split [|' '|]
    if cmdList[1] = "startpos" then
        setupStartingPosition ()
        output ("info string Position set to the starting position")
        if cmdList.Length > 2 && cmdList[2] = "moves" then
            let moves = Array.skip 3 cmdList
            Array.iter parseAndMakeMove moves
    ()

let rec processCommand () =
    let move_time_seconds = 10

    if go then ()

    let cmd = Console.ReadLine ()
    logWriter.makeLogEntry "Incoming " cmd

    match cmd with
    | cmd when cmd[0..1] = "go" ->   // go wtime 300000 btime 300000 winc 0 binc 0
        output ("bestmove e2e4")
    | cmd when cmd[0..9] = "ucinewgame" -> ()
    | cmd when cmd[0..7] = "position" -> startGame cmd
    | cmd when cmd[0..8] = "startpos " -> ()
    | cmd when cmd[0..2] = "go " -> ()
    | cmd when cmd[0..8] = "setoption" -> ()
    | "test" -> () // For debugging exact positions
    | "wac" -> ()

    | "savefen" -> ()
    | cmd when cmd[0..2] = "uci" ->
        output ("id name " + engine)
        output ("id author Hugh Cumper")
        output ("option:")
        output ("uciok")
    | cmd when cmd[0..6] = "isready" -> output ("readyok")
    | cmd when cmd[0..3] = "quit" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | cmd when cmd[0..3] = "stop" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | _ -> output ("Unrecognized uci command " + cmd)

    if cmd[0..3] <> "quit" && cmd[0..3] <> "stop" then
        processCommand ()

[<EntryPoint>]
//Thread.Sleep 10000
processCommand ()

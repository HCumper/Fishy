﻿module Fish.UCI

open System
open Types
open Logger
open Chess
open MakeMove
open FENParser
let engine = "Fishy"
let version = "0.1"

let player = true
let go = false
let quit = false

let logWriter = UCILogger()

let convertNumbersToCoordinates ((fromFile, fromRank), (toFile, toRank), promoteTo) =
    let files = "abcdefgh"
    let ranks = "12345678"
    let fileChar1 = files.[fromFile - 1]
    let rankChar1 = ranks.[fromRank - 1]
    let fileChar2 = files.[toFile - 1]
    let rankChar2 = ranks.[toRank - 1]
    sprintf "%c%c%c%c" fileChar1 rankChar1 fileChar2 rankChar2

let output (text: string) =
    Console.WriteLine text
    logWriter.makeLogEntry "Outgoing " text
    ()

let startGame (cmd: string) =
    let cmdList = cmd.Split [|' '|]
    if cmdList[1] = "startpos" then
        parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" |> ignore

        output ("info string Position set to the starting position")
        if cmdList.Length > 2 && cmdList[2] = "moves" then
            let moves = Array.skip 3 cmdList
            Array.iter parseAndMakeMove moves
    ()

let rec processCommand () =

    if go then ()

    let cmd = Console.ReadLine ()
    logWriter.makeLogEntry "Incoming " cmd

    match cmd with
    | cmd when cmd[0..1] = "go" ->
        let move = engineMove () |> List.head |> convertNumbersToCoordinates
        output ($"bestmove {move}")
    | cmd when cmd[0..9] = "ucinewgame" -> ()
    | cmd when cmd[0..7] = "position" -> startGame cmd
    | cmd when cmd[0..8] = "startpos " -> ()
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
processCommand ()

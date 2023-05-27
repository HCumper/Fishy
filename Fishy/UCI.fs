﻿module Fish.UCI

open System
open System.Diagnostics
open Logger
open Fishy
open MakeMove
open FENParser
open Evaluation
open Types
open LookAhead

let engine = "Fishy"
let version = "0.1"

let player = true
let quit = false

let (gameState: OtherState) = {
    WhiteKingMoved = false
    WhiteQRMoved = false
    WhiteKRMoved = false
    BlackKingMoved = false
    BlackQRMoved = false
    BlackKRMoved = false
    EPSquare = None
    ToPlay = White }

let logWriter = UCILogger()

let output (text: string) =
    logWriter.makeLogEntry "Outgoing " text
    Console.WriteLine text
    Debug.WriteLine("Outgoing:  " + text);

    ()

let convertNumbersToCoordinates (move: Move) =
    let files = " abcdefgh"
    let ranks = " 12345678"
    let fileChar1 = files[move.fromFile]
    let rankChar1 = ranks[move.fromRank]
    let fileChar2 = files[move.toFile]
    let rankChar2 = ranks[move.toRank]
    $"%c{fileChar1}%c{rankChar1}%c{fileChar2}%c{rankChar2} "

let setupPosition (cmd: string) =

    let cmdList = cmd.Split [|' '|]
    let fen =
        if cmdList[1] = "startpos" then
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        else
            cmdList[1]
    parseFEN fen |> ignore

    if cmdList.Length > 2 && cmdList[2] = "moves" then
        let moves = Array.skip 3 cmdList
        let moveList = Array.iter (fun x -> parseAndMakeMove currentBoard gameState x |> ignore) moves
        ()
    else
        ()

// various options unsupported
let go (cmd: String) =
    let cmdList = Array.toList (cmd.Split [|' '|])

    let level =
        match List.tryFindIndex (fun parm -> parm = "wtime") cmdList with
        | Some index -> (int cmdList[index+1]) / 5000
        | None -> 3
    let valuation = chooseEngineMove 4
    let (pv: string list) = List.map (fun x -> convertNumbersToCoordinates x) (List.rev (snd valuation))
    let displayPV = List.reduce (+) pv
    let score = (fst valuation) / 10
//    output $"info score cp {score} pv {displayPV}"
    output $"bestmove {pv.Head}"

let rec processCommand () =

    let cmd = Console.ReadLine ()
    Debug.WriteLine("Incoming:  " + cmd);
    logWriter.makeLogEntry "Incoming " cmd

    match cmd with
    | cmd when cmd[0..4] = "debug" -> logWriter.makeLogEntry "Outgoing " "- debug command received"
    | cmd when cmd[0..1] = "go" -> go cmd
    | cmd when cmd[0..6] = "isready" -> output "readyok"
    | "wac" -> ()
    | cmd when cmd[0..7] = "position" -> setupPosition cmd
    | cmd when cmd[0..3] = "quit" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | cmd when cmd[0..7] = "register" -> ()
    | "savefen" -> ()
    | cmd when cmd[0..8] = "setoption" -> ()
    | cmd when cmd[0..8] = "startpos " -> ()
    | cmd when cmd[0..3] = "stop" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | "test" -> () // For debugging exact positions
    | cmd when cmd[0..2] = "uci" ->
        output ("id name " + engine)
        output "id author Hugh Cumper"
        output "option:"
        output "uciok"
        initializePlacementValues () |> ignore
    | cmd when cmd[0..9] = "ucinewgame" -> ()
    | _ -> output ("Unrecognized uci command " + cmd)

    if cmd[0..3] <> "quit" && cmd[0..3] <> "stop" then
        processCommand ()

[<EntryPoint>]
processCommand ()

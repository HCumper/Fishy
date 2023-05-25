module Fish.UCI

open System
open Logger
open Fishy
open MakeMove
open FENParser
open Evaluation
open Types
open GenerateMoves

let engine = "Fishy"
let version = "0.1"

let player = true
let go = false
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
    Console.WriteLine text
    logWriter.makeLogEntry "Outgoing " text
    ()

let convertNumbersToCoordinates (move: Move) =
    let files = " abcdefgh"
    let ranks = " 12345678"
    let fileChar1 = files[move.fromFile]
    let rankChar1 = ranks[move.fromRank]
    let fileChar2 = files[move.toFile]
    let rankChar2 = ranks[move.toRank]
    $"%c{fileChar1}%c{rankChar1}%c{fileChar2}%c{rankChar2}"

let startGame (cmd: string) =

    let cmdList = cmd.Split [|' '|]
    if cmdList[1] = "startpos" then
        parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" |> ignore

        output $"command {cmd} received"
        if cmdList.Length > 2 && cmdList[2] = "moves" then
            let moves = Array.skip 3 cmdList

            let moveList = Array.iter (fun x -> parseAndMakeMove currentBoard gameState x |> ignore) moves
            let y = moveList
            ()
        else
            ()
    else
        ()
    ()

let rec processCommand () =

    if go then ()

    let cmd = Console.ReadLine ()
    logWriter.makeLogEntry "Incoming " cmd

    match cmd with
    | cmd when cmd[0..1] = "go" ->
        let moves = generateMoves currentBoard gameState
        let moves2 = moves |> List.head
        let (finalMove: string) = moves2 |> convertNumbersToCoordinates
        let eval = evaluate currentBoard gameState
        output $"bestmove {finalMove}"
    | cmd when cmd[0..9] = "ucinewgame" -> ()
    | cmd when cmd[0..7] = "position" -> startGame cmd
    | cmd when cmd[0..8] = "startpos " -> ()
    | cmd when cmd[0..8] = "setoption" -> ()
    | "test" -> () // For debugging exact positions
    | "wac" -> ()

    | "savefen" -> ()
    | cmd when cmd[0..2] = "uci" ->
        output ("id name " + engine)
        output "id author Hugh Cumper"
        output "option:"
        output "uciok"
        initializePlacementValues ()
    | cmd when cmd[0..6] = "isready" -> output "readyok"
    | cmd when cmd[0..3] = "quit" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | cmd when cmd[0..3] = "stop" ->
            logWriter.makeLogEntry "Outgoing " "quitting"
    | _ -> output ("Unrecognized uci command " + cmd)

    if cmd[0..3] <> "quit" && cmd[0..3] <> "stop" then
        processCommand ()

[<EntryPoint>]
processCommand ()

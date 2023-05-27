module Fish.UCI

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

// must retain state of the game as UCI is stateful
let mutable sessionBoard = Array2D.createBased -1 -1 12 12 offBoard
let mutable sessionState =
    { WhiteCanCastleKingside = true
      WhiteCanCastleQueenside = true
      BlackCanCastleKingside = true
      BlackCanCastleQueenside = true
      ToPlay = White
      EPSquare = None
      HalfMoveClock = 0
      FullMoveNumber = 0
    }

let logWriter = UCILogger()

let output (text: string) =
    logWriter.makeLogEntry "Outgoing " text
    Console.WriteLine text
    Debug.WriteLine("Outgoing:  " + text)

let convertNumbersToCoordinates (move: Move) =
    let files = " abcdefgh"
    let ranks = " 12345678"
    let fileChar1 = files[move.fromFile]
    let rankChar1 = ranks[move.fromRank]
    let fileChar2 = files[move.toFile]
    let rankChar2 = ranks[move.toRank]
    $"%c{fileChar1}%c{rankChar1}%c{fileChar2}%c{rankChar2} "

let setupPosition (cmd: string) =
    let applyMoves move =
        sessionState <- snd (parseAndMakeMove sessionBoard sessionState move)

    let cmdList = cmd.Split [|' '|]
    let fen =
        if cmdList[1] = "startpos" then
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        else
            cmdList.[1]
    let parseResult = parseFEN fen
    sessionBoard <- fst parseResult
    sessionState <- snd parseResult

    if cmdList.Length > 2 && cmdList[2] = "moves" then
        let moves = Array.skip 3 cmdList
        Array.iter (applyMoves) moves

let go (cmd: string) =
    let cmdList = Array.toList (cmd.Split [|' '|])

    let level =
        match List.tryFindIndex (fun parm -> parm = "wtime") cmdList with
        | Some index -> (int cmdList.[index+1]) / 5000
        | None -> 3
    let valuation = chooseEngineMove sessionBoard 4 sessionState
    let pv = List.map (convertNumbersToCoordinates) (List.rev (snd valuation))
    let displayPV = String.concat "" pv
    let score = fst valuation / 10
    // output $"info score cp {score} pv {displayPV}"
    output $"bestmove {List.head pv}"

let rec processCommand () =
    let cmd = Console.ReadLine ()
    Debug.WriteLine("Incoming:  " + cmd)
    logWriter.makeLogEntry "Incoming " cmd

    match cmd with
    | cmd when cmd.StartsWith("debug") -> logWriter.makeLogEntry "Outgoing " "- debug command received"
    | cmd when cmd.StartsWith("go") -> go cmd
    | "isready" -> output "readyok"
    | "wac" -> ()
    | cmd when cmd.StartsWith("position") -> setupPosition cmd
    | cmd when cmd.StartsWith("quit") ->
        logWriter.makeLogEntry "Outgoing " "quitting"
    | cmd when cmd.StartsWith("register") -> ()
    | "savefen" -> ()
    | cmd when cmd.StartsWith("setoption") -> ()
    | cmd when cmd.StartsWith("startpos") -> ()
    | cmd when cmd.StartsWith("stop") ->
        logWriter.makeLogEntry "Outgoing " "quitting"
    | "test" -> ()
    | cmd when cmd.StartsWith("uci") ->
        output ("id name " + engine)
        output "id author Hugh Cumper"
        output "option:"
        output "uciok"
        initializePlacementValues () |> ignore
    | "ucinewgame" -> ()
    | _ -> output ("Unrecognized uci command " + cmd)

    if not (cmd.StartsWith("quit") || cmd.StartsWith("stop")) then
        processCommand ()

[<EntryPoint>]
let main _ =
    processCommand ()
    0

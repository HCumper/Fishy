module Fish.UCI

open System.Threading.Tasks
open TranspositionTable
open UCILogger
open Fishy
open MakeMove
open FENParser
open Evaluation
open Types
open LookAhead
open Zobrist

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
      HashKey = 0
    }

let mutable myColor = White
let mutable level = 0

let setupPosition (cmd: string) =
    let applyMoves move =
        sessionState <- snd (parseAndMakeMove sessionBoard sessionState move)

    let cmdList = cmd.Split [|' '|]
    let fen =
        if cmdList[1] = "startpos" then
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        else
            cmdList[1]
    let parseResult = parseFEN fen
    sessionBoard <- fst parseResult
    sessionState <- snd parseResult
    myColor <- White
    if cmdList.Length > 2 && cmdList[2] = "moves" then
        let moves = Array.skip 3 cmdList
        if moves.Length % 2 = 0 then myColor <- White else myColor <- Black
        Array.iter (applyMoves) moves
    writeOutput "position set up"

let go (cmd: string) =
    let cmdList = Array.toList (cmd.Split [|' '|])

    let time =
        match myColor with
        | White -> List.tryFindIndex (fun parm -> parm = "wtime") cmdList
        | _ -> List.tryFindIndex (fun parm -> parm = "btime") cmdList
    // let level =
    //     match time with
    //     | Some length -> min ((int (cmdList[(int length) + 1])) / 10000) 4
    //     | None -> 4
    level <- 4
    makeLogEntry $"level {level}"
//    let valuation = chooseEngineMove sessionBoard level sessionState
//    get value from transposition table
    // let pv = List.map convertNumbersToCoordinates (List.rev valuation)
    // writeOutput $"bestmove {List.head pv}"
    ()
let rec processCommand () =
    let mutable exit = false

    while not exit do
        let cmd = readInput ()

        match cmd with
        | cmd when cmd.StartsWith("run") ->
            initializePlacementValues () |> ignore
            setupPosition "startpos startpos"
            go ""
        | cmd when cmd.StartsWith("debug") -> ()
        | cmd when cmd.StartsWith("go") -> go cmd
        | cmd when cmd.StartsWith("isready") -> writeOutput "readyok"
        | cmd when cmd.StartsWith("wac") -> ()
        | cmd when cmd.StartsWith("position") -> setupPosition cmd
        | cmd when cmd.StartsWith("quit") -> makeLogEntry "quitting"
        | cmd when cmd.StartsWith("register") -> ()
        | cmd when cmd.StartsWith("savefen") -> ()
        | cmd when cmd.StartsWith("setoption") -> ()
        | cmd when cmd.StartsWith("startpos") -> ()
        | cmd when cmd.StartsWith("stop") ->
            writeOutput("Bestmove e2e4")
            makeLogEntry "Incoming: stop"
        | cmd when cmd.StartsWith("test") -> ()
        | cmd when cmd.StartsWith("uci") ->
            writeOutput ("id name " + engine)
            writeOutput "id author Hugh Cumper"
            writeOutput "option:"
            writeOutput "uciok"
            initializePlacementValues () |> ignore
        | cmd when cmd.StartsWith("ucinewgame") ->
    //        Transpositions.resetTranspositionTable
            ()
        | _ -> writeOutput ("Unrecognized uci command " + cmd)

        if cmd.StartsWith("quit") || cmd.StartsWith("stop") then
            exit <- true

let reportToUCI () =
    let rec fetchMovesFromTranspositionTable board state (movesSoFar: Move list) =
        let hash = hashAPosition board state
        match transpositionTableLookupByHash hash with
        | Some (score, confidence, move) ->
            match confidence with
            | 0 -> movesSoFar
            | _ ->
                let updatedBoard, updateState = makeMove board state move
                move :: fetchMovesFromTranspositionTable updatedBoard updateState movesSoFar
        | None -> movesSoFar

    let topLevelScore board state =
        let hash = hashAPosition board state
        match transpositionTableLookupByHash hash with
        | Some (score,_ , _) -> score
        | None -> failwith "no top level node"
    ()
    // try
    //     if repNodes > 0 then
    //         let pv = fetchMovesFromTranspositionTable sessionBoard sessionState []
    //     //    let score = topLevelScore sessionBoard sessionState
    //         let score = 8
    //         writePV score level repNodes (int repStopwatch.ElapsedMilliseconds) pv
    //         writeCurrmove repCurrMove repMoveNumber (repCacheHits / (repCacheMisses+1))
    // with
    // | ex -> ()

let rec oneSecondReporting () =
    async {
        reportToUCI ()
        do! Task.Delay(1000) |> Async.AwaitTask
        return! oneSecondReporting ()
    }

let setupOneSecondReporting () =
    async {
        do! oneSecondReporting ()
    }

let reporting () =
    Async.StartImmediate(setupOneSecondReporting ())

[<EntryPoint>]
let main _ =
    reporting ()
//    initializeLogging ()
    processCommand ()
    0

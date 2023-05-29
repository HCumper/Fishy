module UCILogger

open System
open System.IO
open Types

let outputFile = @".\ucilog.txt"
#if DEBUG
let writer = File.AppendText outputFile
#endif

let initializeLogging () =
#if DEBUG
    writer.AutoFlush <- true
    writer.WriteLine ""
    writer.WriteLine ""
    writer.WriteLine ""
    writer.WriteLine ""
    writer.WriteLine $"New Fishy session started {DateTime.Now.ToString()}"
    writer.WriteLine ""
#endif
    ()

let convertNumbersToCoordinates (move: Move) =
    let files = " abcdefgh"
    let ranks = " 12345678"
    let fileChar1 = files[move.fromFile]
    let rankChar1 = ranks[move.fromRank]
    let fileChar2 = files[move.toFile]
    let rankChar2 = ranks[move.toRank]
    $"%c{fileChar1}%c{rankChar1}%c{fileChar2}%c{rankChar2} "

let writePV evaluation depth nodes time pv =
    let pvString = List.fold (fun acc item -> acc + convertNumbersToCoordinates item) "" pv
    let eval = if evaluation < -1000000 then 0 else evaluation
    let cmdString = $"info depth {depth.ToString()} score cp {eval.ToString()}  time {time.ToString()} nodes {nodes.ToString()} nps {(nodes * 1000 / time).ToString()} pv {pvString}"
    Console.WriteLine cmdString
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay.ToString()} Outgoing:     {cmdString}"
#endif
    ()

let writeCurrmove currmove currmoveNumber hashfull =
    let currMoveString = convertNumbersToCoordinates currmove
    let cmdString = $"info currmove {currMoveString} hashfull {hashfull} currmovenumber {currmoveNumber}"
    Console.WriteLine cmdString
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay.ToString()} Outgoing:     {cmdString}"
#endif
    ()


let writeOutput (cmd: string) =
    Console.WriteLine cmd
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay} Outgoing:    {cmd}"
#endif
    ()

let makeLogEntry (cmd: string) =
#if DEBUG
    writer.WriteLine ("Debug only:    " + cmd)
#endif
    ()

let readInput =
    let cmd = Console.ReadLine ()
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay.ToString()} Incoming:    {cmd}"
#endif
    cmd

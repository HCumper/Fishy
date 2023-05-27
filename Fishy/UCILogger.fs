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

let writeInfo evaluation depth nodes time currmove pv =
    let currMoveString = convertNumbersToCoordinates currmove
    let pvString = List.fold (fun acc item -> acc + convertNumbersToCoordinates item) "" pv
    let eval = if evaluation < -1000000 then 0 else evaluation
    let cmdString = $"info score cp {eval.ToString()} depth {depth.ToString()} nodes {nodes.ToString()} time {time.ToString()} currmove {currMoveString} pv {pvString}"
    Console.WriteLine cmdString
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay.ToString()} Outgoing:     {cmdString}"
#endif
    ()

let writeOutput (cmd: string) =
    Console.WriteLine cmd
#if DEBUG
    writer.WriteLine ($"{DateTime.Now} Outgoing:    {cmd}")
#endif
    ()

let makeLogEntry (cmd: string) =
#if DEBUG
    writer.WriteLine ("Debug only:    " + cmd)
#endif
    ()

let readInput (cmd: string) =
    let cmd = Console.ReadLine ()
#if DEBUG
    writer.WriteLine $"{DateTime.Now.TimeOfDay.ToString()} Incoming:    {cmd}"
#endif
    cmd

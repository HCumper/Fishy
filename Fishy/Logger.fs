module Logger

open System
open System.IO
open System.Diagnostics
let outputFile = @".\ucilog.txt"
let uciLogging = true

type UCILogger () =
    let logWriter =
//       Debug.Listeners.Add(new TextWriterTraceListener(Console.Out));
//       Debug.AutoFlush = true;
       Debug.Indent();
       Debug.WriteLine("Entering Main");
       Console.WriteLine("Hello World.");
       Debug.WriteLine("Exiting Main");
       Debug.Unindent();
        match uciLogging with
        | true ->
            File.Delete outputFile
            let writer = File.AppendText outputFile
            do writer.WriteLine DateTime.Now
            do writer.WriteLine ""
            do writer.WriteLine ""
            writer.Flush ()
            Some writer
        | false -> None

    member _.makeLogEntry direction message =
        if uciLogging then
            logWriter.Value.WriteLine (direction.ToString () + ":  " + message)
            logWriter.Value.Flush ()

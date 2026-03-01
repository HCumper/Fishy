// ===========================
// Program entrypoint
// ===========================

module Program

open UCILogger.Uci
open UCIIntegration
open System
open System.Diagnostics
open System.Threading

[<EntryPoint>]
let main _ =
    // Optional: set environment variable WAIT_FOR_DEBUGGER=1 before launching from Fritz.
    // Then attach Rider to the running process.
    waitForDebuggerIfRequested()
    while not Debugger.IsAttached do
            Thread.Sleep(100)
    let api = createApi()
    run api
    0
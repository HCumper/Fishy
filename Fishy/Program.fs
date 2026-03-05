// ===========================
// Program entrypoint
// ===========================

module Program

open Uci
open UCIIntegration

[<EntryPoint>]
let main _ =
    // Optional: set environment variable WAIT_FOR_DEBUGGER=1 before launching from Fritz.
    // Then attach Rider to the running process.
    //waitForDebuggerIfRequested()
    // while not Debugger.IsAttached do
    //         Thread.Sleep(100)
//    UCIIntegration.tt
    let api = createApi()
    run api
    0
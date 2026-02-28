module Fishy
open UCIIntegration

[<EntryPoint>]
let main _ =
    waitForDebuggerIfRequested()
    let api = UCIIntegration.createApi()   // returns EngineApi record
    UCI.run api 
    0
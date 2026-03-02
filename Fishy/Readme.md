What actually determines strength/speed

For practical chess engines, the “best” search stack typically includes:

Alpha–beta pruning (expressed as negamax)

Iterative deepening

Transposition table (PV/exact/lower/upper)

Good move ordering

TT move first

captures (MVV-LVA), promotions

killer moves, history heuristic

Quiescence search (captures, often checks too)

Extensions and reductions

check extension, recapture extension (careful)

late move reductions (LMR)

Null-move pruning (careful around zugzwang/endgames)

Aspiration windows




Steps to make this work in Fritz 20 + Rider

Make sure it builds as a console EXE

Rider project settings: Output type = Exe

Target net8.0 (or net6.0)

Build

Publish/copy output to a stable folder
Example:

C:\Chess\Engines\MyFSharpEngine\
Copy:

MyFSharpEngine.exe

MyFSharpEngine.runtimeconfig.json

MyFSharpEngine.deps.json

required DLLs

(Using dotnet publish -c Debug -r win-x64 is fine.)

Add to Fritz 20 as a UCI engine

Engines / Create UCI Engine / Browse to EXE

Name it “MyFSharpEngine (Debug)”

Debug attach

Set system environment variable (Windows):

WAIT_FOR_DEBUGGER=1

Start Fritz and select your engine for analysis.

Engine will start and wait.

Rider: Run → Attach to Process… → select MyFSharpEngine.exe and attach.

Clear WAIT_FOR_DEBUGGER when you no longer need startup waiting.



Recommended Implementation Order
Phase 1 (Critical - Do First):

✅ Add transposition table
✅ Add basic move ordering (MVV-LVA)
✅ Add quiescence search

Phase 2 (Important - Do Soon):

Add iterative deepening
Add time management
Add UCI info output
Add check extensions

Phase 3 (Optimization - Do Later):

Add null move pruning
Add late move reductions
Add aspiration windows
Add killer moves
Add history heuristic

Parallelism?

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

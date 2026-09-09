# Bin Packing — Greedy Heuristics + Exact Branch & Bound Solver

Solving the Bin Packing problem in OCaml, both approximately (fast greedy heuristics) and exactly (Branch & Bound with pruning). Written during my CPGE MPI coursework (Lycée Montaigne, 2024–2025).

## The problem

Bin Packing: given `n` items of sizes `x0, ..., xn-1` and identical bins of capacity `C`, distribute the items into as few bins as possible without exceeding `C` in any bin.

It's a classic NP-complete problem. A direct real-world instance is memory allocation: bins are contiguous free memory blocks, items are allocation requests — which is also why an *exact* solver only makes sense on small instances, and why approximate heuristics matter in practice.

## What's implemented

**Greedy heuristics** (fast, approximate):
| Algorithm | Idea | Complexity |
|---|---|---|
| Next-Fit | always try the last opened bin | O(n) |
| First-Fit | try every previously opened bin, in order | O(n²) |
| First-Fit-Decreasing | sort items largest-first, then First-Fit | O(n²) |
| Best-Fit-Decreasing | sort items largest-first, keep bins sorted by remaining space | O(n log n) |

**Exact solver** — Branch & Bound:
- Explores a binary decision tree (include the current item in the current bin, or defer it) rather than brute-forcing every partition.
- Prunes a branch as soon as its lower bound (remaining volume ÷ bin capacity) exceeds the current best upper bound, found beforehand with Best-Fit-Decreasing.
- Guarantees an optimal solution, at the cost of worst-case exponential time — expected for an NP-complete problem, and the reason the greedy heuristics exist alongside it.

## Known limitation

On larger or adversarial instances (tested with 23 items, capacity 101) the search tree grows faster than pruning can cut it down, and the exact solver becomes impractically slow. This doesn't affect the heuristics, which stay fast regardless of instance size — it's the expected trade-off for guaranteeing optimality on an NP-complete problem, not a bug.

## Run it

Requires OCaml (tested with 4.14).

```bash
ocaml bin_packing.ml
```

This runs every heuristic and the exact solver on the same example instance and prints the resulting bins.

Example output:
```
Instance: capacity = 10, items = [2; 5; 4; 7; 1; 3; 8]

Next-Fit (5 boxes):
  Box 1: 5, 2
  Box 2: 4
  Box 3: 1, 7
  Box 4: 3
  Box 5: 8

Best-Fit-Decreasing (3 boxes):
  Box 1: 1, 4, 5
  Box 2: 2, 8
  Box 3: 3, 7

Exact solution via Branch & Bound (3 boxes):
  Box 1: 8, 2
  Box 2: 1, 4, 5
  Box 3: 3, 7
```

On this instance the heuristics already reach the optimum — the gap between Next-Fit and the rest shows why bin ordering matters more than the exact-vs-approximate choice on easy inputs.

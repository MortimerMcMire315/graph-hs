graph-hs
========
A small library for dealing with graph theory problems in Haskell. A couple of algorithms are included.

Installation
------------
1. Download the [Haskell Platform](https://www.haskell.org/) or install it using a package manager.
2. Install [cabal](https://www.haskell.org/cabal/), the Haskell package manager and build system.
3. run `cabal build` to build the GraphTheory library.

4. `cabal build graphtest` to build Main.hs.
5. `cabal run graphtest` to run Main.hs... At this point, it will just run a test of the delta-plus-one edge-coloring algorithm.

The Other Thing Of Interest
---------------------------
is [the PDF explaining my functional take on this edge-coloring algorithm](https://github.com/MortimerMcMire315/graph-hs/blob/master/tex/EdgeColor.pdf) compiled from [the literate Haskell file EdgeColor.lhs](https://github.com/MortimerMcMire315/graph-hs/blob/master/src/GraphTheory/Algorithm/EdgeColor.lhs).

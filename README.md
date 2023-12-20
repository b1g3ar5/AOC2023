# Advent of Code 2023

Advent of Code 2023 solutions in Haskell

Day12 takes 1.2s - it's a memoisation
Day14 takes 0.7s - replaced the nap woth a trie, used bytestrings instead of strings...
Day16 takes 1.5s - a lot of routes through the grid and I can't think of a further easy optimisation other than parallelisation...
Day17 takes 6.5s - Dijkstra on 140*140 grid - I tried library dijkstra and aStar and both were slower
                   lots of time in extractBin from the PQueue library - 30 million calls


Total 10s up to Day19 (real time)

Day 19 Hylomorphism!!!


### Installation

This is a stack project, so you can probably compile and run using:

    stack build

and:

    stack run -- AOC2023-exe


I would recommend using ghcup to install the required verions of stack, ghc, cabal, etc. I call it using:

    ghcup tui



### Profiling

stack build --profile

stack exec --profile -- AOC2023-exe +RTS -p


### Current output

Day1: part1: 56042
Day1: part2: 55358
Day2: part1: 2377
Day2: part2: 71220
Day3: part1: 533784
Day3: part2: 78826761
Day4: part1: 26218
Day4: part2: 9997537
Day5: part1: 662197086
Day5: part2: 52510809
Day6: part1: 771628
Day6: part2: 27363861
Day7: part1: 256448566
Day7: part2: 254412181
Day8: part1: 17873
Day8: part2: 15746133679061
Day9: part1: 1715351481
Day9: part2: 1043
Day10: part1: 6613
Day10: part2: 511
Day11: part1: 10313550
Day11: part2: 611998089572
Day12: part1: 7402
Day12: part2: 3384337640277
Day13: part2: 36015
Day13: part2: 35335
Day14: part1: 108918
Day14: part2: 100310
Day15: part1: 503487
Day15: part2: 261505
Day16: part1: 7242
Day16: part2: 7572
Day17: part2: 1039
Day17: part2: 1201
Day18: part2: 39194
Day18: part2: 78242031808225
Day19: part1: 397134
Day19: part2: 127517902575337

real    0m10.393s
user    0m17.900s
sys     0m0.720s
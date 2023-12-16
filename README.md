# Advent of Code 2023

Advent of Code 2023 solutions in Haskell

Day12 takes 1.3s - it's a memoisation
Day14 takes 2.3s - but the code is pretty neat. All the time is in splitOn() from the Data.List.Split library
Day16 takes 3.4s - a lot of routes through the grid and I can't think of a further easy optimisation other than parallelisation...

Total 8s as of Day 16


I tried parallelisation on Day16 but the time increased with 

    parMap rpar (run g) setOff

But it seems to run more quickly - I don't think timeIt works with rpar?

More work on paralleisation required...



### Installation

This is a stack project, so you can probably compile and run using:

    stack build

and:

    stack run -- AOC2023-exe


However, I would recommend usign ghcup. I call it using:

    ghcup tui

and then install all the required versions...


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
Day12: part2:count 1
Day12: part2:count 0
Day12: part2:count 0
Day12: part2:count' 0
Day12: part2:count' 0
Day12: part2:count' 0
Day13: part2: 36015
Day13: part2: 35335
Day14: part1: 108918
Day14: part2: 100310
Day15: part1: 503487
Day15: part2: 261505
Day16: part1: 7242
Day16: part2: 7572
CPU time:   8.93s
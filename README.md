# Advent of Code 2023

Advent of Code 2023 solutions in Haskell

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
    CPU time:   0.26s
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


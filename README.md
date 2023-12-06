# AOC2022

Haskell solutions to the Advent of Code 2022

Slow solutions are:

  - Day19 part2 = 21s
      This is the puzzle about buying mining robots geode, Obsidian etc.
      My solution just uses a hylomorphism to do the bfs with a few speed-ups:
        
        1. If you can afford a Geode robot buy it
        
        2. If you didn't buy an ore or clay robot the previous turn when you could 
           afford it then you can't buy it the next go
        
        3. Robots of each material limitied to the maximum of that material you
           can spend in one turn
        
        4. Stock of each material limited to the maximum you can spend in the remaining time

        It would be much quicker to use a bfs because the tree used in the hylomorphism
        has a lot of duplication..
        
  - Day20 part2 = 2s
  - Day23 part2 = 5s
      My solution did take 2 minutes - but I incorporated the moving into the proposing
      and it came down to 5s

Day21 is pretty neat - it's the first time that I would have scored and I would have
beaten J Paulson. 

I had a lot of time that day so I reimplemented both solutions using hylomorphisms.
The first algebra does the usual operations, the second algebra does the inverse 
operations treating "humn" as a variable and then solves the resulting
problem in the "root" evaluation - also very neat.


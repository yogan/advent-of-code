# Solutions for [Everybody Codes](https://everybody.codes)

## [2025: The Song of Ducks and Dragons](https://everybody.codes/event/2025/quests)

### [Quest 1: Whispers in the Shell](https://everybody.codes/event/2025/quests/1)

- [Python](2025/quest-01-python/ec.py)

### [Quest 2: From Complex to Clarity](https://everybody.codes/event/2025/quests/2)

- [Python](2025/quest-02-python/ec.py) (including [visualizations](2025/quest-02-python/README.md))

### [Quest 3: The Deepest Fit](https://everybody.codes/event/2025/quests/3)

- [Python](2025/quest-03-python/ec.py)

### [Quest 4: Teeth of the Wind](https://everybody.codes/event/2025/quests/4)

- [Python](2025/quest-04-python/ec.py)

### [Quest 5: Fishbone Order](https://everybody.codes/event/2025/quests/5)

- [Python](2025/quest-05-python/ec.py)
- first somewhat tricky problem, at least ordering for part 3 was non-trivial
- nice that Python compares arrays like this: `[1, 3, 2] > [1, 2, 3]`

### [Quest 6: Mentorship Matrix](https://everybody.codes/event/2025/quests/6)

- [Python](2025/quest-06-python/ec.py)
- initial version was brute-force for part 3, which took ~ 1 min (iterating
  over a `1_000 * 10_000` long array is no fun)
- final version considers the repeating middle parts and is fast
- tricky that the splitting up works for the real long input, but not for the
  sample

### [Quest 7: Namegraph](https://everybody.codes/event/2025/quests/7)

- [Python](2025/quest-07-python/ec.py)
- recursive solution for part 3, fast enough without any caching (~ 2 sec)
- all functions beside parsing and the recursive `combinations()` for part 3
  could eventually be rewritten to one-liners (using list comprehensions,
  `sum()`, `next()`, `all()`, etc.)

### [Quest 8: The Art of Connection](https://everybody.codes/event/2025/quests/8)

- [Python](2025/quest-08-python/ec.py)
- nice geometric problem, easy once you found the intersection condition
- ended up with nice concise solutions
- run time for part 3 is mediocre (~ 2 sec), not sure how to optimize though

### [Quest 9: Encoded in the Scales](https://everybody.codes/event/2025/quests/9)

- [Python](2025/quest-09-python/ec.py)
- yay, genetics!
- initial brute-force solution for part 3 was very slow (~ 5 min), as it was
  iterating over lists of letters for the child/parents check
- eventually ended up with bitmaps and bit operations
- also remembering and skipping sequences for which parents are already found
- those two optimizations speed up things immensely (down to ~ 4 sec)
- solution is technically still _O(n³)_, though

### [Quest 10: Feast on the Board](https://everybody.codes/event/2025/quests/10)

- [Python](2025/quest-10-python/ec.py)
- this one was brutal, at least part 3
- it was obvious that caching/DP was required, as you have to traverse a vast
  state space with many paths leading to the same state
- I still really struggled to implement it
- final solution runs in about 15 sec, which is ok

### [Quest 11: The Scout Duck Protocol](https://everybody.codes/event/2025/quests/11)

- [Python](2025/quest-11-python/ec.py)
- fun one; for part 3, you have to make an observation about the number of
  required moves (spoiler / explanation as a comment in the source file)

### [Quest 12: One Spark to Burn Them All](https://everybody.codes/event/2025/quests/12)

- [Python](2025/quest-12-python/ec.py)
- flood fill and some greedy brute force for part 3

### [Quest 13: Unlocking the Mountain](https://everybody.codes/event/2025/quests/13)

- [Python](2025/quest-13-python/ec.py)
- fun problem, not too hard (requires modulo arithmetic and ranges)
- could have solved it somewhat fast, but of course I overlooked that the ranges
  that are added counterclockwise are also reversed (`[5-19]` becomes `[19-5]`)
- sample did not have this issue, because you find the solution in a non-reverse
  range

### [Quest 14: The Game of Light](https://everybody.codes/event/2025/quests/14)

- [Python](2025/quest-14-python/ec.py)
- cellular automaton simulation
- part 3 required a loop detection, which I actually got right on the first try
- no big optimizations besides the loop, run time is ~ 1.5 sec, so good enough

### [Quest 15: Definitely Not a Maze](https://everybody.codes/event/2025/quests/15)

- [Python](2025/quest-15-python/ec.py)
- another tough weekend problem (like quest 10)
- parts 1 and 2 could be easily solved with flood fill (BFS) on a grid
- part 3 had huge distances, so it's impossible to go in single steps
- final solutions is a combination of coordinate compression and Dijkstra
- had to add some visualization functions to understand what I had to do
  (run with `--create-svgs` to create the images)
- run time < 1 sec.

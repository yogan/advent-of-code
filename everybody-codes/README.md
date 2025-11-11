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
- initial version was brute-force for part 3, which took ~ 1 min. (iterating
  over a `1_000 * 10_000` long array is no fun)
- final version considers the repeating middle parts and is fast
- tricky that the splitting up works for the real long input, but not for the
  sample

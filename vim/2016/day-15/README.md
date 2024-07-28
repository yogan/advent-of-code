# Advent of Code 2016 Day 15

The puzzle can be solved with the [Chinese Remainder
Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem).

Initially I solved this by hand by putting together the equations seen below and
letting them be solved by an online calculator. Eventually I decided that code
is always better, so I wrote the whole thing in
[VimScript](./aoc-2016-15.commented.vim). The algorithm was shamelessly ported
from the [Rosetta Code C# CRT
implementation](https://rosettacode.org/wiki/Chinese_remainder_theorem#C#).

## Part 1

- Disc 1: `t + 10 + 1 = 0 mod 13` ⇔ `t = -11 mod 13` ⇔ `t =  2 mod 13`
- Disc 2: `t + 15 + 2 = 0 mod 17` ⇔ `t = -17 mod 17` ⇔ `t =  0 mod 17`
- Disc 3: `t + 17 + 3 = 0 mod 19` ⇔ `t = -20 mod 19` ⇔ `t = 18 mod 19`
- Disc 4: `t +  1 + 4 = 0 mod  7` ⇔ `t =  -5 mod  7` ⇔ `t =  2 mod  7`
- Disc 5: `t +  0 + 5 = 0 mod  5` ⇔ `t =  -5 mod  5` ⇔ `t =  0 mod  5`
- Disc 6: `t +  1 + 6 = 0 mod  3` ⇔ `t =  -7 mod  3` ⇔ `t =  2 mod  3`

Using [an online
calculator](https://www.omnicalculator.com/math/chinese-remainder) gives us
**203660** as the answer to part 1:

- `t ≡ 203660 mod 440895`

## Part 2

- Discs 1–6: `t = 203660 mod 440895`
- New disc 7: `t = -7 mod 11` ⇔ `t = 4 mod 11`

Which gives us **2408135** as the answer to part 2:

- `t ≡ 2408135 mod 4849845`

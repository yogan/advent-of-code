# Advent of Code in Vim

## Usage

The `*.vim` files can either be passed to Vim with `-s <script>`, or loaded at
runtime with `:so[urce] <script>`. The scripts expect an empty buffer and input
data in a file named `input.txt` in the current working directory.

## Patterns

Some not-so-common Vim patterns that can be helpful:

- reading input file into buffer without keeping an empty blank line at the top:
    ```vim
    " | combines commands, 1d deletes first line
    :r input.txt|1d
    ```

- evaluating expressions (see `:help :s\=`, `:help eval()`):
    ```vim
    " replaces a line with e.g. 1+2+3 with 6
    :s/.*/\=eval(submatch(0))
    ```

- getting length of current line (see `:help col()`):
    ```vim
    " $ is virtual char behind the last one, so - 1
    :let l = col("$") - 1

    " replace the line with its length
    :s/.*/\=col("$") - 1
    ```

- getting number of lines in buffer (see `:help line()`):
    ```vim
    " fun fact, $ is the last line number here
    :let n = line("$")
    ```

- search and replace, but get rid of message (see `:help :silent`):
    ```vim
    " ! to also suppress errors (e.g. "no matches")
    :sil! %s/foo/bar/g
    ```

- running normal commands from a script (see `:help :normal`):
    ```vim
    " select paragraph and join lines
    :norm vipgJ
    ```

- using `:exe` for dynamic commands and special keys (see `:help :exe`):
    ```vim
    " run macro q for all lines
    :exe "norm " . line("$") . "@q"

    " enter and exit insert mode with <Esc>
    :exe "norm ccWAT\<esc>"
    ```

- building complicated macros that are still kind of readable (but watch out for
  escaping hell):
    ```vim
    " imaging buffer with lines like this: 1x69x420
    " replace x with * and evaluate the line
    :let m1 = ':s/x/*/g'
    :let m2 = ':s/.*/\=eval(submatch(0))/'
    " no enter, no fun ("" instead of '' is important)
    :let cr = "\<cr>"
    :let @q =  m1 . cr . m2 . cr
    " run this sucker til the end
    :norm 999@q
    ```

## Vimscript

When things get tough, there is no way around Vimscript. A good guide is [Learn
Vimscript the Hard Way](https://learnvimscriptthehardway.stevelosh.com/). Some
helpful things:

- "debugging" (see `:help echo`, `:help echom`, `:help message-history`):
    ```vim
    :echo MyFunction()
    " also put into message history (:mes)
    :echom MyFunction()
    ```

- putting the result of a function to the buffer (see `:help :put`):
    ```vim
    " below current line:
    :pu =MyFunction()
    " above current line:
    :pu! =MyFunction()
    ```

- iterating over all lines in the buffer:
    ```vim
    :function TotalChars()
        let chars = 0
        for line in getline(1, '$')
            let chars += strlen(line)
        endfor
        return chars
    endfunction
    ```

## Solutions

### 2015

- [2015 Day 03](https://adventofcode.com/2015/day/3) ⭐⭐
    - [`aoc.vim`](2015/day-03/aoc.vim)
- [2015 Day 05](https://adventofcode.com/2015/day/5) ⭐⭐
    - [`aoc.vim`](2015/day-05/aoc.vim)
- [2015 Day 06](https://adventofcode.com/2015/day/6) ⭐⭐
    - [`aoc.vim`](2015/day-06/aoc.vim)
- [2015 Day 08](https://adventofcode.com/2015/day/8) ⭐⭐
    - [`aoc.vim`](2015/day-08/aoc.vim)

### 2016

- [2016 Day 01](https://adventofcode.com/2016/day/1) ⭐⭐
    - [`aoc.vim`](2016/day-01/aoc.vim)
- [2016 Day 02](https://adventofcode.com/2016/day/2) ⭐⭐
    - [`aoc.vim`](2016/day-02/aoc.vim)
- [2016 Day 03](https://adventofcode.com/2016/day/3) ⭐⭐
    - [`aoc.vim`](2016/day-03/aoc.vim)
    - solved together with the wonderful people at
      [JSCraftCamp 2024](https://jscraftcamp.org)
- [2016 Day 04](https://adventofcode.com/2016/day/4) ⭐⭐
    - [`aoc.vim`](2016/day-04/aoc.vim)
- [2016 Day 06](https://adventofcode.com/2016/day/6) ⭐⭐
    - [`aoc.vim`](2016/day-06/aoc.vim)
- [2016 Day 08](https://adventofcode.com/2016/day/8) ⭐⭐
    - [`aoc.vim`](2016/day-08/aoc.vim)
    - [animated solution](2016/day-08/README.md)
- [2016 Day 15](https://adventofcode.com/2016/day/15) ⭐⭐
    - [`aoc.vim`](2016/day-15/aoc.vim)
    - [Chinese Remainder
      Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem),
      see [Day 15 README](2016/day-15/README.md)

### 2017

- [2017 Day 02](https://adventofcode.com/2017/day/2) ⭐⭐
    - [`aoc.vim`](2017/day-02/aoc.vim)
    - solved together with wonderful people at
      [SoCraTes 2024](https://www.socrates-conference.de)

### 2022

- [2022 Day 01](https://adventofcode.com/2022/day/1) ⭐⭐
    - [`aoc.vim`](2022/day-01/aoc.vim)
- [2022 Day 02](https://adventofcode.com/2022/day/2) ⭐⭐
    - [`aoc.vim`](2022/day-02/aoc.vim)
- [2022 Day 03](https://adventofcode.com/2022/day/3) ⭐⭐
    - [`aoc.vim`](2022/day-03/aoc.vim)

### 2023

- [2023 Day 01](https://adventofcode.com/2023/day/1) ⭐⭐
    - [`aoc.vim`](2023/day-01/aoc.vim)
- [2023 Day 02](https://adventofcode.com/2023/day/2) ⭐⭐
    - [`aoc.vim`](2023/day-02/aoc.vim)
- [2023 Day 03](https://adventofcode.com/2023/day/3) ⭐⭐
    - [`aoc.vim`](2023/day-03/aoc.vim)
- [2023 Day 04](https://adventofcode.com/2023/day/4) ⭐⭐
    - [`aoc.vim`](2023/day-04/aoc.vim)

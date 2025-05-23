# Advent of Code in Vim

## Usage

The scripts expect an empty buffer and input data in a file named `input.txt` in
the current working directory.

For interactive use, start Vim with `vim --clean` and load a script with
`:so[urce] aoc.vim` (ideally comment out the `:x! out` at the end).

To run a script non-interactively, use `./run.sh` which adds a bit of magic to
keep Vim from messing up the terminal. Results are printed to stdout. As long as
the script isn't super slow (and has the `:x! out` line active), you can even
get a watch mode with `fd | entr -cc ./run.sh`.

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
    " go down as many lines as the number in the current line says:
    :exe "norm " . getline(".") . "j"

    " enter and exit insert mode with <Esc>
    " (cw = change word, which ends up in insert mode)
    :exe "norm cwNEW!\<esc>"
    ```

- building complicated macros that are still kind of readable (but watch out for
  escaping hell, see `:help expr-quote`, and mode changes, see
  `:help vim-modes`)
    ```vim
    " Imagine our buffer has lines like 1x69x420 that describe boxes.

    " Replace x with * and evaluate the line:
    :let s1 = ':s/x/*/g'
    :let s2 = ':s/.*/\=eval(submatch(0))'
    " Append a + at the end of the line, use <esc> to leave insert mode:
    :let append_plus = "A+\<esc>"
    " Move down to next line:
    :let move_down = "j"
    " The enter key (only works with double quotes) to finish the :s commands:
    :let cr = "\<cr>"

    " When the macro gets executed, the string in the register is executed as
    " if it was typed character by character in normal mode.
    " The substitutions in s1 and s2 are entering command line mode with ":",
    " and we need to "press" the enter key to execute them and go back to
    " normal mode.
    " The characters of append_plus and move_down are just regular normal mode
    " commands, so we let the macro "type" them as they are, no enter key
    " needed.
    :let @q =  s1 . cr . s2 . cr . append_plus . move_down
    
    " Use :reg q to check the final result of the macro.
    " Note that special keys like <cr> and <esc> are shown as ^M and ^[.

    " Now is a good time to manually test the macro with @q, 10@q, etc.
    ```

- running a macro on all lines:
    ```vim
    " This seems to be the most sane way to do it:
    :%norm @q

    " Another option is to get the number of lines and use that:
    :exe "norm " . line("$") . "@q"

    " Or we just go nuts and do it like a psychopath:
    :norm 9999@q
    " The reason why this does not completely explode is that the macro stops
    " when it reaches the end of the buffer. This happens because @ executes a
    " register like a mapping, and mappings stop when they encounter an error.
    " See https://stackoverflow.com/a/77800926/183582 for where this is hidden
    " deep in the Vim docs.
    " I highly recommend not doing this, and just document it here because many
    " of the existing solutions are using that, because I didn't know better at
    " the time.
    ```

## Vimscript

When things get tough, there is no way around Vimscript. A good guide is [Learn
Vimscript the Hard Way](https://learnvimscriptthehardway.stevelosh.com/). For an
overview of Vim's built-in functions grouped by topic, see `:help
function-list`.

- "debugging" (see `:help echo`, `:help echom`, `:help message-history`):
    ```vim
    " just show stuff on the command line:
    :echo MyFunction()
    " also put into message history (:mes):
    :echom MyFunction()
    ```

- putting the result of a function to the buffer (see `:help :put`):
    ```vim
    " below current line:
    :pu =MyFunction()
    " above current line:
    :pu! =MyFunction()
    ```

- arguments need to be accessed with `a:argname` (see `:help a:`):
    ```vim
    :function MyFunction(foo, bar)
        return a:foo + a:bar
    endfunction
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

- functions and `call()` (see `:help call()`, `:help functions`):
    ```vim
    " On the right hand side of an assignment, or as parameters to commands,
    " both built-in and user-defined functions (starting with a capital letter)
    " can be called directly:
    :let line = getline(1)
    :echo col("$")
    :let p1 = Part1()
    :pu =Part2()
    :s/\(\d+\), \(\d+\)/\=eval(submatch(1) + submatch(2))
    :s/\(\d+\): \(\d+\)/\=DoSomeMath(submatch(1), submatch(2))

    " Otherwise, we need to use `call()`:
    :call setreg('"', join(things, "+"))
    :call cursor(lnum, col)
    :call MyFunction(a, b)
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
- [2017 Day 05](https://adventofcode.com/2017/day/5) ⭐⭐
    - [`aoc.vim`](2017/day-05/aoc.vim)
    - solved with my awesome colleagues at [inovex](https://www.inovex.de/)
      Sommerplenum 2024

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

### 2024

- [2024 Day 01](https://adventofcode.com/2024/day/1) ⭐⭐
    - [`aoc.vim`](2024/day-01/aoc.vim)

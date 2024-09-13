# Advent of Code in Vim

The script expects an empty buffer and input data in a file named `input.txt` in
the current working directory.

For interactive use, start Vim with `vim --clean` and load the script with
`:so[urce] aoc.vim` (ideally comment out the `:x! out` at the end).

To run the script non-interactively, use `./run.sh` which adds a bit of magic to
keep Vim from messing up the terminal. Results are printed to stdout. As long as
the script isn't super slow (and has the `:x! out` line active), you can even
get a watch mode with `fd | entr -cc ./run.sh`.

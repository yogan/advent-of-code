# Advent of Code in OCaml

## Scripts

```sh
./build-ci.sh                       # Build binaries
./run.sh [FILENAME]                 # Run with FILENAME (defaults to input.txt)
./test.sh                           # Run tests
```

## Watch Mode

```sh
fd | entr -cc ./run.sh [FILENAME]   # Build and run on file change
dune exec aoc <FILENAME>            # Alternative; do NOT run ./test-watch.sh or
                                    # dune runtest -w in parallel with this

./test-watch.sh                     # Requires that ./run.sh is in watch mode
dune runtest -w                     # Alternative; do NOT run ./run.sh in watch
                                    # mode in parallel with this
```

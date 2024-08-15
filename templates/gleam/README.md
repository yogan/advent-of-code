# Advent of Code in Gleam

## Scripts

```sh
./run.sh [FILENAME]         # Run with FILENAME (defaults to input.txt)
./test.sh                   # Run the tests
```

## Watch Mode

```sh
fd | entr -cc ./run.sh [FILENAME]
fd | entr -cc ./test.sh
```

## Gleam Commands

```sh
gleam run [FILENAME]        # Run the project
gleam test                  # Run the tests
gleam shell                 # Run an Erlang shell
gleam add PACKAGE           # Add a package
gleam remove PACKAGE        # Remove a package
```

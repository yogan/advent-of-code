# Advent of Code in Gleam

## Scripts

```sh
./run.sh                    # Run with input.txt
./sample.sh                 # Run with sample.txt
./test.sh                   # Run the tests
```

## Watch Mode

```sh
fd | entr -cc ./run.sh      # Run with input.txt on changes
fd | entr -cc ./sample.sh   # Run with sample.txt on changes
fd | entr -cc ./test.sh     # Run the tests on changes
```

## Gleam Commands

```sh
gleam run [FILENAME]        # Run the project
gleam test                  # Run the tests
gleam shell                 # Run an Erlang shell
gleam add PACKAGE           # Add a package
gleam remove PACKAGE        # Remove a package
```

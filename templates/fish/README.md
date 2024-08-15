# Advent of Code in fish

## Scripts

```sh
./run.fish [FILENAME]         # Run with FILENAME (defaults to input.txt)
./test.fish                   # Run the tests
```

## Watch Mode

```sh
fd | entr -cc ./run.fish [FILENAME]
fd | entr -cc ./test.sfih
```

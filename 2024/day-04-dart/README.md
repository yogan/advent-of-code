# Advent of Code in Dart

## Scripts

```sh
./run.sh [FILENAME]         # Run with FILENAME (defaults to input.txt)
./test.sh                   # Run the tests
./build.sh                  # Build the project
./test-ci.sh                # Run CI tests (using binary created by build.sh)
```

## Watch Mode

```sh
fd | entr -cc ./run.sh [FILENAME]
fd | entr -cc ./test.sh
```

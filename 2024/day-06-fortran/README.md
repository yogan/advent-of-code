# Advent of Code in Fortran

## Scripts

```sh
./run.sh        # Run (with sample and real input)
./test.sh       # Run the tests
./build-ci.sh   # Build the project
./test-ci.sh    # Run CI tests (using binary created by build-ci.sh)
```

## Watch Mode

```sh
fd | entr -cc -s './build-ci.sh && ./test.sh; echo; ./run.sh'
```

# Advent of Code in C++

## Prerequisites

C++ ecosystem is a mess. To get compilation and LSP support in NeoVim running,
those packages (on Ubuntu 22.04) are what eventually seemed to be working:

- clang-14
- clangd-14
- g++-12
- cmake (3.22.1)

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

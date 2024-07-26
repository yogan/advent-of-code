# Advent of Code in C++

## Prerequisites

C++ ecosystem is a mess. To get compilation and LSP support in NeoVim running,
those packages (on Ubuntu 22.04) are what eventually seemed to be working:

- clang-14
- clangd-14
- g++-12
- cmake (3.22.1)

## Usage

- prepare CMake build once with `./build-ci.sh`
- build and run with `./run.sh <puzzle-input>`
- watch mode: `fd | entr -cc ./run.sh <puzzle-input>`
- watch mode for tests: `fd | entr -cc ./test.sh`

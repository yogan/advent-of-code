# Advent of Code in Zig

Created with `zig init` and cleaned up a bit (lib removed, only main/exe).

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

## Recommended Dev Environment

- setting up Zig is extracting a tarball and adding it to your path
    - see how it's done in the `Dockerfile`
- NeoVim works great; with lsp-zero and Mason:
    - `:Mason install zls`
    - restart nvim
    - check with `:LspInfo`
    - you should get syntax highlighting, completion, and even format on save

## Zig Resources

- [Zig In-depth Overview](https://ziglang.org/learn/overview)
- [Zig Documentation](https://ziglang.org/documentation/master)
- [Using Zig for Advent of Code](https://www.huy.rocks/everyday/12-11-2022-zig-using-zig-for-advent-of-code)

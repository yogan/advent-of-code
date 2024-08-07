# Advent of Code in Zig

Created with `zig init` and cleaned up a bit (lib removed, only main/exe).

## Usage

- run: `./run.sh`
- run (watch mode): `fd | entr -cc ./run.sh`
- test: `./test.sh`
- test (watch mode): `fd | entr -cc ./test.sh`

## Recommended Dev Environment

- setting up Zig is extracting a tarball and adding it to your path
    - see how it's done in the Dockerfile
- NeoVim is great; with lsp-zero and Mason:
    - `:Mason install zls` you need to do (check with `:LspInfo`).
    - restart nvim
    - check with `:LspInfo`
    - you should get syntax highlighting, completion, and even format on save

## Zig Resources

- [Zig In-depth Overview](https://ziglang.org/learn/overview)
- [Zig Documentation](https://ziglang.org/documentation/master)
- [Using Zig for Advent of Code](https://www.huy.rocks/everyday/12-11-2022-zig-using-zig-for-advent-of-code)

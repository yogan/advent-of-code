# Advent of Code in Elixir

## Usage

```sh
./run.sh [INPUTFILE]
./test.sh
```

Automatically run on changes (watch mode):

```sh
fd | entr -cc ./run.sh [INPUTFILE]
fd | entr -cc ./test.sh
```

When no `INPUTFILE` is provided, `input.txt` is used by default.

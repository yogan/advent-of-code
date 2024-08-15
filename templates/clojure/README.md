# Advent of Code in Clojure 

Created with `lein new app advent-of-code-template`

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

- [Leiningen](https://leiningen.org/)
    - `apt install leiningen` (v2.9.1-5 from Ubuntu 22.04 is fine)
- VS Code with [Calva extension](https://marketplace.visualstudio.com/items?itemName=betterthantomorrow.calva)

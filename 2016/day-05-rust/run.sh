#!/bin/sh
if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample.txt"
fi

cargo run --release --quiet "$input"

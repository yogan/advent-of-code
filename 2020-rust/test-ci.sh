#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


[ ! -d inputs ] && mkdir inputs

days=$(find src | grep day | sort | sed 's/^.*\([0-9][0-9]\).*$/\1/')

for day in ${days}; do
    if [ ! -f "inputs/day${day}.txt" ]; then
        ../scripts/aoc-get.sh 2020 "${day}"
        mv input.txt inputs/day"${day}".txt
    fi
done

# Sometimes rustup complains that no default toolchain is configured.
# Could be caused by network issues, see: https://stackoverflow.com/a/46864309
rustup default stable

for day in ${days}; do
    ./main.sh "${day}"
done

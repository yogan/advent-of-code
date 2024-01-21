#!/bin/bash
cd "$(dirname "${BASH_SOURCE[0]}")"/../vim || exit 1

mapfile -t yd < <(find . -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)

for year in "${yd[@]}"; do
    ../scripts/test-days.sh "$year"
done

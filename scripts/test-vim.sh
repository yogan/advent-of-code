#!/bin/bash
initial_dir=$(pwd)
cd "$(dirname "${BASH_SOURCE[0]}")"/../vim || exit 1

exit_code=0

mapfile -t yd < <(find . -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort)

for year in "${yd[@]}"; do
    if ! ../scripts/test-days.sh "$year"; then
        exit_code=1
    fi
done

cd "$initial_dir" || exit 1
exit $exit_code

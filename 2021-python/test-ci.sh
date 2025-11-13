#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


[ ! -d inputs ] && mkdir inputs

# shellcheck disable=SC2012
days=$(ls day??.py | sort | sed 's/[^0-9]//g')

for day in ${days}; do
    if [ ! -f "inputs/${day}/input.txt" ]; then
        ../scripts/aoc-get.sh 2021 "${day}"
        [ ! -d inputs/"${day}" ] && mkdir inputs/"${day}"
        mv input.txt inputs/"${day}"/
    fi
done

exit_code=0

for day in ${days}; do
    if ! pypy3 day"${day}".py; then
        exit_code=1
        exit "${exit_code}"
    fi
done

exit "${exit_code}"

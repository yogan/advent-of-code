#!/bin/bash
set -euo pipefail

FILENAME="input.txt"

stderr() {
    echo "$@" >&2
}

if [ -z "${AOC_SESSION}" ]; then
    stderr "AOC_SESSION is not set - check session cookie on https://adventofcode.com"
    stderr "In fish: set -Ux AOC_SESSION <value>"
    exit 1
fi

if [ $# -ge 2 ] && [ -n "$1" ] && [ -n "$2" ]; then
    year=$1
    day=$2
else
    year=$(date +%Y)
    day=$(date +%d)
fi
day=${day#"${day%%[!0]*}"}

if ! echo "${year}" | grep -qE '^(201[5-9]|20[2-9][0-9])$'; then
    stderr "Invalid year: ${year} (must be 2015-2099)"
    exit 1
fi
if ! echo "${day}" | grep -qE '^([1-9]|1[0-9]|2[0-5])$'; then
    stderr "Invalid day: ${day} (must be 1-25)"
    exit 1
fi

if ! curl --fail --silent --output "${FILENAME}" \
    --cookie "session=${AOC_SESSION}" \
    "https://adventofcode.com/${year}/day/${day}/input"; then
    stderr "Failed to get ${FILENAME} (curl error code $?)."
    exit 1
fi

[ ! -f "${FILENAME}" ] && stderr "Failed to get ${FILENAME}." && exit 1

lines=$(wc -l "${FILENAME}" | cut -d' ' -f1)
if [ "${lines}" -eq 0 ] || [ "${lines}" -eq 1 ]; then
    stats="single line with $(wc -c "${FILENAME}" | cut -d' ' -f1) chars"
else
    stats="${lines} lines"
fi
stderr "Saved ${FILENAME} for year ${year}, day ${day} (${stats})."

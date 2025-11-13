#!/bin/bash
set -euo pipefail

FILENAME="input.txt"
SAMPLE="sample.txt"

stderr() {
    echo "$@" >&2
}

if [ -z "${I18N_SESSION}" ]; then
    stderr "I18N_SESSION is not set - check sessionid cookie on https://i18n-puzzles.com"
    stderr "In fish: set -Ux I18N_SESSION <value>"
    exit 1
fi

if [ "$1" = "-s" ] || [ "$1" = "--sample" ]; then
    GET_SAMPLE=1
    shift
fi

if [ -n "$1" ]; then
    day=$1
else
    stderr "Usage: $(basename "$0") [-s|--sample] day"
    exit 1
fi

day=${day#"${day%%[!0]*}"}

if ! echo "${day}" | grep -qE '^([1-9]|1[0-9]|2[0-5])$'; then
    stderr "Invalid day: ${day} (must be 1-25)"
    exit 1
fi

if [ -n "${GET_SAMPLE}" ]; then
    if ! curl --fail --silent --output "${SAMPLE}" \
        "https://i18n-puzzles.com/puzzle/${day}/test-input"; then
        stderr "Failed to get ${SAMPLE} (curl error code $?)."
        exit 1
    fi
    [ ! -f "${SAMPLE}" ] && stderr "Failed to get ${SAMPLE}." && exit 1
    stderr "Saved i18n ${SAMPLE} for day ${day}."
fi

if ! curl --fail --silent --output "${FILENAME}" \
    --cookie "sessionid=${I18N_SESSION}" \
    "https://i18n-puzzles.com/puzzle/${day}/input"; then
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
stderr "Saved i18n ${FILENAME} for day ${day} (${stats})."

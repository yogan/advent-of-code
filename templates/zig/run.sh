#!/bin/bash
set -eo pipefail
cd "$(dirname "$0")" || exit 1

filename="input.txt"
if [ "${1:-}" != "" ]; then
    filename=$1
fi
zig build run -- "${filename}"

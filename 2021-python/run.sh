#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

day=$(printf "%02d" "$1")
python3 "day${day}.py"

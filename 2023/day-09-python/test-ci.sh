#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# Assertions in Python script are all we have for this…
./sample.sh && ./run.sh

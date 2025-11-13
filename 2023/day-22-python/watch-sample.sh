#!/bin/bash
set -euo pipefail
find . -name '*.py' -or -name '*.txt' | entr -cc ./sample.sh "$@"

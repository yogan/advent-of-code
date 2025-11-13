#!/bin/bash
set -euo pipefail
find . -name '*.clj' -or -name '*.txt' | entr -cc ./run.sh

#!/bin/bash
set -euo pipefail
find . -name '*.gleam' -or -name '*.txt' | entr -cc ./run.sh

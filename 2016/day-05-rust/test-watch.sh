#!/bin/bash
set -euo pipefail
find . -name '*.rs' -or -name '*.txt' | entr -cc ./test.sh

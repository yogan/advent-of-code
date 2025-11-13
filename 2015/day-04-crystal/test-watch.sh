#!/bin/bash
set -euo pipefail
find . -name '*.cr' -or -name '*.txt' | entr -cc ./test.sh

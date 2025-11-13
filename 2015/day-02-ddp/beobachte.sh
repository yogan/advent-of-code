#!/bin/bash
set -euo pipefail
find . -name '*.ddp' -or -name '*.txt' | entr -cc ./fuehre-aus.sh

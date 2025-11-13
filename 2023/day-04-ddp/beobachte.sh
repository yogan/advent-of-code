#!/bin/bash
set -euo pipefail
find . -name '*.ddp' -or -name '*.txt' -or -name '*.text' | entr -cc ./fuehre-aus.sh

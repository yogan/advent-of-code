#!/bin/bash
set -euo pipefail
fd | entr -cc ./run.sh "$@"

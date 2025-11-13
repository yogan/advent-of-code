#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

./run.sh --test && ./run.sh --sample && ./run.sh

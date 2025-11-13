#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

bun run main.ts "$@"

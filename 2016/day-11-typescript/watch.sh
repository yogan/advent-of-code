#!/bin/bash
set -euo pipefail
bun run --watch main.ts "$@"

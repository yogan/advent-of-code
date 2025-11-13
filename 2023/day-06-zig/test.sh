#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

zig build test && echo "Tests passed"

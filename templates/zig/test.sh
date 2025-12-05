#!/bin/bash
set -eo pipefail
cd "$(dirname "$0")" || exit 1

zig build test && echo "Tests passed"

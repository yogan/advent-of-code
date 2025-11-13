#!/bin/bash
set -euo pipefail
find src/ -name '*.zig' | entr -cc ./test.sh

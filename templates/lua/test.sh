#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# Needs busted globally installed, e.g. via:
#   sudo apt install luarocks
#   sudo apt luarocks install busted
# or:
#   sudo apt install lua-busted
busted aoc_spec.lua

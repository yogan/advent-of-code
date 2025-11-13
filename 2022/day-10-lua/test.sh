#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# Needs busted globally installed, e.g. via:
# sudo install luarocks
# sudo luarocks install busted
busted day10_spec.lua

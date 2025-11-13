#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

julia --project=. test/runtests.jl

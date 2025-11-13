#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

cd build || exit 1
if cmake --build .; then
    ./tests/tests
else
    exit 1
fi

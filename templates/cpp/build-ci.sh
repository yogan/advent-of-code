#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if [ ! -d "build" ]; then
  mkdir build
fi
cd build || exit 1
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
cmake --build .

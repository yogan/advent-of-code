#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

dune build

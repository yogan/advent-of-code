#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

nim compile --run --verbosity:0 aoctests

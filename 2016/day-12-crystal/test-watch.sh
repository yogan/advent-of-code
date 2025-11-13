#!/bin/bash
set -euo pipefail
fd | entr -cc ./test.sh

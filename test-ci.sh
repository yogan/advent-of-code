#!/bin/bash
set -euo pipefail
./test-templates.sh
echo
echo
./test-solutions.sh

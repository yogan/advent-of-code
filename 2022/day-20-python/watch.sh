#!/bin/bash
set -euo pipefail
run_sample=1
run_real=1

if [ "$1" = "--sample" ]; then
    run_real=0
elif [ "$1" = "--real" ]; then
    run_sample=0
fi

while inotifywait -qq -e close_write ./*.py ; do
    if [ "${run_sample}" -eq 1 ]; then
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                           S A M P L E                           │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        ./sample.sh
    fi
    if [ "${run_real}" -eq 1 ]; then
        echo
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                       R E A L   I N P U T                       │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        ./run.sh
    fi
done

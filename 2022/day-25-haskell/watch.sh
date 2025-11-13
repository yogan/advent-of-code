#!/bin/bash
set -euo pipefail
run_sample=1
run_real=1

if [ "$1" = "--sample" ]; then
    run_sample=1
    run_real=0
elif [ "$1" = "--real" ]; then
    run_sample=0
    run_real=1
fi

run() {
    clear
    if [ "${run_sample}" -eq 1 ]; then
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                           S A M P L E                           │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        runghc day*hs sample.txt
    fi
    if [ "${run_real}" -eq 1 ]; then
        echo
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                       R E A L   I N P U T                       │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        runghc day*hs input.txt
    fi
}

run
while inotifywait -qq -e close_write *.hs ; do
    run
done

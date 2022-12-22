#!/bin/sh
run_sample=1
run_sample_extra=0
run_real=1

if [ "$1" = "--sample" ]; then
    run_real=0
elif [ "$1" = "--sample-extra" ]; then
    run_sample=0
    run_sample_extra=1
    run_real=0
elif [ "$1" = "--real" ]; then
    run_sample=0
fi

while inotifywait -qq -e close_write *.py ; do
    clear
    if [ $run_sample -eq 1 ]; then
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                           S A M P L E                           │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        ./sample.sh
    fi
    if [ $run_sample_extra -eq 1 ]; then
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                   E X T R A   S A M P L E S                     │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        python3 day*.py sample_extra.txt
    fi
    if [ $run_real -eq 1 ]; then
        echo
        echo "  ┌─────────────────────────────────────────────────────────────────┐"
        echo "  │                       R E A L   I N P U T                       │"
        echo "  └─────────────────────────────────────────────────────────────────┘"
        ./run.sh
    fi
done

#!/bin/sh
filename="input.txt"
if [ "$1" != "" ]; then
    filename=$1
fi
nim compile --run --verbosity:0 main "$filename"

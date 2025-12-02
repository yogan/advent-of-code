#!/bin/bash
cd "$(dirname "$0")" || exit 1
./test.sh && ./run.sh sample.txt && ./run.sh input.txt

#!/bin/bash
# The Python script runs both unit tests and assertions for end results, nothing
# more to do here besides running with sample and real input data.
./sample.sh 1 >/dev/null 2>&1 \
    && ./sample.sh 2 >/dev/null 2>&1 \
    && ./sample.sh 3 >/dev/null 2>&1 \
    && ./sample.sh 4 >/dev/null 2>&1 \
    && ./run.sh >/dev/null 2>&1

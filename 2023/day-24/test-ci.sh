#!/bin/bash
pypy3 -m venv venv >/dev/null 2>&1
source venv/bin/activate >/dev/null 2>&1
pypy3 -m pip install -r requirements.txt >/dev/null 2>&1

# The Python script runs both unit tests and assertions for end results, nothing
# more to do here besides running with sample and real input data.
./sample.sh >/dev/null 2>&1 && ./run.sh >/dev/null 2>&1

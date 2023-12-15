#!/bin/sh
TEST_LOG="test_log.txt"
SAMPLE_LOG="sample_log.txt"
RUN_LOG="run_log.txt"

./test.sh >"$TEST_LOG" 2>&1
TEST_EXIT_CODE=$?

./sample.sh >"$SAMPLE_LOG" 2>&1
SAMPLE_EXIT_CODE=$?

./run.sh >"$RUN_LOG" 2>&1
RUN_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -ne 0 ]; then
    echo "Tests failed." >&2
    cat "$TEST_LOG" >&2
    EXIT_CODE=1
elif [ $SAMPLE_EXIT_CODE -ne 0 ]; then
    echo "Run with sample data failed." >&2
    cat "$SAMPLE_LOG" >&2
    EXIT_CODE=2
elif [ $RUN_EXIT_CODE -ne 0 ]; then
    echo "Run with real input failed." >&2
    cat "$RUN_LOG" >&2
    EXIT_CODE=3
else
    EXIT_CODE=0
fi

rm -f "$TEST_LOG" "$SAMPLE_LOG" "$RUN_LOG"

exit $EXIT_CODE

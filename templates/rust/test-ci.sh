#!/bin/sh
TEST_LOG="test.log"
SAMPLE_LOG="sample.log"
RUN_LOG="run.log"

execute_command() {
    command="$1"
    log_file="$2"
    $command >"$log_file" 2>&1
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        echo "Error running \"$command\". Printing log file \"$log_file\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "$log_file" >&2
        rm -f "$log_file"
        exit $exit_code
    fi
}

execute_command "./test.sh" "$TEST_LOG"
execute_command "./run.sh sample.txt" "$SAMPLE_LOG"
execute_command "./run.sh input.txt" "$RUN_LOG"

rm -f "$TEST_LOG" "$SAMPLE_LOG" "$RUN_LOG"

#!/bin/sh

RUSTUP_LOG="rustup_log.txt"
TEST_LOG="test_log.txt"
SAMPLE_LOG="sample_log.txt"
RUN_LOG="run_log.txt"

execute_command() {
    command="$1"
    log_file="$2"
    $command >"$log_file" 2>&1
    exit_code=$?
    if [ $exit_code -ne 0 ]; then
        echo "Error running \"$command\". Printing log file \"$log_file\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "$log_file" >&2
        exit $exit_code
    fi
}

# Sometimes rustup complains that no default toolchain is configured.
# Could be caused by network issues, see: https://stackoverflow.com/a/46864309
execute_command "rustup default stable" "$RUSTUP_LOG"

execute_command "./test.sh" "$TEST_LOG"
execute_command "./sample.sh" "$SAMPLE_LOG"
execute_command "./run.sh" "$RUN_LOG"

rm -f "$RUSTUP_LOG" "$TEST_LOG" "$SAMPLE_LOG" "$RUN_LOG"

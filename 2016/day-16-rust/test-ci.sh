#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

TEST_LOG="test_log.txt"
RUN_LOG="run_log.txt"

execute_command() {
    command="$1"
    log_file="$2"
    ${command} >"${log_file}" 2>&1
    exit_code=$?
    if [ "${exit_code}" -ne 0 ]; then
        echo "Error running \"${command}\". Printing log file \"${log_file}\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "${log_file}" >&2
        rm -f "${log_file}"
        exit "${exit_code}"
    fi
}

execute_command "./test.sh" "${TEST_LOG}"
execute_command "./run.sh" "${RUN_LOG}"

rm -f "${TEST_LOG}" "${RUN_LOG}"

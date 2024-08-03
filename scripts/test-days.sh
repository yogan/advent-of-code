#!/bin/bash
if [ -z "$1" ]; then
    echo "Usage: $0 <year>" >&2
    exit 1
fi
year="$1"

script_dir=$(dirname "$(realpath "$0")")
initial_dir=$(pwd)
cd "$year" || exit 1
year_dir=$(pwd)

exit_code=0

execute_command() {
    command="$1"
    log_file="$2"
    $command >"$log_file" 2>&1
    code=$?
    if [ $code -ne 0 ]; then
        echo "Error running \"$command\". Printing log file \"$log_file\":" >&2
        echo "------------------------------------------------------------" >&2
        cat "$log_file" >&2
        rm -f "$log_file"
        return $code
    fi
    rm -f "$log_file"
}

mapfile -t day_dirs < <(find -L . -mindepth 1 -maxdepth 1 -type d -iname 'day*' -printf '%f\n' | sort)

for day_dir in "${day_dirs[@]}"; do
    lang=$(<"$day_dir/.language")
    echo -n "$year/$day_dir ($lang)… "

    if [ ! -x "$day_dir/test-ci.sh" ]; then
        echo "SKIPPED (no test-ci.sh found)"
        continue
    fi

    cd "$day_dir" || exit 1

    if [ ! -f "input.txt" ]; then
        day=$(echo "$day_dir" | sed -E 's/day-([0-9]+)-?.*/\1/')
        if ! "$script_dir/aoc-get.sh" "$year" "$day" 2>/dev/null; then
            echo "FAILED (could not download input)"
            exit_code=1
            cd "$year_dir" || exit 1
            continue
        fi
    fi

    if [ "$lang" = "Haskell" ]; then
        "$script_dir/haskell-prepare.sh"
    fi

    # optional build step
    if [ -x "build-ci.sh" ]; then
        start=$(date +%s.%N)
        if ! execute_command "./build-ci.sh" "build.log"; then
            echo "FAILED (build)"
            exit_code=1
            cd "$year_dir" || exit 1
            continue
        fi
        end=$(date +%s.%N)
        build_runtime=$(printf "%.0f" "$(echo "($end - $start) * 1000" | bc)")
        if [ "$build_runtime" -ge 1000 ]; then
            build_runtime=$(printf "%.1f s" "$(echo "$build_runtime / 1000" | bc -l)")
        else
            build_runtime="${build_runtime} ms"
        fi
    else
        build_runtime=""
    fi

    start=$(date +%s.%N)
    if ! execute_command "./test-ci.sh" "test.log"; then
        echo "FAILED"
        exit_code=1
    else
        end=$(date +%s.%N)
        runtime=$(printf "%.0f" "$(echo "($end - $start) * 1000" | bc)")
        if [ "$runtime" -ge 1000 ]; then
            runtime=$(printf "%.1f s" "$(echo "$runtime / 1000" | bc -l)")
        else
            runtime="${runtime} ms"
        fi
        if [ -n "$build_runtime" ]; then
            runtime_stats="build: ${build_runtime}, run: ${runtime}"
        else
            runtime_stats="${runtime}"
        fi
        echo "OK ($runtime_stats)"
    fi

    if [ "$lang" = "Haskell" ]; then
        # Running will make every next Haskell day slower, because stack will
        # have to re-download all its stuff, but currently there are only two
        # Haskell solutions (in two different years), so it's not a big deal.
        # And we do run out of disk space without this… :-(
        "$script_dir/haskell-cleanup.sh"
    fi

    cd "$year_dir" || exit 1
done

cd "$initial_dir" || exit 1
exit $exit_code

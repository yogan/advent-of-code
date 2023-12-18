#!/bin/bash
exit_code=0
cwd=$(pwd)
year=$(basename "$cwd")

mapfile -t day_dirs < <(find . -mindepth 1 -maxdepth 1 -type d -iname 'day*' -printf '%f\n' | sort)

for day_dir in "${day_dirs[@]}"; do
    lang=$(<"$day_dir/.language")
    echo -n "$year/$day_dir ($lang)… "

    if [ ! -x "$day_dir/test-ci.sh" ]; then
        echo "SKIPPED (no test-ci.sh found)"
        continue
    fi

    cd "$day_dir" || exit 1

    if [ ! -f "input.txt" ]; then
        day=${day_dir//day-/}
        if ! ../../scripts/aoc-get.sh "$year" "$day" 2>/dev/null; then
            echo "FAILED (could not download input)"
            exit_code=1
            cd "$cwd" || exit 1
            continue
        fi
    fi

    # optional build step
    if [ -x "build-ci.sh" ]; then
        start=$(date +%s.%N)
        ./build-ci.sh
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
    if ! ./test-ci.sh; then
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

    cd "$cwd" || exit 1
done

cd "$cwd" || exit 1
exit $exit_code

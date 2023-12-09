#!/bin/bash
exit_code=0
cwd=$(pwd)
year=$(basename "$cwd")

mapfile -t day_dirs < <(find . -mindepth 1 -maxdepth 1 -type d -iname 'day*' -printf '%f\n' | sort)

for day_dir in "${day_dirs[@]}"; do
    echo -n "$year/$day_dir… "

    if [ ! -f "$day_dir/test-ci.sh" ]; then
        echo "SKIPPED (no test-ci.sh found)"
        continue
    fi

    cd "$day_dir" || exit 1

    if [ ! -f "input.txt" ]; then
        day=${day_dir//day-/}
        if ! ../../get-input.sh "$year" "$day" 2>/dev/null; then
            echo "FAILED (could not download input)"
            exit_code=1
            cd "$cwd" || exit 1
            continue
        fi
    fi

    if ! ./test-ci.sh; then
        echo "FAILED"
        exit_code=1
        cd "$cwd" || exit 1
    else
        echo "OK"
    fi

    cd "$cwd" || exit 1
done

cd "$cwd" || exit 1
exit $exit_code

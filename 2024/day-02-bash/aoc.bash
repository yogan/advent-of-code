#!/bin/bash
IFS=$'\n' read -d '' -ra reports

function is_safe() {
    local levels=("$@")
    local last=${levels[0]}
    local increasing=$((levels[0] < levels[1]))

    for level in "${levels[@]:1}"; do
        if (((last < level) != increasing)); then
            return 1
        fi

        local diff=$((level - last))
        local abs_diff=${diff#-}
        if ((abs_diff < 1 || abs_diff > 3)); then
            return 1
        fi

        last=$level
    done

    return 0
}

function is_safe_enough() {
    local levels=("$@")

    for i in $(seq 0 $((${#levels[@]} - 1))); do
        local levels_without_i=("${levels[@]:0:i}" "${levels[@]:i+1}")
        if is_safe "${levels_without_i[@]}"; then
            return 0
        fi
    done

    return 1
}

p1=0
p2=0

for report in "${reports[@]}"; do
    IFS=' ' read -ra levels <<<"$report"
    if is_safe "${levels[@]}"; then
        p1=$((p1 + 1))
    fi
    if is_safe_enough "${levels[@]}"; then
        p2=$((p2 + 1))
    fi
done

echo $p1
echo $p2

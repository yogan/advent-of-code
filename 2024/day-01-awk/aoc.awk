{ left[NR] = $1; right[NR] = $2; right_counter[$2]++ }

function abs(x) { return x < 0 ? -x : x }

END {
    asort(left); asort(right)
    for (i in right) { p1 += abs(right[i] - left[i]) }
    for (i in left)  { p2 += left[i] * right_counter[left[i]] }
    print p1; print p2
}

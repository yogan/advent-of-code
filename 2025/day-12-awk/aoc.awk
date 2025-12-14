BEGIN { n = 1; total = 0 }

# Count the number of #s in each shape.
/^[0-9]+:$/ { count = 0               ; next }
/^[#.]{3}$/ { count += gsub(/#/, "#") ; next }
/^$/        { shapes[n++] = count     ; next }

# Heuristic: if summed up #s for all shapes is less then the area,
# we assume that the pieces can fit in.
/[0-9]+x[0-9]+:/ {
    split($0, nums, /[^0-9]+/)
    area = nums[1] * nums[2]
    required = 0
    for (i = 1; i < n; i++) required += shapes[i] * nums[i+2]
    if (required <= area) total++
}

END { print total }

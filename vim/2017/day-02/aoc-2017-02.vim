:r input.txt|1d
:silent! %s/$/\r
:silent! %s/\s\+/\r/g
:norm gg
:let p1 = ':exe ":norm vip:sort! n\<cr>"'
:let p2 = ':norm jV}kkd\'
:let p3 = ':exe ":norm kA-\<esc>J"'
:let p4 = ':s/.*/\=eval(submatch(0))'
:let p5 = ':norm j'
:let cr = "\<cr>"
:let @m = p1 . cr . p2 . cr . p3 . cr . p4 . cr . p5 . cr
:norm 999@m
:g/^$/d
:silent! %s/$/+
:norm $xvipJ
:s/.*/\=eval(submatch(0))
:r input.txt|1d a
:function DivisibleValue(line)
    let num_strings = split(a:line)
    let nums = map(num_strings, 'str2nr(v:val)')
    for i in range(0, len(nums) - 2)
        for j in range(i + 1, len(nums) - 1)
            let x = nums[i]
            let y = nums[j]
            let a = max([x, y])
            let b = min([x, y])
            if a % b == 0
                return a / b
            endif
        endfor
    endfor
endfunction
:function Part2()
    let sum = 0
    for line in getline(1, '$')
        let sum += DivisibleValue(line)
    endfor
    return sum
endfunction
:pu! =Part2()
:silent! 2,$d
:pu! a
:x! out

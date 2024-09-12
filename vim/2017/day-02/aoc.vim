"                       Advent of Code 2017 Day 2 in Vim
"                  Solved with a bunch of wonderful people at
"      _____       ______          ______             ___   ____ ___  __ __
"     / ___/____  / ____/________ /_  __/__  _____   |__ \ / __ \__ \/ // /
"     \__ \/ __ \/ /   / ___/ __ `// / / _ \/ ___/   __/ // / / /_/ / // /_
"    ___/ / /_/ / /___/ /  / /_/ // / /  __(__  )   / __// /_/ / __/__  __/
"   /____/\____/\____/_/   \__,_//_/  \___/____/   /____/\____/____/ /_/
"

" -------- part 1 -------------------------------------------------------------

" read input and remove blank first line
:r input.txt|1d

" add extra blank lines between input lines
:silent! %s/$/\r

" converting spaces to line breaks
:silent! %s/\s\+/\r/g

" go to top (:1 would work as well)
:norm gg

" select one paragraph (vip), sort paragraph numerically
:let p1 = ':exe ":norm vip:sort! n\<cr>"'
" we end at start of paragraph

" only keep top and bottom line of paragraph (largest / smallest)
:let p2 = ':norm jV}kkd\'

" join the remaining two lines together with a minus sign in between
:let p3 = ':exe ":norm kA-\<esc>J"'

" evaluate the expression to get the difference
:let p4 = ':s/.*/\=eval(submatch(0))'

" move to next paragraph
:let p5 = ':norm j'

" create macro (with enter keys between each command to execute it)
:let cr = "\<cr>"
:let @m = p1 . cr . p2 . cr . p3 . cr . p4 . cr . p5 . cr

" execute the macro for all lines (it stops at end of buffer)
:norm 999@m

" delete empty lines
:g/^$/d

" add plus at the end of each line
:silent! %s/$/+

" get rid of trailing + in last line, join all lines
:norm $xvipJ

" eval the sum
:s/.*/\=eval(submatch(0))

" -------- part 2 -------------------------------------------------------------

" read input, delete first line (part 1 result) to register a
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

" -------- output -------------------------------------------------------------

" calculate part 2 and put it at the top of the buffer
:pu! =Part2()

" get rid of input lines
:silent! 2,$d

" restore part 1 result from register a
:pu! a

" kthxbye
:x! out

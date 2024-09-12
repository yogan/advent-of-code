" ----------------------------------- input ------------------------------------
" read data from input file, remove empty blank top line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------
" calculate the sum of the volumes of the boxes

" evaluate LxWxH as L*W*H and add a trailing +
:sil %s/\v(\d+)x(\d+)x(\d+)/\=submatch(1)*submatch(2)*submatch(3) . '+'

" join lines together, delete trailing +
:norm vipgJ$x

" evaluate the sum
:s/.*/\=eval(submatch(0))

" ----------------------------------- part 2 -----------------------------------
" calculate the sum of the surface area of the boxes

" To show off function usage, and also because the substitution / expression
" thingy really looks ugly for the surface area formula, we use a different
" approach than for part 1 here.

:function Part2()
    let sum = 0
    for line in readfile('input.txt')
        let [l, w, h] = split(line, 'x')
        let sum += 2*l*w + 2*w*h + 2*h*l
    endfor
    return sum
endfunction

:pu =Part2()

" ----------------------------------- output -----------------------------------
" write results to output file
:x! out

" ----------------------------------- part 1 -----------------------------------

" read data from input file, remove empty blank top line
:r input.txt|1d

" first line is our step counter (and an implicit exit), last line is an exit
:norm O0
:norm Goexit
:2

" yank line to register a
:let s1 = '"ayy'
" turn offsets into movements:
:let s2 = ":sil! s/\\v^[1-9]\\d*$/k\\0j\<cr>"
:let s3 = ":sil! s/\\v^-(\\d+)$/k\\1k\<cr>"
:let s4 = ":sil! s/^0$/h\<cr>"
" delete line to reg b, paste back reg a above
:let s5 = '"bdd"aP'
" increment number in line, increment counter at top, jump back
:let s6 = "\<C-a>gg\<C-a>\<C-o>"
" execute register b as macro an pray
:let s7 = "@b"

:let @q = s1 . s2 . s3 . s4 . s5 . s6 . s7 . "l"

:function Solve()
    while getline(".") != "exit" && line(".") != 1
        norm @q
    endwhile
    sil 2,$d
endfunction

:call Solve()

" ----------------------------------- part 2 -----------------------------------

:function Part2()
    let steps = 0
    let numbers = []
    let i = 0
    " for line in readfile("sample.txt")
    for line in readfile("input.txt")
        call add(numbers, str2nr(line))
    endfor
    while i >= 0 && i < len(numbers)
        let steps += 1
        let offset = numbers[i]
        if offset >= 3
            let numbers[i] -= 1
        else
            let numbers[i] += 1
        endif
        let i += offset
    endwhile
    return steps
endfunction

:pu =Part2()

" ----------------------------------- output -----------------------------------
" write results to output file
:x! out

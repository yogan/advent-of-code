"           .__
"           |__| ____   _______  __ ____ ___  ___
"           |  |/    \ /  _ \  \/ // __ \\  \/  /
"           |  |   |  (  <_> )   /\  ___/ >    <
"           |__|___|  /\____/ \_/  \___  >__/\_ \
"                   \/                 \/      \/   Sommerplenum 2024

" ----------------------------------- part 1 -----------------------------------

" read data from input file, remove empty blank top line
:r input.txt|1d

" first line is our step counter (and implicit exit), last line is exit
:norm O0
:norm Goexit
:2

" store original offset in register a
:let s1 = '"ayy'
" turn offsets into movements (should not be empty, so we use h for 0, which
" does nothing); one regex for positive/negative/0, only one will match
:let s2 = ":sil! s/\\v^[1-9]\\d*$/k\\0j\<cr>"
:let s3 = ":sil! s/\\v^-(\\d+)$/k\\1k\<cr>"
:let s4 = ":sil! s/^0$/h\<cr>"
" delete movement to register b, paste back original offset from register a
:let s5 = '"bdd"aP'
" increment offset (current line), increment counter (top), jump back
:let s6 = "\<C-a>gg\<C-a>\<C-o>"
" execute register b (the movement) as macro and pray
:let s7 = "@b"
" all together now
:let @q = s1 . s2 . s3 . s4 . s5 . s6 . s7

:function Solve()
    while getline(".") != "exit" && line(".") != 1
        norm @q
    endwhile
    sil 2,$d
endfunction

:call Solve()

" ----------------------------------- part 2 -----------------------------------

" Pure Vimscript required, an approach like for part 1 is too slow and for
" unknown reasons uses A LOT of memory. This function gets it done in about
" 2:30 minutes, which is acceptable (we end up with a step counter of over
" 28 million).

:function Part2()
    let numbers = []
    for line in readfile("input.txt")
        call add(numbers, str2nr(line))
    endfor
    let steps = 0
    let i = 0
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

:x! out

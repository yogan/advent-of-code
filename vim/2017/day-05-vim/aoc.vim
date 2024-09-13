" ----------------------------------- input ------------------------------------
" read data from input file, remove empty blank top line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" first line is our step counter (and an implicit exit), last line is an exit
:norm O0
:norm Goexit
:2

" yank line to register a
:let s1 = '"ayy'
" turn offsets into movements:
:let s2 = ":sil! s/\\v^[1-9]\\d*$/k\\0j\<cr>"
:let s3 = ":sil! s/^\\v-(\\d+)$/k\\1k\<cr>"
:let s4 = ":sil! s/^0$/h\<cr>"
" delete line to reg b, paste back reg a above
:let s5 = '"bdd"aP'
" increment number in line, increment counter at top, jump back
:let s6 = "\<C-a>gg\<C-a>\<C-o>"
" execute register b as macro an pray
:let s7 = "@b"

:let @q = s1 . s2 . s3 . s4 . s5 . s6 . s7 . "l"

:function Part1()
    while getline(".") != "exit" && line(".") != 1
        norm @q
    endwhile
    sil 2,$d
endfunction

:call Part1()

" ----------------------------------- output -----------------------------------
" write results to output file
:x! out

" ----------------------------------- input ------------------------------------

" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" You'd think that we can just yank the movement commands of each line into a
" register and execute it as a macro, or :normal it, but no. When we are trying
" to do a movement that does not work (e.g. moving left in the first column),
" this is treated as an error and subsequent commands are aborted (see :help
" :normal).
" So we build our own function, called Unnormal, which executes each character
" as a single command.
:function Unnormal(line)
:   for c in split(a:line, '\zs')
:       exe 'norm ' . c
:   endfor
:endfunction

:function Part1()
"   convert input to Vim movements (golfy way with the tr() function)
:   silent %s/.*/\=tr(submatch(0), 'LDUR', 'hjkl')/g
:   let lines = getline(1, '$')
"   delete input and create the keypad, kh to move cursor to the middle (5)
:   exe "norm cgg123\r456\r789\<esc>kh"
"   "execute" each line with our unnormal function and build the result
:   let result = ''
:   for line in lines
:       call Unnormal(line)
"       append digit under cursor to result
:       let result .= getline('.')[col('.') - 1]
:   endfor
"   put result in buffer and clear the keypad above
:   pu =result|norm kdgg
:endfunction

:call Part1()

" ----------------------------------- output -----------------------------------

" kthxbye
:x! out

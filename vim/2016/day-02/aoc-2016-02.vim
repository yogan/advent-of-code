:r input.txt|1d
:function! Unnormal(line)
:   for c in split(a:line, '\zs')
:       exe 'norm ' . c
:   endfor
:endfunction
:function Part1()
:   silent %s/.*/\=tr(submatch(0), 'LDUR', 'hjkl')/g
:   let lines = getline(1, '$')
:   exe "norm cgg123\r456\r789\<esc>kh"
:   let result = ''
:   for line in lines
:       call Unnormal(line)
:       let result .= getline('.')[col('.') - 1]
:   endfor
:   pu =result|norm kdgg
:endfunction
:call Part1()
:x! out

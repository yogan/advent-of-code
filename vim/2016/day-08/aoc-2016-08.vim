:let g:animate = 0
:r input.txt | 1d
:function Part1()
:  let lines = getline(1, '$')
:  1,$d
:  exe "norm 50i.\<esc>yy5p"
:  for line in lines
:    call ParseLine(line)
:    if (g:animate) | redraw | sleep 60m | endif
:  endfor
:  if (g:animate) | call input("Press Enter to count pixels") | endif
:  norm vipgJ
:  s/\.//g
:  s/.*/\=col('$') - 1
:endfunction
:function ParseLine(line)
:  let words = split(a:line)
:  if words[0] == 'rect'
:    let xy = split(words[1], 'x')
:    call Rect(str2nr(xy[0]), str2nr(xy[1]))
:  elseif words[0] == 'rotate'
:    let xy = str2nr(split(words[2], '=')[1])
:    let offset = str2nr(words[4])
:    if words[1] == 'column'
:      call RotateColumn(xy, offset)
:    elseif words[1] == 'row'
:      call RotateRow(xy, offset)
:    endif
:  endif
:endfunction
:function Rect(x, y)
:  if (a:x > 1) | let right = (a:x - 1) . "l" | else | let right = "" | endif
:  if (a:y > 1) | let down  = (a:y - 1) . "j" | else | let down = ""  | endif
:  0 | exe "norm \<c-v>" . right . down . "r#"
:endfunction
:function RotateColumn(col, offset)
:  let offset = a:offset % 6
:  if offset == 0 | return | endif
:  call cursor(1, (a:col + 1))
:  exe "norm \<c-v>6jx"
:  let cs = split(@@, "\n")
:  let cs = cs[-offset:] + cs[:-(offset + 1)]
:  call setreg('"', join(cs, "\n"), "b")
:  norm P
:endfunction
:function RotateRow(row, offset)
:  let offset = a:offset % 50
:  if offset == 0 | return | endif
:  call cursor(a:row + 1, 50 - a:offset + 1)
:  norm D0P
:endfunction
:silent call Part1()
:if (!g:animate) | :x! out | endif

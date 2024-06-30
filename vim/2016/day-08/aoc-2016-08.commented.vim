" ----------------------------------- input ------------------------------------

" NOTE: set this to 1 to see the animation (does not work with vim -s, use :so)
:let g:animate = 0

" read input and remove blank first line
:r input.txt | 1d

" ----------------------------------- part 1 -----------------------------------

:function Part1()
"  read lines and clear buffer
:  let lines = getline(1, '$')
:  1,$d
:
"  create a blank screen of 50×6
:  exe "norm 50i.\<esc>yy5p"
:
"  perform the operations on the screen
:  for line in lines
:    call ParseLine(line)
:    if (g:animate) | redraw | sleep 60m | endif
:  endfor
:
"  yank result to register a (for part 2)
:  1,$y a
:
"  join all lines, remove empty pixels
:  norm vipgJ
:  s/\.//g
:
"  number of pixels (result for part 1) is the line length
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
"  Use visual block mode to select the rectangle, fill it with # with r#.
"  We need to subtract 1 from x and y to because selection includes first line
"  and column without any movement.
"  :0 ensures that we start the selection from the top left corner.
:  if (a:x > 1) | let right = (a:x - 1) . "l" | else | let right = "" | endif
:  if (a:y > 1) | let down  = (a:y - 1) . "j" | else | let down = ""  | endif
:  0 | exe "norm \<c-v>" . right . down . "r#"
:endfunction

:function RotateColumn(col, offset)
"  make sure offset is in range, also do nothing if offset is 0
:  let offset = a:offset % 6
:  if offset == 0 | return | endif
"  position cursor at the top of the column
:  call cursor(1, (a:col + 1))
"  delete the column (fixed height of 6) into default register via visual block
:  exe "norm \<c-v>6jx"
"  get chars of column as array by splitting default register (@@) on newlines
:  let cs = split(@@, "\n")
"  rotate the array by offset
:  let cs = cs[-offset:] + cs[:-(offset + 1)]
"  put it back into the default register; "b" is important to treat it as block
:  call setreg('"', join(cs, "\n"), "b")
"  paste the rotated column back into the column
:  norm P
:endfunction

:function RotateRow(row, offset)
"  make sure offset is in range, also do nothing if offset is 0
:  let offset = a:offset % 50
:  if offset == 0 | return | endif
"  position cursor in row, offset chars from the right
:  call cursor(a:row + 1, 50 - a:offset + 1)
"  delete until end of line, paste back at the beginning
:  norm D0P
:endfunction

:silent call Part1()

" ----------------------------------- part 2 -----------------------------------

:if (g:animate)
"   all we can do for part 2 is to make the letters a bit more readable
:   s/.*/\=getreg("a")
:   $d
:   silent %s/\./ /g
:   silent %s/#/█/g
:else
"   when running automated, we can't do any better than just printing the answer
:   s/$/\rZJHRKCPLYJ
:endif

" ----------------------------------- output -----------------------------------

" kthxbye
:if (!g:animate) | :x! out | endif

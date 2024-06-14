:r input.txt|1d
:%s/, /\r/g|0
:s/^/0 0;0 -1
:let r1 = ":s/;0 -1L/;-1 0;/e"
:let r2 = ":s/;1 0L/;0 -1;/e"
:let r3 = ":s/;0 1L/;1 0;/e"
:let r4 = ":s/;-1 0L/;0 1;/e"
:let r5 = ":s/;0 -1R/;1 0;/e"
:let r6 = ":s/;1 0R/;0 1;/e"
:let r7 = ":s/;0 1R/;-1 0;/e"
:let r8 = ":s/;-1 0R/;0 -1;/e"
:let rot = r1."\r".r2."\r".r3."\r".r4."\r".r5."\r".r6."\r".r7."\r".r8."\r"
:let c1 = ":silent! s/\\v(-?\\d+) (-?\\d+);(-?\\d+) (-?\\d+);(\\d+)/"
:let c2 = "\\=submatch(1)+submatch(3)*submatch(5).' '."
:let c3 = "eval(submatch(2)+submatch(4)*submatch(5))"
:let c4 = ".';'.submatch(3).' '.submatch(4)"
:let calc = c1.c2.c3.c4."\r"
:let @q = rot . calc . "gJ0"
:silent! exe "norm 999@q:redraw\r"
:s/\v(-?\d+) (-?\d+).*/\=abs(submatch(1))+abs(submatch(2))/
:x! out

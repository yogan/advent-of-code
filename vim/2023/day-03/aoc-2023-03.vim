:r input.txt|1d
:%s/.*/.\0./
:1|norm yy2P
:1,2s/././g|1m$|1
:set nowrapscan
:norm mi
:let spc = "O\<esc>mjyy2p"
:let sel = ":exe \"norm /\\\\d\\\<lt>cr>\"\<cr>\<c-v>iwjlohk"
:let cpy = "\"ay'jp3gJ"
:let clr = "gvjlohkr."
:let pos = "'i"
:let @q = spc . sel . cpy . clr . pos
:norm 2000@q
:,$d
:g/\v^(\.|\d)*$/d
:%s/\v^\D*(\d+)\D*$/\1+/
:norm $xvipgJ
:let @s = "0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out

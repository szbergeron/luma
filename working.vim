let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/oss/rsharp
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +31 src/ast.rs
badd +93 src/grammar.lalrpop
badd +1 examples/lex_expr_2.rsh
badd +1 examples/lex_expr_3.rsh
badd +0 examples/expr_specified_type.rsh
badd +35 src/parse.rs
badd +3 src/lex.rs
badd +93 src/helper.rs
badd +0 examples/nested_modules
badd +1 src/parse_expr.rs
badd +0 Cargo.toml
argglobal
%argdel
set stal=2
edit src/ast.rs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 157 - ((40 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
157
normal! 041|
wincmd w
argglobal
if bufexists("src/ast.rs") | buffer src/ast.rs | else | edit src/ast.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 134 - ((39 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
134
normal! 015|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
tabedit src/grammar.lalrpop
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe '2resize ' . ((&lines * 39 + 41) / 83)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
exe '3resize ' . ((&lines * 40 + 41) / 83)
exe 'vert 3resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 87 - ((10 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
87
normal! 030|
wincmd w
argglobal
if bufexists("src/ast.rs") | buffer src/ast.rs | else | edit src/ast.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 351 - ((29 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
351
normal! 05|
wincmd w
argglobal
if bufexists("src/ast.rs") | buffer src/ast.rs | else | edit src/ast.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 202 - ((31 * winheight(0) + 20) / 40)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
202
normal! 08|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe '2resize ' . ((&lines * 39 + 41) / 83)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
exe '3resize ' . ((&lines * 40 + 41) / 83)
exe 'vert 3resize ' . ((&columns * 147 + 147) / 295)
tabedit examples/lex_expr_2.rsh
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe '2resize ' . ((&lines * 39 + 41) / 83)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
exe '3resize ' . ((&lines * 40 + 41) / 83)
exe 'vert 3resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 037|
wincmd w
argglobal
if bufexists("examples/lex_expr_3.rsh") | buffer examples/lex_expr_3.rsh | else | edit examples/lex_expr_3.rsh | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 021|
wincmd w
argglobal
if bufexists("examples/expr_specified_type.rsh") | buffer examples/expr_specified_type.rsh | else | edit examples/expr_specified_type.rsh | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 20) / 40)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 027|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe '2resize ' . ((&lines * 39 + 41) / 83)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
exe '3resize ' . ((&lines * 40 + 41) / 83)
exe 'vert 3resize ' . ((&columns * 147 + 147) / 295)
tabedit src/parse.rs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 108 - ((35 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
108
normal! 03|
wincmd w
argglobal
if bufexists("src/parse.rs") | buffer src/parse.rs | else | edit src/parse.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 33 - ((32 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
33
normal! 0
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
tabedit Cargo.toml
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 13 - ((12 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
13
normal! 021|
tabedit src/lex.rs
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 121 - ((47 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
121
normal! 0
tabedit src/ast.rs
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 32 - ((31 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
32
normal! 039|
tabedit src/parse_expr.rs
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 19 - ((18 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
19
normal! 079|
tabedit src/parse.rs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 135 - ((53 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
135
normal! 014|
wincmd w
argglobal
if bufexists("src/lex.rs") | buffer src/lex.rs | else | edit src/lex.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 33 - ((32 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
33
normal! 011|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
tabedit src/helper.rs
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 47 - ((0 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
47
normal! 024|
wincmd w
argglobal
if bufexists("src/lex.rs") | buffer src/lex.rs | else | edit src/lex.rs | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 143 - ((27 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
143
normal! 020|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 147) / 295)
exe 'vert 2resize ' . ((&columns * 147 + 147) / 295)
tabedit examples/nested_modules
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 19 - ((18 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
19
normal! 034|
tabnext 4
set stal=1
if exists('s:wipebuf') && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 winminheight=1 winminwidth=1 shortmess=filnxtToOFc
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

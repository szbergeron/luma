let SessionLoad = 1
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/oss/rsharp
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 src/parse_helper.rs
badd +33 src/parse.rs
badd +32 src/parse_expr.rs
badd +552 src/ast.rs
badd +56 specification.bnf
badd +3 src/main.rs
argglobal
%argdel
set stal=2
edit src/parse_helper.rs
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
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
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
let s:l = 17 - ((16 * winheight(0) + 40) / 81)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
17
normal! 09|
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
let s:l = 84 - ((0 * winheight(0) + 40) / 81)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
84
normal! 05|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
tabedit src/parse_expr.rs
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
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe '2resize ' . ((&lines * 33 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
exe '3resize ' . ((&lines * 46 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 147 + 98) / 196)
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
let s:l = 68 - ((67 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
68
normal! 09|
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
let s:l = 561 - ((16 * winheight(0) + 16) / 33)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
561
normal! 012|
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
let s:l = 291 - ((5 * winheight(0) + 23) / 46)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
291
normal! 019|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe '2resize ' . ((&lines * 33 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
exe '3resize ' . ((&lines * 46 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 147 + 98) / 196)
tabedit src/parse_expr.rs
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
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe '2resize ' . ((&lines * 39 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
exe '3resize ' . ((&lines * 40 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 147 + 98) / 196)
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
normal! 044|
wincmd w
argglobal
if bufexists("specification.bnf") | buffer specification.bnf | else | edit specification.bnf | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 56 - ((18 * winheight(0) + 19) / 39)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
56
normal! 023|
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
let s:l = 313 - ((18 * winheight(0) + 20) / 40)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
313
normal! 021|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe '2resize ' . ((&lines * 39 + 28) / 56)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
exe '3resize ' . ((&lines * 40 + 28) / 56)
exe 'vert 3resize ' . ((&columns * 147 + 98) / 196)
tabedit src/ast.rs
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
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 48 + 98) / 196)
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
let s:l = 350 - ((1 * winheight(0) + 26) / 53)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
350
normal! 03|
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
let s:l = 75 - ((29 * winheight(0) + 26) / 53)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
75
normal! 040|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 48 + 98) / 196)
tabedit src/ast.rs
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
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
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
let s:l = 307 - ((43 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
307
normal! 063|
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
let s:l = 538 - ((55 * winheight(0) + 40) / 80)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
538
normal! 05|
wincmd w
exe 'vert 1resize ' . ((&columns * 147 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 147 + 98) / 196)
tabnew
set splitbelow splitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
argglobal
enew
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
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

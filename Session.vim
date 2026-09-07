let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
doautoall SessionLoadPre
silent only
silent tabonly
cd ~/Projects/tm-ghci
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
set shortmess+=aoO
badd +1 Session.vim
badd +1 Makefile
badd +1 CHANGELOG.md
badd +26 ghci-interface/package.yaml
badd +4 ghci-interface/src/Main.hs
badd +17 ghci-interface/src/TeXmacs/Data/ControlCharacters.hs
badd +20 ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
badd +50 ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
badd +12 GPL-3.0-LICENSE.txt
badd +1 ghci-interface/src/TeXmacs/Control/IOLoop.hs
badd +60 ghci-interface/src/TeXmacs/Control/IO.hs
badd +52 ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
badd +4 ghci-interface/src/TeXmacs/Data/String/Utils.hs
badd +2 ~/.local/bin/texplug
badd +234 ghci-interface/src/TeXmacs/Control/IO-Boneyard.hs
badd +41 readme/Makefile
argglobal
%argdel
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabrewind
edit Makefile
argglobal
balt Session.vim
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=1
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 1 - ((0 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
lcd ~/Projects/tm-ghci
tabnext
edit ~/Projects/tm-ghci/ghci-interface/package.yaml
argglobal
balt ~/Projects/tm-ghci/Makefile
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=2
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 26 - ((25 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 26
normal! 012|
lcd ~/Projects/tm-ghci/ghci-interface
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/Main.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/package.yaml
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 8 - ((7 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 8
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/Main.hs
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 20 - ((19 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 20
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 33 - ((32 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 33
normal! 041|
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 4 - ((3 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 4
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 52 - ((51 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 52
normal! 063|
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IOLoop.hs
setlocal foldmethod=manual
setlocal foldexpr=0
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
silent! normal! zE
let &fdl = &fdl
let s:l = 60 - ((47 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 60
normal! 026|
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

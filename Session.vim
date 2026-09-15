let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
doautoall SessionLoadPre
silent only
silent tabonly
cd ~/Projects/tm-ghci/ghci-interface/src/Plugin/Control
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
set shortmess+=aoO
badd +1 ~/Projects/tm-ghci/Session.vim
badd +197 ~/Projects/tm-ghci/Makefile
badd +1 ~/Projects/tm-ghci/CHANGELOG.md
badd +26 ~/Projects/tm-ghci/ghci-interface/package.yaml
badd +46 ~/Projects/tm-ghci/ghci-interface/src/Main.hs
badd +17 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/ControlCharacters.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
badd +12 ~/Projects/tm-ghci/GPL-3.0-LICENSE.txt
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IOLoop.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils.hs
badd +2 ~/.local/bin/texplug
badd +234 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO-Boneyard.hs
badd +41 ~/Projects/tm-ghci/readme/Makefile
badd +3 ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO/Types.hs
badd +61 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Types.hs
badd +38 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Reading.hs
badd +1 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Processing.hs
badd +1 Bridging.hs
badd +5 IO/Bridging.hs
badd +2 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
badd +3 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils/Parsers.hs
badd +2 ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils.hs
argglobal
%argdel
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabrewind
edit ~/Projects/tm-ghci/Makefile
argglobal
balt ~/Projects/tm-ghci/Session.vim
setlocal foldmethod=expr
setlocal foldexpr=v:lua.vim.treesitter.foldexpr()
setlocal foldmarker={{{,}}}
setlocal foldignore=#
setlocal foldlevel=1
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 197 - ((76 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 197
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
setlocal foldlevel=3
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldenable
let s:l = 40 - ((39 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 40
normal! 03|
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
let s:l = 44 - ((43 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 44
normal! 041|
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
let s:l = 8 - ((7 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 8
normal! 023|
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
let s:l = 2 - ((1 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 2
normal! 05|
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils.hs
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
let s:l = 2 - ((1 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 2
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils/Parsers.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
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
let s:l = 3 - ((2 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 3
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/Plugin/Control/IO/Bridging.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/Plugin/Control/Bridging.hs
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
let s:l = 26 - ((25 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 26
normal! 026|
lcd ~/Projects/tm-ghci/ghci-interface/src/Plugin/Control/IO
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Types.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO.hs
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
let s:l = 41 - ((40 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 41
normal! 052|
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Reading.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Types.hs
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
let s:l = 26 - ((25 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 26
normal! 025|
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Processing.hs
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
let s:l = 2 - ((1 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 2
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output
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
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :

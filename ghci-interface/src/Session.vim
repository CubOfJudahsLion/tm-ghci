let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
doautoall SessionLoadPre
silent only
silent tabonly
cd ~
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
set shortmess+=aoO
badd +1 Projects/tm-ghci/Session.vim
badd +48 Projects/tm-ghci/Makefile
badd +1 Projects/tm-ghci/CHANGELOG.md
badd +37 Projects/tm-ghci/ghci-interface/package.yaml
badd +1 Main.hs
badd +17 TeXmacs/Data/ControlCharacters.hs
badd +10 TeXmacs/Data/Char/ControlCharacters.hs
badd +1 TeXmacs/Data/String/MessageFormatting.hs
badd +12 Projects/tm-ghci/GPL-3.0-LICENSE.txt
badd +1 TeXmacs/Control/IOLoop.hs
badd +1 TeXmacs/Control/IO.hs
badd +1 TeXmacs/Data/String/Utils/Parsers.hs
badd +1 TeXmacs/Data/String/Utils.hs
badd +2 .local/bin/texplug
badd +234 TeXmacs/Control/IO-Boneyard.hs
badd +41 Projects/tm-ghci/readme/Makefile
badd +3 TeXmacs/Control/IO/Types.hs
badd +10 GHCi/Control/IO/Types.hs
badd +39 GHCi/Control/IO/Output/Reading.hs
badd +1 GHCi/Control/IO/Output/Processing.hs
badd +1 Plugin/Control/Bridging.hs
badd +1 Plugin/Control/IO/Bridging.hs
badd +3 GHCi/Control/IO/Output/Sequencing.hs
badd +20 GHCi/Data/String/Utils/Parsers.hs
badd +35 GHCi/Data/String/Utils.hs
badd +36 System/IO/StrictImmediate.hs
badd +14 TmGHCi/Control/IO/Bridging.hs
badd +1 Bridging.hs
badd +29 IO/Bridging.hs
badd +1 Session.vim
badd +18 Projects/tm-ghci/ghci-interface/src/System/IO/StrictImmediate.hs
badd +29 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
badd +5 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
badd +32 Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils.hs
badd +0 Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils/Parsers.hs
badd +10 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Types.hs
badd +39 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Reading.hs
badd +0 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
badd +0 Projects/tm-ghci/ghci-interface/src/TmGHCi/Control/IO/Bridging.hs
badd +4 Projects/tm-ghci/ghci-interface/src/Main.hs
badd +0 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils.hs
badd +0 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
badd +0 Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO.hs
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
tabrewind
edit Projects/tm-ghci/ghci-interface/src/Main.hs
argglobal
balt Main.hs
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
lcd ~/Projects/tm-ghci/ghci-interface/src
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/System/IO/StrictImmediate.hs
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
let s:l = 18 - ((17 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 18
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
let s:l = 29 - ((28 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 29
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
let s:l = 5 - ((4 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 5
normal! 0
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
let s:l = 32 - ((31 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 32
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
let s:l = 20 - ((19 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 20
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils
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
let s:l = 10 - ((9 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 10
normal! 063|
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
let s:l = 39 - ((38 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 39
normal! 065|
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
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
let s:l = 3 - ((2 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 3
normal! 079|
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TmGHCi/Control/IO/Bridging.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
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
let s:l = 1 - ((0 * winheight(0) + 39) / 79)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TmGHCi/Control/IO
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

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
badd +1 Projects/tm-ghci/ghci-interface/src/System/IO/StrictImmediate.hs
badd +33 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char/ControlCharacters.hs
badd +58 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/MessageFormatting.hs
badd +34 Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils.hs
badd +18 Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils/Parsers.hs
badd +33 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Types.hs
badd +4 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Reading.hs
badd +27 Projects/tm-ghci/ghci-interface/src/GHCi/Control/IO/Output/Sequencing.hs
badd +47 Projects/tm-ghci/ghci-interface/src/TmGHCi/Control/IO/Bridging.hs
badd +22 Projects/tm-ghci/ghci-interface/src/Main.hs
badd +1 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils.hs
badd +1 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Utils/Parsers.hs
badd +1 Projects/tm-ghci/ghci-interface/src/TeXmacs/Control/IO.hs
badd +42 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Escaping.hs
badd +2 Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Formatting.hs
badd +21 Projects/tm-ghci/ghci-interface/src/System/IO/Strict.hs
badd +32 Projects/tm-ghci/ghci-interface/src/Common/Control/IO/Types.hs
badd +58 Projects/tm-ghci/ghci-interface/src/Common/Control/IO/Classes.hs
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
let s:l = 22 - ((21 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 22
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/System/IO/Strict.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/System/IO/StrictImmediate.hs
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
let s:l = 21 - ((20 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 21
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/System/IO
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/Common/Control/IO/Types.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/System/IO/Strict.hs
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
let s:l = 55 - ((54 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 55
normal! 045|
lcd ~/Projects/tm-ghci/ghci-interface/src/Common/Control/IO
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/Common/Control/IO/Classes.hs
argglobal
balt ~/Projects/tm-ghci/ghci-interface/src/Common/Control/IO/Types.hs
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
let s:l = 58 - ((48 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 58
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/Common/Control/IO
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
let s:l = 33 - ((32 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 33
normal! 0
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/Char
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Escaping.hs
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
let s:l = 42 - ((41 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 42
normal! 038|
lcd ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String
tabnext
edit ~/Projects/tm-ghci/ghci-interface/src/TeXmacs/Data/String/Formatting.hs
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
let s:l = 2 - ((1 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 2
normal! 039|
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
let s:l = 34 - ((33 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 34
normal! 031|
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
let s:l = 18 - ((17 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 18
normal! 027|
lcd ~/Projects/tm-ghci/ghci-interface/src/GHCi/Data/String/Utils
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
let s:l = 33 - ((32 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 33
normal! 0
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
let s:l = 4 - ((0 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 4
normal! 059|
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
let s:l = 27 - ((26 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 27
normal! 0
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
let s:l = 18 - ((17 * winheight(0) + 33) / 66)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 18
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

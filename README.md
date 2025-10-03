# <span style="font-family: monospace">tm-ghci &#x2237; <img src="readme-src/haskell.png" width="36" height="32"> &#x2192; <img src="readme-src/texmacs.png" width="36" height="32"></span>
A
[<img src="readme-src/texmacs-text.png" width="60" height="14" alt="image" />](https://texmacs.org/)
plugin for running
[<span style="font-variant: small-caps">GHCi</span>](https://wiki.haskell.org/GHC/GHCi)
sessions.

## Installing

### From release files

The release archives contain statically linked files, which makes it
possible to interact with different versions of
<span style="font-variant: small-caps">GHCi</span>.

To install, uncompress the archive for your
<span style="font-variant: small-caps">OS</span> at `$TEXMACS_PATH/plugins/` (usually
`/usr/share/TeXmacs/plugins/` in <span style="font-variant: small-caps">Linux</span>,
*\<<span style="font-family: serif">TeXmacs-install-dir</span>\>*`\ plugins\` in
<span style="font-variant: small-caps">Windows</span>) or `$TEXMACS_HOME_PATH/plugins/`
(`$HOME/.TeXmacs/plugins/` in <span style="font-variant: small-caps">Linux</span> or
`%UserProfile%\AppData\ Roaming\TeXmacs\plugins\` in
<span style="font-variant: small-caps">Windows</span>.)

`ghci` (or `ghci.bin` or `ghci.exe`, depending on your system) needs to
be in the system `PATH` for the plugin to work.

The plugin is rather simple — mostly forwarding input and output. The
resulting interaction and evaluation facilities are all
<img src="readme-src/texmacs-text.png" width="60" height="14" alt="image" />’s.

### Building and installing from source

Building the plugin requires a `bash`-style shell and a
<span style="font-variant: small-caps">Haskell</span> distribution
(<span style="font-variant: small-caps">GHC</span> 9.2.5 or later.) In
<span style="font-variant: small-caps">Windows</span>, this means installing
<span style="font-variant: small-caps">Haskell</span> through
<span style="font-variant: small-caps">[GHCup](https://www.haskell.org/ghcup/)</span>
and setting up an <span style="font-variant: small-caps">Msys2</span> directory, then
installing the development packages so that `make`, `strip` and other
required utilities will be available.

Installing is simple. In your shell prompt, just `cd` to the `ghci`
folder and type:

```shell
    make deploy
```

Which takes care of compiling and installing the plug-in in the
appropriate directory (`$TEXMACS_HOME_PATH/plugins`.)

While this should be sufficient, this software is still in alpha phase,
and it’s only been tested in <span style="font-variant: small-caps">Windows</span> 10
and <span style="font-variant: small-caps">Arch Linux</span>. I don’t own a
<span style="font-variant: small-caps">MacOS</span> system, so I’m unable to provide
the respective package.

## Features and Limitations

`tm-ghci` is *alpha*-stage software, distributed *as-is*.

Presently, it’s able to run a vanilla `ghci` session, i.e., without
project dependencies autoloaded as with `stack repl` or `cabal repl`.

## Licensing

`tm-ghci` is distributed under the
[<span style="font-variant: small-caps">GPL3</span>](https://www.gnu.org/licenses/gpl-3.0.en.html)
license.

## Issues

Please [submit an
issue](https://github.com/CubOfJudahsLion/tm-ghci/issues) if you find
one. Bug reports must include the steps required to reproduce the error.

You can also send any feedback to *10951848+~~nope~~CubOfJudahsLion*
ä$`\tau`$ *users.noreply.github.com*.

## Thanks to

- *The TeXmacs developers* for giving us such a great tool.

- *Massimiliano Gubinelli* for setting me straight on the help file
  mechanism for plug-ins.

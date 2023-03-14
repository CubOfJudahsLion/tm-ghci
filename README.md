# <table style="border: 0; border-style: hidden !important; padding: 0; margin: 0; border-collapse: collapse"><tr><td><img src="./texmacs.png"></td><td>&nbsp;&#183;&nbsp;</td><td><img src="./haskell.png"></td><td>&nbsp;tm-ghci</td></tr></table>

A [TeXmacs](https://texmacs.org/) plugin for running [GHCi](https://wiki.haskell.org/GHC/GHCi) sessions.


## Installing from release files

The executable binaries are compiled with GHC 9.2.5. The Windows binary
is considerably larger due to the lack of dynamic libraries in Windows
Haskell distributions.

To install, uncompress the archive for your OS at `$TEXMACS_PATH/plugins/`
(usually `/usr/share/TeXmacs/plugins/` in Linux,
_<TeXmacs_install_dir>_`\plugins\` in Windows) or
`$TEXMACS_HOME_PATH/plugins/` (`$HOME/.TeXmacs/plugins/` in
Linux, `%UserProfile%\AppData\Roaming\TeXmacs\plugins\` in Windows.)

`ghci` needs to be in the system `PATH` for the plugin to work.

The plugin is rather simple -- mostly forwarding input and output -- and
the resulting interaction and evaluation facilities are all TeXmacs's.


## Building and installing from source

Building the plugin requires a _bash_-style shell and a Haskell [distribution](https://www.haskell.org/downloads/).
In Windows, this means using [MSYS2](https://www.msys2.org/). Installing Haskell
through [ghcup](https://www.haskell.org/ghcup/install/) allows setting up a
MSYS2 directory (recommended) and installing the `mingw64` toolchain so that
`make`, `strip` and other required utilities will be available.

Installing is straightford. Just `cd` to the `ghci` folder and write:

    make deploy

Which takes care of compiling and installing the plug-in in the appropriate
directory (`$TEXMACS_HOME_PATH/plugins`.)

While this should be sufficient, this software is still in alpha phase, and
it's only been tested in Windows 10 and Arch Linux. I don't own a MacOS
system, so I'm unable to provide the respective version.


## Limitations

`tm_ghci` is _alpha_-stage software.

Presently, it's able to run a vanilla `ghci` session, i.e.,
without project dependencies autoloaded as with `stack repl` or `cabal repl`.


## Licensing

`tm-ghci` is distributed under the [MIT License](https://mit-license.org/).
See the [LICENSE.txt](./LICENSE.txt) file for details.


## Bug reports and such

Send feedback to 10951848+<s>nope</s>CubOfJudahsLion äτ users.noreply.github.com.
Bug reports must include the steps required to reproduce the error.

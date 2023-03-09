# tm-ghci

A [TeXmacs](https://texmacs.org/) plugin for running [GHCi](https://wiki.haskell.org/GHC/GHCi) sessions.


## Installing from release files

The executable binaries are compiled with GHC 9.2.5. The Windows binary
is considerably larger due to the lack of dynamic libraries in Windows
Haskell distributions.

To install, uncompress the archive for your OS at `$TEXMACS_PATH/plugins/`
(usually `/usr/share/TeXmacs` in Linux, _<TeXmacs_install_dir>_`\plugins\` in
Windows) or `$TEXMACS_HOME_PATH/plugins/` (`$HOME/.TeXmacs/plugins/` in
Linux, `%UserProfile%\AppData\Roaming\TeXmacs\plugins\` in Windows.)

The plugin is rather simple -- mostly forwarding input and output -- and
the resulting interaction and evaluation facilities are all TeXmacs's.

## Buiiding and installing from source

Building the plugin requires a _bash_-style shell and a Haskell [distribution](https://www.haskell.org/downloads/).
In Windows, this means using [MSYS2](https://www.msys2.org/). Installing Haskell
through [ghcup](https://www.haskell.org/ghcup/install/) allows setting up a
MSYS2 directory (recommended) and installing the `mingw64` toolchain so that
`make`, `strip` and other required utilities will be available.

Installing is straightford. Just `cd` to the `ghci` folder and write:

    make deploy

Which takes care of compiling and installing the plug-in in the appropriate
directory (`$TEXMACS_HOME_PATH`.)

While this should be sufficient, this software is still in alpha phase, and
it's only been tested in Windows 10 and Arch Linux. 

## Limitations

Currently, `tm_ghci` is at an _alpha_ stage.

At this point, it's only able to run a vanilla `ghci` session, i.e.,
without project dependencies autoloaded as with `stack repl` or `cabal repl`. No
autocompletion, syntax coloring or styling. The official feature set is
still undecided.

## Disclaimer

This software is distributed _as-is_. No guarantees of usability or fitness
for purpose, support or platform availability are offered or implied. The
author shall not be liable for any damages resulting, directly or indirectly,
from the use of this software.

By using the software you agree to the terms set by this disclaimer.

## Licensing

`tm-ghci` is distributed under the [MIT License](https://mit-license.org/).
See the [LICENSE.txt](./LICENSE.txt) file for details.

## Bug reports and such

Send all feedback to [`10951848+CubOfJudahsLion at users.noreply.github.com`](mailto:10951848+CubOfJudahsLion@users.noreply.github.com)

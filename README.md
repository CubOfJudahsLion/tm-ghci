# <table style="border: 0px; padding: 0; margin: 0; border: 0; border-collapse: collapse !important"><tr><td>tm-ghci &#x2237;&nbsp;</td><td><img src="./haskell.png"></td><td>&nbsp;&#x2192;&nbsp;</td><td><img src="./texmacs.png"></td></tr></table>

A [TeXmacs](https://texmacs.org/) plugin for running [GHCi](https://wiki.haskell.org/GHC/GHCi) sessions.


## Installing

### From release files

The release archives contain statically linked files, which makes it
possible to interact with different versions of GHCi.

To install, uncompress the archive for your OS at `$TEXMACS_PATH/plugins/`
(usually `/usr/share/TeXmacs/plugins/` in Linux,
_<TeXmacs_install_dir>_`\plugins\` in Windows) or
`$TEXMACS_HOME_PATH/plugins/` (`$HOME/.TeXmacs/plugins/` in
Linux, `%UserProfile%\AppData\Roaming\TeXmacs\plugins\` in Windows.)

`ghci` needs to be in the system `PATH` for the plugin to work.

The plugin is rather simple -- mostly forwarding input and output.
the resulting interaction and evaluation facilities are all TeXmacs's.


### Building and installing from source

Building the plugin requires a _bash_-style shell and a Haskell
distribution (GHC 9.2.5 or later.) In Windows, this means installing
Haskell through [ghcup](https://www.haskell.org/ghcup/) and setting up
an MSYS2 directory, then installing the development packages so that
`make`, `strip` and other required utilities will be available.

Installing is simple. Just `cd` to the `ghci` folder (inside `tm-ghci`)
and write:
```bash
    make deploy
```
Which takes care of compiling and installing the plug-in in the appropriate
directory (`$TEXMACS_HOME_PATH/plugins`.)

While this should be sufficient, this software is still in alpha phase, and
it's only been tested in Windows 10 and Arch Linux. I don't own a MacOS
system, so I'm unable to provide the respective version.


## Features and Limitations

`tm-ghci` is _alpha_-stage software.

Presently, it's able to run a vanilla `ghci` session, i.e.,
without project dependencies autoloaded as with `stack repl` or `cabal repl`.


## Licensing

`tm-ghci` is distributed under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.


## Issues

Send feedback to 10951848+<s>nope</s>CubOfJudahsLion äτ users.noreply.github.com.
Bug reports must include the steps required to reproduce the error.


## Thanks to

* _The TeXmacs developers_ for giving us such a magnificent tool.
* _Massimiliano Gubinelli_ for setting me straight on the help file mechanism
  for plug-ins.


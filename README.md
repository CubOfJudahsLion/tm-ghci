# tm-ghci = ![TeXmacs Logo](./texmacs.png)&nbsp;&#183;&nbsp;![Haskell logo](./haskell.png)

A [TeXmacs](https://texmacs.org/) plugin for running [GHCi](https://wiki.haskell.org/GHC/GHCi) sessions.


## Installing
### From release files

Last release binaries are compiled with GHC 9.2.8, and they can be
obtained from the [releases](https://github.com/CubOfJudahsLion/tm-ghci/releases)
page. The Windows binary is considerably larger due to the lack of dynamic
libraries in Haskell distributions for Windows.

To install, uncompress the archive for your OS at `$TEXMACS_PATH/plugins/`
(usually `/usr/share/TeXmacs/plugins/` in Linux,
_<TeXmacs_install_dir>_`\plugins\` in Windows) or
`$TEXMACS_HOME_PATH/plugins/` (`$HOME/.TeXmacs/plugins/` in
Linux, `%UserProfile%\AppData\Roaming\TeXmacs\plugins\` in Windows.)

`ghci` needs to be in the system `$PATH` for the plugin to work.

The plugin itself is simple -- mostly forwarding input and output. The
resulting interaction and evaluation features are all TeXmacs's.


### Building and installing from source

Building the plugin requires a _bash_-style shell and a Haskell
[distribution](https://www.haskell.org/downloads/). In Windows, this means
using [MSYS2](https://www.msys2.org/). Installing through
[ghcup](https://www.haskell.org/ghcup/install/) allows setting up a MSys2
distribution for your Haskell setup alone (recommended) and installing the
development tools so that `make`, `strip` and other required utilities are
available.

Once this is done, type:

    make deploy

which takes care of compiling and installing the plug-in in the appropriate
directory (`$TEXMACS_HOME_PATH/plugins`.)

While this should be sufficient, this software is still in beta phase.
Please report any mishaps to the email address below.


## Features and Limitations

The primary function of the plugin is to pass TeXmacs's input to GHCi and
then report back. That's it. However, that's enough for TeXmacs to do
plenty, beyond providing just a better-typeset REPL: background evaluation,
referrable expression cells -- even spreadsheets built out of Haskell/GHCi
expressions.

For this version, I decided to add all of the functionality I'd want if I
were to get serious about writing and documenting an extensive Haskell
project. These new features will be added: project awareness (i.e., tm-ghci
will run a Cabal or Stack REPL using project assets), tab-completion,
importing syntax-highlighted Haskell code and making Haskell/GHCi a 
_scripting_ language.

Those new features imply some refactoring. The minimalistically didactical
single-file project simply  _has_ to go away. Nevertheless I'm hoping proper
module organization will still convey that impression for each individual
feature, as the features are, in my opinion, worth having.

`tm_ghci` is _beta_-stage software. It's only been tested in Windows 10 and
Arch Linux. I don't own a MacOS system, so I'm unable to provide the
respective version. If anyone is willing to volunteer to compile and test,
please drop me a line.


## Licensing

`tm-ghci` is distributed under the [MIT License](https://mit-license.org/).
See the [LICENSE](./LICENSE) file for details.


## Bug reports and such

* Send feedback to 10951848+<s>nope</s>CubOfJudahsLion äτ users.noreply.github.com.
* Bug reports must include the steps required to reproduce the error.


## Thanks to

* _The TeXmacs developers_ for giving us such a magnificent tool.
* _Massimiliano Gubinelli_ for setting me straight on the plugin help file mechanism.

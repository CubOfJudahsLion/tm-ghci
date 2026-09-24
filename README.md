# `tm-ghci` :: ![Haskell](img/haskell.png) &rarr; ![TeXmacs](img/texmacs.png)

A [TeXmacs](https://texmacs.org) plugin for running
[GHCi](https://wiki.haskell.org/index.php?title=GHC/GHCi)
sessions.

## Installing

### From release files

The release archives contain statically linked files, which makes it
possible to interact with different versions of
GHCi.

To install, uncompress the archive for your
OS at `$TEXMACS_PATH/plugins/` (usually
`/usr/share/TeXmacs/plugins/` in Linux,
*`<TeXmacs_install_dir>`*`\ plugins\` in
Windows) or `$TEXMACS_HOME_PATH/plugins/`
(`$HOME/.TeXmacs/plugins/` in Linux,
`%UserProfile%\AppData\ Roaming\TeXmacs\plugins\` in
Windows.)

`ghci` needs to be in your `PATH` for the plugin to work.

The plugin is comparatively simple, as its job is mostly forwarding
input and output. The resulting interaction and evaluation facilities
are all TeXmacs’s.

### Building and installing from source

Building the plugin requires a *`bash`*-style shell and a
[Haskell](https://haskell.org)
distribution (GHC 9.10.3 or later.) In
Windows, this means installing
Haskell through
[`ghcup`](https://www.haskell.org/ghcup/) and setting up an
[MSys2](https://www.msys2.org/)
directory, then installing the development packages so that `make`,
`strip` and other required utilities will be available. `git` is also
required for cloning the repository, which is our first step:

    git clone https://github.com/cubofjudahslion/tm-ghci

This will create a folder named `tm-ghci` right under your current one.
To install just write

    cd tm-ghci/ghci
    make deploy

Which takes care of compiling and installing the plug-in in the
appropriate directory (`$TEXMACS_HOME_PATH/plugins`.)

## Features and Limitations

`tm-ghci` is *alpha*-stage software. It’s only been tested in
Windows 10 and
Arch Linux. I don’t own a
MacOS system, so I’m unable to provide
the respective version.

Presently, it’s able to run a vanilla `ghci` session, i.e., without
project dependencies autoloaded (as `stack repl` or `cabal repl` would
do.)

## Licensing

`tm-ghci` is distributed under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html)
license.

## AI Policy

Research has shown that overreliance on [AI causes cognitive
deterioration](https://news.harvard.edu/gazette/story/2025/11/is-ai-dulling-our-minds/)
— all while [failing to improve
productivity](https://futurism.com/artificial-intelligence/ai-failing-boost-productivity)
in a perceivably meaningful way, and it’s certainly not helping [code
quality](https://www.reversinglabs.com/blog/software-quality-collapse-ai-accelerate),
[technical
debt](https://sloanreview.mit.edu/article/how-to-manage-tech-debt-in-the-ai-era/)
or — one would think this would tip the scales —
[finances](https://fortune.com/article/why-is-the-cost-of-ai-higher-than-human-workers-nvidia-executive/).
This project is thus AI-free for the benefit of both the user and the
author.

## Issues

Use the [issues page](https://github.com/CubOfJudahsLion/tm-ghci/issues)
to report erroneous behavior. Please include platform data (i.e.,
OS/distribution, hardware), a clear description of the error condition,
and the steps required to reproduce it.

## Thanks to

- *The TeXmacs developers* for giving us such a magnificent tool.

- *Massimiliano Gubinelli* for setting me
  straight on the help file mechanism for plug-ins.

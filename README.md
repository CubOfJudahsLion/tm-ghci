# tm-ghci

A TeXmacs plugin for running GHCi sessions.

## Setup

1.  Copy the `ghci` folder to `$TEXMACS_PATH/plugins` or `$TEXMACS_HOME_PATH/plugins`
    (the latter is usually equivalent to `$HOME/.TeXmacs`.)

2.  `cd` to the `ghci/src` directory and run:

        make

    While this should be sufficient, this software is still in pre-alpha phase, and
    it's only been tested in x86_64 Windows and Arch Linux.

## Limitations

At this point, *tm_ghci* is only able to run a vanilla `GHCi` session, i.e.,
without project dependencies autoloaded as with `stack repl` or `cabal repl`.

## Gripes

Send all feedback to [`10951848+CubOfJudahsLion@users.noreply.github.com`](mailto:10951848+CubOfJudahsLion@users.noreply.github.com)

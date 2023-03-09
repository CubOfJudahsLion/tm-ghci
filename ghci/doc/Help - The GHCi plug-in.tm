<TeXmacs|2.1.1>

<style|tmdoc>

<\body>
  <tmdoc-title|The GHCi plug-in>

  This is a plug-in for interactive <name|GHCi> sessions in <TeXmacs>. Use
  menu option <samp|Insert<math|\<rightarrow\>>Session<math|\<rightarrow\>>GHCi>,
  and it will display the <name|GHCi> version notice and prompt:

  <\session|ghci|default>
    <\output>
      GHCi, version 9.2.5: https://www.haskell.org/ghc/ \ :? for help
    </output>

    <\unfolded-io>
      ghci\<gtr\>\ 
    <|unfolded-io>
      putStrLn "Hello, TeXmacs!"
    <|unfolded-io>
      Hello, TeXmacs!
    </unfolded-io>

    <\input>
      ghci\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  Multiple-line commands (i.e., those surrounded by <verbatim|:{> and
  <verbatim|:}>) are processed correctly. <TeXmacs> provides additional
  functionality to that of a REPL, allowing re-editing of cells and
  re-evaluation of single and multiple cells; refer to the <TeXmacs>
  documentation for further information. To finish, use the right-click
  context menu and choose <samp|Close Session>.

  There's also a one-liner evaluation facility, available through
  <samp|Insert<math|\<rightarrow\>>Fold<math|\<rightarrow\>>Executable<math|\<rightarrow\>>GHCi>.
  Here it's used to calculate the <em|machine epsilon> for the
  <verbatim|Float> type:

  <script-input|ghci|default|head $ take 1 $ dropWhile ((/= 0) . (/ 2)) $
  iterate (/ 2) (1 :: Float)|1.0e-45>

  Place the cursor inside the light-yellow box and hit <key|Enter>. After a
  moment it produces the desired result. Hit <key|Enter> again to see the
  source expression once more and edit it as needed.

  \;

  <paragraph|Feedback:><verbatim|10951848+CubOfJudahsLion> <em|at>
  <verbatim|users.noreply.github.com>.

  The <name|GitHub> repo is <hlink|CubOfJudahsLion/tm-ghci|https://github.com/CubOfJudahsLion/tm-ghci>.

  \;
</body>

<\initial>
  <\collection>
    <associate|page-height|auto>
    <associate|page-medium|paper>
    <associate|page-type|letter>
    <associate|page-width|auto>
    <associate|preamble|false>
  </collection>
</initial>
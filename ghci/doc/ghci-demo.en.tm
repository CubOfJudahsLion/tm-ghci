<TeXmacs|2.1.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Usage and Example>

  Select the menu option <menu|Insert|Session|GHCi>, and <TeXmacs> will
  display the <name|GHCi> version notice and prompt:

  <\session|ghci|default>
    <\output>
      GHCi, version 9.10.3: https://www.haskell.org/ghc/ \ :? for help
    </output>

    <\unfolded-io>
      ghci\<gtr\>\ 
    <|unfolded-io>
      putStrLn "Hello from TeXmacs"
    <|unfolded-io>
      Hello from TeXmacs
    </unfolded-io>

    <\input>
      ghci\<gtr\>\ 
    <|input>
      :{
    </input>

    <\input>
      ghci\|\ 
    <|input>
      powerset :: [a] -\<gtr\> [[a]]
    </input>

    <\input>
      ghci\|\ 
    <|input>
      powerset [] = [[]]
    </input>

    <\input>
      ghci\|\ 
    <|input>
      powerset (x:xs) = let pset = powerset xs in fmap (x:) pset ++ pset
    </input>

    <\input>
      ghci\|\ 
    <|input>
      :}
    </input>

    <\unfolded-io>
      ghci\<gtr\>\ 
    <|unfolded-io>
      powerset [1,2,3,4]
    <|unfolded-io>
      [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],[1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
    </unfolded-io>

    <\input>
      ghci\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <TeXmacs> provides additional functionality to that of a REPL, allowing
  re-editing of cells and re-evaluation of single and multiple cells; refer
  to the <TeXmacs> documentation for further information. To finish a
  session, right-click on a <name|GHCi> cell and choose <menu|Close Session>
  in the context menu (<verbatim|:quit> commands are currently ignored for
  stability reasons.)

  There's also a single-field evaluation facility, which allows evaluating
  short expressions amidst text sections. It's available through
  <menu|Insert|Fold|Executable|GHCi>. Here it's used to calculate the
  <em|machine epsilon> for the <verbatim|Float> type:

  <script-input|ghci|default|let h = \\x-\<gtr\> let x'=x/2 in if x'==0 then
  x else h x' in h (1::Float)|1.0e-45>

  Place the cursor inside the light-yellow text box above and hit
  <key|Enter>. After a moment, it produces the desired result. Hit
  <key|Enter> again to see the source expression once more and edit it as
  needed.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|preamble|false>
  </collection>
</initial>
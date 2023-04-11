<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Usage and Example>

  Select the menu option <menu|Insert|Session|GHCi>, and <TeXmacs> will
  display the <name|GHCi> version notice and prompt:

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
      :{
    </input>

    <\input>
      ghci\|\ 
    <|input>
      powerSet :: [t] -\<gtr\> [[t]]
    </input>

    <\input>
      ghci\|\ 
    <|input>
      powerSet l = ps [[]] l
    </input>

    <\input>
      ghci\|\ 
    <|input>
      \ \ where
    </input>

    <\input>
      ghci\|\ 
    <|input>
      \ \ \ \ ps :: [[t]] -\<gtr\> [t] -\<gtr\> [[t]]
    </input>

    <\input>
      ghci\|\ 
    <|input>
      \ \ \ \ ps acc [] = acc
    </input>

    <\input>
      ghci\|\ 
    <|input>
      \ \ \ \ ps acc (x:xs) = ps (acc ++ fmap (x:) acc) xs
    </input>

    <\input>
      ghci\|\ 
    <|input>
      :}
    </input>

    <\unfolded-io>
      ghci\<gtr\>\ 
    <|unfolded-io>
      powerSet [1,2,3,4]
    <|unfolded-io>
      [[],[1],[2],[2,1],[3],[3,1],[3,2],[3,2,1],[4],[4,1],[4,2],[4,2,1],[4,3],[4,3,1],[4,3,2],[4,3,2,1]]
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

  <script-input|ghci|default|head $ take 1 $ dropWhile ((/= 0) . (/ 2)) $
  iterate (/ 2) (1 :: Float)|1.0e-45>

  Place the cursor inside the light-yellow text box above and hit
  <key|Enter>. After a moment, it produces the desired result. Hit
  <key|Enter> again to see the source expression once more and edit it as
  needed.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>
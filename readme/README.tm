<TeXmacs|2.1.5>

<style|generic>

<\body>
  <with|font-base-size|20|<center|<verbatim|tm-ghci> ::
  <image|../img/haskell.png||20pt||> <math|\<rightarrow\>>
  <image|../img/texmacs.png||20pt||>>>

  <center|A <hlink|<TeXmacs>|https://texmacs.org/> plugin for running
  <hlink|<name|GHCi>|https://wiki.haskell.org/index.php?title=GHC/GHCi>
  sessions.>

  <section|Installing>

  <subsection|From release files>

  The release archives contain statically linked files, which makes it
  possible to interact with different versions of <name|GHCi>.\ 

  To install, uncompress the archive for your <acronym|OS> at
  <rigid|<verbatim|$TEXMACS_PATH/plugins/>> (usually
  <rigid|<verbatim|/usr/share/TeXmacs/plugins/>> in <name|Linux>,
  <verbatim|<em|\<less\>TeXmacs_install_dir\<gtr\>>\\plugins\\> in
  <name|Windows>) or <rigid|<verbatim|$TEXMACS_HOME_PATH/plugins/>>
  (<rigid|<verbatim|$HOME/.TeXmacs/plugins/>> in <name|Linux>,
  <rigid|<verbatim|%UserProfile%\\AppData\\Roaming\\TeXmacs\\plugins\\>> in
  <name|Windows>.)

  <verbatim|ghci> needs to be in the system <verbatim|PATH> for the plugin to
  work.

  The plugin is rather simple \V mostly forwarding input and output. The
  resulting interaction and evaluation facilities are all <TeXmacs>'s.

  <subsection|Building and installing from source>

  Building the plugin requires a <verbatim|<em|bash>>-style shell and a
  <hlink|<name|Haskell>|https://haskell.org> distribution (<acronym|GHC>
  9.10.3 or later.) In <name|Windows>, this means installing <name|Haskell>
  through <hlink|<verbatim|ghcup>|https://www.haskell.org/ghcup/> and setting
  up an <hlink|<name|MSys2>|https://www.msys2.org/> directory, then
  installing the development packages so that <verbatim|make>,
  <verbatim|strip> and other required utilities will be available.
  <verbatim|git> is also required for cloning the repository, which is our
  first step:

  <\shell-code>
    git clone https://github.com/cubofjudahslion/tm-ghci
  </shell-code>

  This will create a folder named <verbatim|tm-ghci> right under your current
  one. To install just write

  <\shell-code>
    cd tm-ghci/ghci

    make deploy
  </shell-code>

  Which takes care of compiling and installing the plug-in in the appropriate
  directory (<rigid|<verbatim|$TEXMACS_HOME_PATH/plugins>>.)

  <section|Features and Limitations>

  <verbatim|tm-ghci> is <em|alpha>-stage software. It's only been tested in
  <name|Windows> 10 and <name|Arch Linux>. I don't own a <name|MacOS> system,
  so I'm unable to provide the respective version.

  Presently, it's able to run a vanilla <verbatim|ghci> session, i.e.,
  without project dependencies autoloaded (as <verbatim|stack repl> or
  <verbatim|cabal repl> would do.)

  <section|Licensing>

  <verbatim|tm-ghci> is distributed under the
  <hlink|<acronym|GPL-3>|https://www.gnu.org/licenses/gpl-3.0.en.html>
  license.

  <section|Issues>

  Please use the <hlink|issues page|https://github.com/CubOfJudahsLion/tm-ghci/issues>
  to report any inconviences. Bug reports must include the steps required to
  reproduce the error.

  <section|Thanks to>

  <\itemize>
    <item><em|The <TeXmacs> developers> for giving us such a magnificent
    tool.

    <item><person|<em|Massimiliano Gubinelli>> for setting me straight on the
    help file mechanism for plug-ins.
  </itemize>
</body>

<\initial>
  <\collection>
    <associate|page-medium|beamer>
    <associate|par-columns|1>
    <associate|par-first|0tab>
    <associate|par-mode|left>
    <associate|par-par-sep|1fn>
    <associate|par-sep|0.5fn>
    <associate|preamble|false>
    <associate|prog-scripts|scheme>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|2|?>>
    <associate|auto-5|<tuple|3|?>>
    <associate|auto-6|<tuple|4|?>>
    <associate|auto-7|<tuple|5|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Installing>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>From release files
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <with|par-left|<quote|1tab>|1.2<space|2spc>Building and installing from
      source <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Features
      and Limitations> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Licensing>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Issues>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|5<space|2spc>Thanks
      to> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
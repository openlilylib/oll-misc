\version "2.16.2"

\include "oll-core/package.ily"
\loadModule oll-misc.layout.display.grob-names

\layout {
  \context {
    \Voice
    \printGrobNames #debug-grob-name-groblist
  }
}

{
  a'4~ a'\fermata g'( f')
}

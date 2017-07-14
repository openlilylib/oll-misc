\version "2.16.2"

\include "oll-core/package.ily"
\loadModule oll-misc.layout.display.explicit-voices


\relative g' {
  g a b c |
  d \voiceOne c
  \voiceTwo bes8 as \voiceThree g\noBeam r
  \oneVoice
  e f g2.
}

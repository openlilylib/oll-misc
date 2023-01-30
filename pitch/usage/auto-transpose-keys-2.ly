\version "2.24.0"

\include "english.ly"

\include "oll-core/package.ily"
\loadModule oll-misc.pitch.auto-transpose

tenor-sax = {
  \transposition bf,
  <>^\markup\bold "T. Sax."
}

alto-sax = {
  \transposition ef
  <>^\markup\bold "A. Sax."
}

global = {
  \key c \major
  s1
  \key f \major
  s1
}

music = \relative c' {
  \tenor-sax
  r4 c2.
  \alto-sax
  r4 ef2.
}

%%% Simultaneous transposition and key changes output correctly regardless of order, but will trigger a warning
\score {
  <<
    \new Staff << \global \music >>
    \new Staff << \music \global >>
  >>
  \layout {
    \context {
      \Staff
      \autoTranspose
    }
  }
  \midi { \tempo 4=150 }
}
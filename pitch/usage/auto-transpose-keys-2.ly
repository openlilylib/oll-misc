\version "2.20.0"

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
  s1
  \key f \major
  s1
}

music = \relative c' {
  \tenor-sax
  r4 c2.
  \alto-sax
  r4 c2.
}

\markup \column { 
  "Simultaneous key changes and transposition changes will trigger a warning and incorrect key signatures."
  \vspace #1
}

%%% create demo score
\score {
  <<
    \new Staff \with {
      instrumentName = "Incorrect"
    } << \global \music >>
    \new Staff \with {
      instrumentName = "Incorrect"
    } << \music \global >>
  >>
  \layout {
    \context {
      \Staff
      \autoTranspose
    }
  }
  \midi { \tempo 4=150 }
}

global = {
  s1
  \key f \major
  s1
}

music = \relative c' {
  \tenor-sax
  r4 c2.
  \alto-sax
  \key f \major
  r4 c2.
}

\markup \column {
  \wordwrap-string "When transposition changes at the same moment as a key change, add an extra \key command after the transposition change, and be careful about the order variables are referenced. A warning will still be triggered."
  \vspace #1
}

%%% create demo score
\score {
  <<
    \new Staff \with {
      instrumentName = "Incorrect"
    } << \global \music >>
    \new Staff \with {
      instrumentName = "Correct"
    } << \music \global >>
  >>
  \layout {
    \context {
      \Staff
      \autoTranspose
    }
  }
  \midi { \tempo 4=150 }
}
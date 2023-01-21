\version "2.24.0"

\include "english.ly"

\include "oll-core/package.ily"
\loadModule oll-misc.pitch.auto-transpose

%%% create demo score
\score {
  \new Staff \with {
    \autoTranspose
  } \relative c'' {
    \key f \major
    r4 c2. |
    % Key signatures inserted at transposition changes
    \transposition bf
    r4 c2. |
    % Key changes are transposed
    \key c \major
    r4 c2. |
    % Key change immediately after a transposition change works correctly
    \transposition ef
    \key f \major
    r4 c2. |
    % Key change immediately before a transposition change triggers a warning and output is incorrect
    \key bf \major
    \transposition c'
    \textMark "Incorrect key!"
    r4 c2. |
  }
  \layout {}
  \midi { \tempo 4=150 }
}
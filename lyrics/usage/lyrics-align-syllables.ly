\version "2.23.80"

\include "oll-core/package.ily"
\loadModule oll-misc.lyrics.align-syllables

%%%%%%%%%%%%%%%%%%%%%
%   USAGE EXAMPLE   %
%%%%%%%%%%%%%%%%%%%%%

% a shorthand for tagging:


\layout {
  ragged-right = ##f
}

\score {
  \new Staff <<
    \new Voice = A {
      \relative c' {
        c d e2 \bar "|."
      }
    }
    \new Lyrics \lyricsto A {
      \align Do -- mi -- nus,
    }

    \new Lyrics \lyricsto A {
      Cant -- a me
    }
    \new Lyrics \lyricsto A {
      Syll -- a -- bum!
    }
  >>
}

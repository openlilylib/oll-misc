\version "2.23.80"

\include "oll-core/package.ily"
\loadModule oll-misc.custom-elements.interval-brackets

\markup \bold \huge "Interval Brackets"

\relative c' {
  \override Staff.TimeSignature.stencil = ##f
  \time 32/4
  \intervalBracketsOn
  c1\tone
  d\semiTone
  ees\tone
  f\tone
  g\semiTone
  as\threeSemiTone
  b!\semiTone c
  \bar "|."
  \intervalBracketsOff
}

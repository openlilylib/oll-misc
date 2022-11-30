\version "2.23.80"

\include "oll-core/package.ily"
\loadModule oll-misc.layout.display.explicit-directions


%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%

\new Staff {
  c''^(
  d'')
  c''(__
  d'')
  c''_\(
  d''\)
  <c'' e''>^(_\mf
  <d'' f''>)
  c'~ c'^~ c'_~ c'
}


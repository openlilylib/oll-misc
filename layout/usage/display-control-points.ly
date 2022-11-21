\version "2.23.80"

\include "oll-core/package.ily"
\loadModule oll-misc.layout.display.control-points

%%%%%%%%%%%%%%%%%%%%%
% USAGE EXAMPLE(S): %
%%%%%%%%%%%%%%%%%%%%%


\score {
  \new Staff \relative c' {
    c( d e\( d~ d1) g'4 a b f |
    % locally hide and show control points
    \hideControlPoints
    e\) d2 \laissezVibrer r4 |
    \displayControlPoints
    r c2.\repeatTie
  }
  \layout {
    \displayControlPoints
  }
}

% This example show some individual uses of the commands
\relative c' {
  % only show the control points for the following curve
  \once \displayControlPoints
  c1~\ppp c
  a''~^\ppp a
  g,1~ g
  \override Tie.stencil = #(display-control-points)
  g1~ g
  f,\(\ppp d\)
  \override PhrasingSlur.stencil = #(display-control-points)
  g'\(^\ppp e\)
  f,\(\ppp d\)
  g'(^\ppp e)
}
%}

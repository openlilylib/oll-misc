\version "2.16.2"

\include "oll-core/package.ily"
\loadModule oll-misc.custom-elements.markup-stencils

{
  \customStencilFromMarkup "Staff.TimeSignature" \markup \vcenter {
    \musicglyph #"timesig.mensural34"
    \musicglyph #"three"
  }
  \customStencilFromMarkup "NoteHead" \markup \vcenter { lol }
  \customStencilFromMarkup "Dots" \markup \vcenter { * }
  \time 6/1
  \clef "petrucci-f4"
  a\breve. g | f e |
}

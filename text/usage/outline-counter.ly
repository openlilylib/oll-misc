\version "2.19.83"

\include "oll-core/package.ily"
\loadModule snippets.markup.outline-counter

\paper {
  ragged-right = ##t
  indent = 0
  left-margin = 25\mm
  right-margin = 25\mm
  top-margin = 20\mm
  bottom-margin = 20\mm

  top-markup-spacing =
  #'((basic-distance . 8)
     (minimum-distance . 4)
     (padding . 1)
     (stretchability . 12))
}

\header {
  title = \markup \column {
    \vspace #2
    "Outline Counter Markup"
    \vspace #1
  }
  subtitle = "Making outline style lists in lilypond"
  tagline = ##f
}

\markup \vspace #2

\markup {
  \fill-line { \italic "Andrew Bernard" }
}

\markup \vspace #0.5

\markup {
  \fill-line { \italic "April 2019" }
}

\markup \vspace #2

\markup \justify {
  This module provides a way to make numbered lists or outline lists in lilypond markup. It has simple functions to implement outline style lists, as commonly seen in word processing programs. This is useful for numbered musical exercises, for example, or numbered paragraphs in blocks of textual matter. Several counters can be used concurrently if necessary.
}

\markup \vspace #2
\markup \large \bold "Usage"
\markup \vspace #1

\markuplist {
  \justified-lines {
    { \typewriter "\createOutline <name>" }
  }
  \justified-lines {
    { where \typewriter { <name> } is a Scheme symbol identifying the counter. Any number of separate counters may be created, distinguished by name. Note that counter names are symbols not strings. }
  }
  \vspace #0.5
  \justified-lines {
    { In all the following functions the counter name must be specified. }
  }

  \vspace #1
  \justified-lines {
    { \typewriter "\inc <name>" }
  }
  \justified-lines {
    { Returns a markup string with the next counter increment. The first time this function is called the counter starts at 1. }
  }

  \vspace #1
  \justified-lines {
    { \typewriter "\indentOutline <name>" }
  }
  \justified-lines {
    { Indents the outline by one level. }
  }

  \vspace #1
  \justified-lines {
    { \typewriter "\outdentOutline <name>" }
  }
  \justified-lines {
    { Reduces the outline level by one. }
  }

  \vspace #1
  \justified-lines {
    { \typewriter "\resetOutline <name>" }
  }
  \justified-lines {
    { Resets the outline to the first level, commencing again at 1. }
  }

  \vspace #1
  \justified-lines {
    { \typewriter "\resetOutlineLevel <name> <number>" }
  }
  \justified-lines {
    { Resets the outline to the first level, commencing at the given number. }
  }
}

\pageBreak
\markup \vspace #2
\markup \large \bold "Example"
\markup \vspace #1

% create outline counter
\createOutline #'outline

% create a separate one
\createOutline #'outline-b

% demonstrate various usage
\markup { \inc #'outline \bold "Lorem ipsum" }
{ c''1 }
\markup { \inc #'outline \italic "Dolor sit amet" }
{ c''1 }
\markup { \inc #'outline "Consectetur adipiscing elit" }
{ c''1 }
\markup { \inc #'outline-b "one" }
\markup { \inc #'outline-b "two" }
\markup { \inc #'outline-b "three" }
\markup \vspace #2
\markup { \indentOutline #'outline "Integer bibendum" }
{ c''1 }
\markup { \inc #'outline "Sapien non feugiat imperdiet" }
{ c''1 }
\markup { \indentOutline #'outline "Sed a ante ornare" }
{ c''1 }
\markup { \inc #'outline "Sollicitudin augue eget" }
{ c''1 }
\markup { \outdentOutline #'outline "Ultrices enim" }
{ c''1 }
\markup { \resetOutline #'outline "Integer accumsan" }
{ c''1 }
\pageBreak
\markup { \inc #'outline \bold "Lorem ipsum" }
{ c''1 }
\markup { \resetOutlineLevel #'outline #10 "Dolor sit amet" }
{ c''1 }
\markup { \inc #'outline "Consectetur adipiscing elit" }
{ c''1 }
% Text not necessary if not needed
\markup { \inc #'outline }
{ c''1 }


\markup \vspace #1
\markup\large\bold "Author"
\markup \vspace #1
\markup {
  \typewriter "andrew.bernard@gmail.com"
}


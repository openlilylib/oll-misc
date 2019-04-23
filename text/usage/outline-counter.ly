% Outline list counter class usage example. Fnacy version supporting numbers, alphabetic
% and roman numerals.
%
% Andrew Bernard April 2019

\version "2.19.83"

\include "oll-core/package.ily"
%\setLogLevel #'debug
\loadModule oll-misc.text.outline-counter

\paper {
  ragged-right = ##t
  indent = 0
  left-margin = 25\mm
  right-margin = 25\mm
  top-margin = 20\mm
  bottom-margin = 20\mm
  markup-markup-spacing = 20\mm

  top-markup-spacing =
  #'((basic-distance . 8)
     (minimum-distance . 4)
     (padding . 1)
     (stretchability . 12))
}

\header {
  title = \markup \column {
    % \vspace #2
    "Outline Counter Markup"
    \vspace #1
  }
  subtitle = "Making outline style lists in LilyPond"
  tagline = ##f
}

\markup \vspace #1
\markup {
  \fill-line { \italic "Andrew Bernard" }
}
\markup \vspace #0.5
\markup {
  \fill-line { \italic "April 2019" }
}
\markup \vspace #2
\markup \justify {
  This module provides a way to make numbered lists or outline lists in LilyPond markup. It has simple functions to implement outline style lists, as commonly seen in word processing programs. This is useful for numbered musical exercises, for example, or numbered and itemised paragraphs in blocks of textual matter. Several independent counters can be used concurrently if necessary. At outline list creation time, the list style is defined, which can be a combination of numeric, alphabetic, uppercase or lowercase, and roman numerals, also uppercase or lowercase. Arbitrary depth levels are supported. The list items can be intermingled with other markup in a natural way.
}

\markup \vspace #2
\markup \large \bold "Usage"
\markup \vspace #1
\markuplist {
  \justified-lines {
    { In all the following functions the counter name must be given. }
  }
  \vspace #0.5
  \justified-lines {
    { \typewriter "\createOutline <name> <styles-list>" }
  }
  \justified-lines {
    { Create a new outline numbered list. The argument \typewriter { <name> } is a Scheme symbol identifying the counter. The argument \typewriter {<styles list> } is a Scheme list of style names for each level. Any number of separate counters may be created, distinguished by name. Note that counter names are symbols not strings. }
  }
  \vspace #1
  \justified-lines {
    { Number styles and alphabetic styles, uppercase and lowercase, and roman numerals, uppercase and lowercase are supported. For roman numerals, the subtractive style is used. }
  }
  \vspace #1
  \column {
    {
      "Style names to be used are:"
      \typewriter {
        "number alpha-upper alpha-lower roman-upper roman-lower"
      }
    }
  }
  \vspace #1
  { "The example uses the following to create the list:" }
  \vspace #0.5
  { \typewriter "\createOutline outline number.alpha-upper.roman-lower.number.number"  }
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
    { Resets the outline to the first level, commencing again at the initial value for all levels. }
  }

  %{
% maybe later.
  \vspace #1
  \justified-lines {
    { \typewriter "\resetOutlineLevel <name> <number>" }
  }
  \justified-lines {
    { Resets the outline to the first level, commencing at the given number. }
  }
  %}
}


%\pageBreak
\markup \vspace #2
\markup \large \bold "Example"
\markup \vspace #1


%=====================
% create outline counter. give it a name and a list of level styles.
\createOutline outline number.alpha-upper.roman-lower.number.number

% create a separate one
\createOutline outline-b number.number.number

% demonstrate various usage
\markup { \inc #'outline \bold "Lorem ipsum" }
{ c''1 }
\markup { \inc #'outline \italic "Dolor sit amet" }
{ c''1 }
\markup { \inc #'outline "Consectetur adipiscing elit" }
{ c''1 }
\markup {
  \inc #'outline-b
  \justify {
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer bibendum sapien non feugiat imperdiet. Sed a ante ornare, sollicitudin augue eget, ultrices enim. Integer accumsan, eros vitae finibus mattis, neque nulla vehicula purus, sed euismod augue felis in nibh.
  }
}
\markup \vspace #0.5
\markup {
  \inc #'outline-b
  \justify {
    Nulla bibendum augue sit amet ullamcorper ornare. Integer aliquet justo rhoncus mauris sagittis faucibus ut ut quam. Suspendisse potenti. Curabitur ultricies neque sed turpis vehicula, a suscipit enim commodo. Maecenas finibus, sapien et laoreet maximus, ante tellus sollicitudin nulla, sit amet eleifend mi purus id ante. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean sollicitudin placerat ipsum.
  }
}
\markup \vspace #0.5
\markup {
  \inc #'outline-b
  \justify {
    Aliquam sagittis nisl at nisi efficitur commodo. In cursus bibendum lectus, eu mollis augue mattis ut. Nullam dignissim egestas aliquam. Proin enim neque, varius quis arcu vel, pellentesque laoreet quam. Pellentesque interdum mollis dui, in suscipit sem. Nunc sit amet erat et arcu auctor dapibus id sed mauris.
  }
}
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

%\pageBreak
\markup { \inc #'outline \bold "Lorem ipsum" }
{ c''1 }
\markup { \indentOutline #'outline "Consectetur adipiscing elit" }
{ c''1 }
\markup { \inc #'outline  "Quisque dictum mi" }
{ c''1 }
% Text not necessary if not needed
\markup { \outdentOutline #'outline }
{ c''1 }

\markup { \outdentOutline #'outline }
{ c''1 }
\markup { \indentOutline #'outline }
{ c''1 }
\markup { \indentOutline #'outline }
{ c''1 }
\markup { \resetOutline #'outline }
{ c''1 }
\markup { \inc #'outline }
{ c''1 }
\markup { \indentOutline #'outline }
{ c''1 }
\markup { \indentOutline #'outline }
{ c''1 }


\markup \vspace #1
\markup\large\bold "Author"
\markup \vspace #1
\markup {
  \typewriter "andrew.bernard@gmail.com"
}

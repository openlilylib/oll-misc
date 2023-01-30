\version "2.24.0"
\include "deutsch.ly"

\include "oll-core/package.ily"
\loadPackage edition-engraver
\loadModule oll-misc.pitch.auto-transpose

% some music to insert into example
bach = \relative c'' { b a c h }

% add two transposing instrument-definitions
\addInstrumentDefinition #"eb-clarinet"
  #`((instrumentTransposition . ,(ly:make-pitch 0 2 -1/2))
     (shortInstrumentName . "Es-Kl")
     (clefGlyph . "clefs.G")
     (middleCPosition . -6)
     (clefPosition . -2)
     (instrumentCueName . "Es-Kl")
     (midiInstrument . "clarinet"))

\addInstrumentDefinition #"b-clarinet"
  #`((instrumentTransposition . ,(ly:make-pitch -1 6 -1/2))
     (shortInstrumentName . "Kl")
     (clefGlyph . "clefs.G")
     (middleCPosition . -6)
     (clefPosition . -2)
     (instrumentCueName . "Kl")
     (midiInstrument . "clarinet"))

\addInstrumentDefinition #"concert-pitch"
  #`((instrumentTransposition . ,(ly:make-pitch 0 0 0))
     (shortInstrumentName . "C")
     (clefGlyph . "clefs.G")
     (middleCPosition . -6)
     (clefPosition . -2)
     (instrumentCueName . "C")
     (midiInstrument . "clarinet"))

%%% create demo score
\addEdition transp
\consistToContexts #edition-engraver Staff
\editionMod transp 2 0/1 switch.instrument.Staff.A \instrumentSwitch "b-clarinet"
\editionMod transp 3 0/1 switch.instrument.Staff.A \instrumentSwitch "eb-clarinet"
\editionMod transp 4 2/4 switch.instrument.Staff.A \instrumentSwitch "b-clarinet"
\editionMod transp 5 0/4 switch.instrument.Staff.A \instrumentSwitch "concert-pitch"

music = {
  \key f \major
  \repeat unfold 3 \bach
  <>_\markup \tiny "repeat unfold c''"
  \repeat unfold 4 c''4
  <>_\markup \tiny "repeat unfold d''"
  \repeat unfold 4 d''
}

\score {
  \new Staff \with {
    \autoTranspose
    \editionID ##f switch.instrument
  } \new Voice \music
  \layout {}
  \midi { \tempo 4=150 }
}



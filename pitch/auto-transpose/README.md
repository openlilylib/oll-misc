The auto-transpose engraver can be used to automatically transpose music according to the instrument transposition. The engraver has to know, if the music it finds in this context is in concert- or in instrument-pitch. Then it can use the `instrumentTransposition`, which is set by the `\transposition` command, to transpose the music as necessary.

The engraver looks for three context-properties:
1. instrumentTransposition
2. music-concert-pitch
3. print-concert-pitch

The first one is a standard lilypond-context-property, which is used to set instrument transposition for MIDI-output. So, if you use for example `\transposition bes` -- e.g. trumpet or clarinet -- MIDI-output will sound one note lower. Now the other two context-properties are used to set, if the music is provided in concert-pitch and if it shall be printed in concert-pitch. If the music is given in concert pitch and shall be printed in instrument pitch, the engraver transposes it accordingly. `\autoTranspose` provides a context-mod which consists the engraver and sets the *-concert-pitch variables to display concert-pitched music in instrument pitch.
So the following displays a C major scale, which sounds like a B flat major scale, if played by an instrument tuned in B flat, like trumpet.

```
\version "2.20.0"
\include "oll-core/package.ily"
\loadModule oll-misc.pitch.auto-transpose

\new Staff \with {
 \autoTranspose
} \relative c'' {
   \transposition bes
   \key bes \major
   bes4 a g f ees d c bes
}
```

If the instrument is switched, the auto-transpose will follow, and the key signature will be reprinted automatically. The inserted key signatures are printed by a separate engraver.

Known issue:

* If a key change happens at the same moment as a transposition change, printing the correct key signature depends on the order in which the Lilypond commands are written. See the examples in auto-transpose-keys-1.ly and auto-transpose-keys-2.ly for details.

There are some TODOs:

* If the music is given in instrument pitch and shall be displayed, the `instrumentTransposition` property still has to be set, but that leads to incorrect MIDI-pitch.
* We only need one context-property beside `instrumentTransposition`, which says:
    1. do nothing
	2. transpose from concert to instrument pitch
	3. transpose from instrument to concert pitch
    This might also be an initial parameter for the engraver, so we needn't define extra context-properties
* Improve the key signature behavior when key changes happen simultaneously with transposition changes. This may not be possible without a patch to Lilypond itself.


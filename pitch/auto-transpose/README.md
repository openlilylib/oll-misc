The auto-transpose engraver can be used to automatically transpose music either from concert pitch to instrument pitch (default), or vica versa.

For example, this snippet is a B-flat major scale written in Lilypond at concert pitch, but will produce sheet music transposed for B-flat clarinet or trumpet:

```
\version "2.24.0"
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

The engraver looks for three context-properties:

1. `instrumentTransposition`
2. `transposeDirection`
3. `autoTransposeKeySignatures`

`instrumentTransposition` is a standard Lilypond context property, set by the `\transposition` command. Lilypond uses it to output correct MIDI pitch when music is entered at instrument pitch. Auto-transpose engraver uses this property to transpose the printed music.

If `transposeDirection` is set to `'concert-to-pitch`, music entered in concert pitch will be printed at instrument pitch according to `instrumentTransposition`. This is the default setting.

If `transposeDirection` is set to `'pitch-to-concert` music entered at instrument pitch (again, according to `instrumentTransposition`) will be printed at concert pitch.

For example, this snippet is a concert B-flat major scale entered into Lilypond at instrument pitch for a B-flat clarinet or trumpet (i.e. as a C major scale). The output sheet music will be a B-flat major scale written in concert pitch.

```
\new Staff \with {
  \consists \autoTransposeEngraver
  transposeDirection = #'pitch-to-concert
} \relative c'' {
  \transposition bes
  \key c \major
  c4 b a g f e d c
}
```

If `transposeDirection` is set to `#f`, no automatic transposition will occur.

If `instrumentTransposition` changes, as when a player switches instruments, auto-transpose will follow. 

If `autoTransposeKeySignatures` is set to `'insert-and-transpose`, key signatures will be reprinted automatically whenever `instrumentTransposition` changes. This is the default.

If `autoTransposeKeySignatures` is set to `'transpose-only`, key signatures will not be automatically inserted, but explicit key changes will still be transposed. This is similar to the old version of auto-transpose.

If `autoTransposeKeySignatures` is set to `#f`, auto-transpose will ignore key signatures. Use this setting any time you would `\remove Key_engraver` or `\omit KeySignature`, such as for atonal passages or transposing instruments that traditionally do not use a key signature, such as orchestral horns. 

In this example, the concert B-flat major scale is printed at instrument pitch for horn, with no key signature. Note that if `autoTransposeKeySignatures = ##f` had not been set, the B-flat accidental would not be printed due to auto-transpose behaving as if the key signature were present.

```
\new Staff \with {
  \consists \autoTransposeEngraver
  autoTransposeKeySignatures = ##f
  \remove Key_engraver
} \relative c'' {
  \transposition f
  \key bes \major
  bes4 a g f ees d c bes
}
```

A context-mod `\autoTranspose` is provided, which enables the default behavior, equivalent to:

```
\with {
  \consists \autoTransposeEngraver
  transposeDirection = #'concert-to-pitch
  autoInsertKeySignatures = ##t
}
```

Known issues:

* If a key change happens at the same moment as a transposition change, a warning may be triggered ``warning: conflict with event: `key-change-event'``. These can be safely ignored.
* If `transposeDirection` is set to `'pitch-to-concert` (music is entered in instrument pitch and displayed at concert pitch), MIDI output is at the incorrect pitch. Currently the easiest way around this is to use a separate `\score` block for MIDI output, and omit auto-transpose from it.


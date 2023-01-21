\version "2.19.7"

\header {
  snippet-title = "auto-transpose"
  snippet-author = "Jan-Peter Voigt, Saul James Tobin"
  snippet-description = \markup {
    \wordwrap {
      This engraver transposes music accordingly to 'instrumentTransposition'
      and 'music-concert-pitch' plus 'print-concert-pitch'.
      The example is working from concert-pitch to instrument-pitch, but MIDI is not right in the other direction. (TODO)
    }
  }
  tags = "transpose,transposition"
  status = "unknown"
}

% taken from "scm/define-context-properties.scm"
#(define (translator-property-description symbol type? description)
   (if (not (and
             (symbol? symbol)
             (procedure? type?)
             (string? description)))
       (throw 'init-format-error))

   (if (not (equal? #f (object-property symbol 'translation-doc)))
       (ly:error (_ "symbol ~S redefined" symbol)))

   (set-object-property! symbol 'translation-type? type?)
   (set-object-property! symbol 'translation-doc description)
   (set! all-translation-properties (cons symbol all-translation-properties))
   symbol)
% add context properties descriptions
%   music-concert-pitch
%   print-concert-pitch
#(translator-property-description 'music-concert-pitch boolean? "music is in concert pitch")
#(translator-property-description 'print-concert-pitch boolean? "print it in concert pitch")

#(define (which-transp context transp)
   (let ((base (ly:make-pitch 0 0 0)) ; pitch c'
          (mcp (ly:context-property context 'music-concert-pitch)) ; music is in concert-pitch t/f
          (pcp (ly:context-property context 'print-concert-pitch)) ; print it in concert-pitch t/f
          )
     (cond
      ((and mcp (not pcp) (ly:pitch? transp))
       (ly:pitch-diff base transp))
      ((and (not mcp) pcp (ly:pitch? transp))
       transp)
      (else #f))))

#(define (cond-transp context music)
   (let ((transp (ly:context-property context 'instrumentTransposition))
         (base (ly:make-pitch 0 0 0)))
     (define (do-transp m)
       (let ((ap (ly:music-property m 'auto-transpose))
             (tp (which-transp context transp))
             )
         (if (ly:pitch? tp)
             (cond
              ((and (ly:pitch? ap)(not (equal? ap tp)))
               (ly:music-transpose m (ly:pitch-diff tp ap)))
              ((not (ly:pitch? ap))
               (ly:music-transpose m tp))
              )
             (if (ly:pitch? ap)
                 (begin
                  (ly:music-transpose m (ly:pitch-diff base tp))
                  ))) ; TODO
         (ly:music-set-property! m 'auto-transpose tp)
         ))
     ; execute transposition
     (do-transp music)
     ))

#(define (complete-keysig alterations)
   (let ((cmaj '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)))
         (update (lambda (el sig) (assoc-set! sig (car el) (cdr el)))))
     (fold update (copy-tree cmaj) alterations)))

autoKeysigEngraver =
#(lambda (context)
   (let ((lasttransp (ly:context-property context 'instrumentTransposition)))
     (define (insert-key)
       (let* ((keysig (complete-keysig (ly:context-property context 'keyAlterations)))
              (tonic (ly:context-property context 'tonic))
              (transp (ly:context-property context 'instrumentTransposition))
              (keysig-music (make-music 'KeyChangeEvent
                              'pitch-alist keysig
                              'tonic tonic
                              'length (ly:make-moment 0)
                              'auto-transpose (which-transp context lasttransp))))
         (if (not (equal? transp lasttransp))
             (let ((new-key (ly:music-deep-copy keysig-music)))
               (cond-transp context new-key)
               (if (not (equal? (ly:music-property keysig-music 'pitch-alist) ; don't reprint key if only 8ve change
                                (ly:music-property new-key 'pitch-alist)))
                   (let ((key-event (ly:make-stream-event
                                     (ly:make-event-class 'key-change-event)
                                     (ly:music-mutable-properties new-key))))
                     (ly:message "Transposition changed. Inserting key signature in measure ~A."
                       (ly:context-property context 'currentBarNumber))
                     (ly:event-set-property! key-event 'music-cause new-key)
                     (ly:broadcast (ly:context-event-source context) key-event)
                     )))
             (set! lasttransp transp))))
     (define (already-key)
       (let ((transp (ly:context-property context 'instrumentTransposition)))
         (if (not (equal? transp lasttransp))
             (set! lasttransp transp))))
     (make-engraver
      (listeners
       ((note-event engraver event)
        (insert-key))
       ((rest-event engraver event)
        (insert-key))
       ((key-change-event engraver event)
        (already-key))
       )
      )
     )
   )

% engraver to automatically transpose music
autoTransposeEngraver =
#(lambda (context)
   ; create engraver
   (make-engraver
    (listeners
     ; transpose note-event
     ((note-event engraver event)
      (cond-transp context (ly:event-property event 'music-cause)))
     ; transpose key-signature
     ((key-change-event engraver event)
      (cond-transp context (ly:event-property event 'music-cause)))
     )
    )
   )

autoTranspose = \with {
  % we have to ensure, the key-engraver acts after transposition is done
  \remove "Key_engraver"
  \consists \autoTransposeEngraver
  \consists \autoKeysigEngraver
  \consists "Key_engraver"
  % if music and print are equal, do nothing
  % else transpose according to transp (up or down)
  music-concert-pitch = ##t
  print-concert-pitch = ##f
  % TODO: if music is given in instrument-pitch, but shall be printed in concert-pitch,
  %   midi pitch is false - instrumentTransposition should be "turned off" for midi(?)
}

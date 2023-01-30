\version "2.24.0"

\header {
  snippet-title = "auto-transpose"
  snippet-author = "Jan-Peter Voigt, Saul James Tobin"
  snippet-description = \markup {
    \wordwrap {
      This engraver transposes music according to 'instrumentTransposition'
      and 'transpositionDirection' and prints key signatures whenever the transposition changes.
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
%   transpose-direction
#(translator-property-description 
  'transposeDirection boolean-or-symbol? 
  "Auto-transpose setting. Valid options are 'concert-to-pitch (default – concert pitch input, transposed output), 'pitch-to-concert (transposed input, concert pitch output), and #f to disable autotranspose.")

#(define (which-transp context transp)
   (let ((transpose-direction (ly:context-property context 'transposeDirection 'concert-to-pitch)))
     (cond
      ((and (equal? transpose-direction 'concert-to-pitch) (ly:pitch? transp))
       (ly:pitch-diff (ly:make-pitch 0 0 0) transp)) ; invert around middle c' for opposite semantics of Lilypond's default
      ((and (equal? transpose-direction 'pitch-to-concert) (ly:pitch? transp))
       transp) ; keep transposition as-is for semantics that match Lilypond's default
      (else #f))))

%  To Do: currently cond-transp is used for keysigs, but there are builtin scheme functions to transpose pitches and keysig alists.
%  Maybe using them could avoid the need for complete-keysig and order-keysig helpers?
%  Also cond-transp should return the transposed music expression.
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

#(define (order-keysig context pitch-alist)
   (let ((order (ly:context-property context 'keyAlterationOrder)))
     (filter
      (lambda (alt)
        (any (lambda (el)
               (equal? el alt))
          pitch-alist))
      order)))

autoTransposeEngraver = 
#(lambda (context)
   (let ((lasttransp (ly:context-property context 'instrumentTransposition))
         (event-cache #f))
     
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
                     (ly:event-set-property! key-event 'pitch-alist 
                       (order-keysig context (ly:event-property key-event 'pitch-alist))) ; Fixing the order might not be needed here
                     (ly:broadcast (ly:context-event-source context) key-event)
                     )))
             (set! lasttransp transp))))
     
     
     (make-engraver
      (listeners
       ((rest-event engraver event)
        ;if transposition changed, broadcast a key change event, then reset lasttransp
        (insert-key)
        )
       
       ((note-event engraver event)
        ;if transposition changed, broadcast a key change event, then reset lasttransp
        (insert-key)
        ;transpose the note
        (cond-transp context (ly:event-property event 'music-cause))
        )
       
       ((key-change-event engraver event)
        ; if no event already cached, or if 'autotranspose is not set, cache the event:
        ; always register explicit key changes, 
        ; but only register automatic keysig if there is no conflicting event
        (if (not (ly:stream-event? event-cache))
            (set! event-cache event)
            (if (not (ly:event-property event 'auto-transpose #f))
                (set! event-cache event)))
        
        ; if transposition changed, reset lasttransp 
        ; (to suppress auto keysig when an explicit key change is already present)
        (let ((transp (ly:context-property context 'instrumentTransposition)))
         (if (not (equal? transp lasttransp))
             (set! lasttransp transp)))
        )
       )
      
      ((pre-process-music engraver)
       ; if an event is cached, transpose the tonic and pitch-alist, then set context properties
       (if (ly:stream-event? event-cache)
           (let ((key-music (ly:event-property event-cache 'music-cause)))
             (cond-transp context key-music)
             (let* ((full-alts (ly:music-property key-music 'pitch-alist))
                    ; discard all the naturals
                    (alts (filter
                           (lambda (alt) (not (equal? 0 (cdr alt))))
                           full-alts))
                    ; fix the order of alterations
                    (proper-alts (order-keysig context alts)))
               ; Key_engraver reads from context properties NOT from the event itself when printing a keysig
               ; So we can wait for all events to be heard, then fix the keysig to reflect the transposition
               ; even if the key change was heard before the transposition change.
               (ly:context-set-property! context 'keyAlterations proper-alts)
               (ly:context-set-property! context 'tonic (ly:music-property key-music 'tonic)))))
       )
      
      ((stop-translation-timestep engraver)
        ;unset the event cache
        (set! event-cache #f)
       )
      )))

autoTranspose = \with {
  \consists \autoTransposeEngraver
  transposeDirection = #'concert-to-pitch
  % TODO: if music is given in instrument-pitch, but shall be printed in concert-pitch,
  %   midi pitch is false - instrumentTransposition should be "turned off" for midi(?)
}

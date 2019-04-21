% Outline list counter class for markup. Fancy version.
%
% Andrew Bernard April 2019

\version "2.19.83"

#(use-modules (ice-9 format))

#(define simple-counter
   (lambda ()
     (let ((count 1))

       (define (inc)
         (let ((v count))
           (set! count (+ 1 count))
           v))

       (define (reset)
         (let ((v 0))
           (set! count 1)
           v))

       (lambda (arg)
         (cond
          ((eq? arg 'inc) inc)
          ((eq? arg 'reset) reset)
          (else (error "invalid method")))))))

#(define roman-counter
   (lambda (case)
     (let ((count 1))

       (define (inc)
         (let ((v count))
           (set! count (+ 1 count))
           ;; use cond
           (if (eq? case 'lowercase)
               (string-downcase (format #f "~@r" v))
               (format #f "~@r" v))))

       (define (reset)
         (set! count 1)
         count)

       (lambda (arg)
         (cond
          ((eq? arg 'inc) inc)
          ((eq? arg 'reset) reset)
          (else (error "invalid method")))))))

#(define alpha-counter
   (lambda (case)
     "Alphabetic counter. Increments letter by one on each call. When z
is reached the letter is doubled, then tripled, and so on."

     (define repeat-char
       (lambda (char num)
         "Repeats given character num times. Returns a string."
         (let loop ((i 0) (s ""))
           (if (= i num)
               s
               (begin
                (set! s (string-append s (string char)))
                (loop (+ 1 i) s))))))

     (let* ((count 0)
            (level 0)
            (ascii
             (cond ((eq? case 'lowercase) 97)
               ((eq? case 'uppercase) 65)
               (else 97)))) ;; a bit strange. think about it later.

       (define (inc)
         (let ((v (+ ascii (modulo count 26))))
           (set! count (+ 1 count))
           (set! level (quotient (- count 1) 26))
           ;; (format #t "count = ~a v = ~a level = ~a\n" count v (+ 1 level))
           (repeat-char (integer->char v) (+ 1 level))))

       (define (reset)
         (set! count 0)
         (set! level 0)
         100)

       (lambda (arg)
         (cond
          ((eq? arg 'inc) inc)
          ((eq? arg 'reset) reset)
          (else (error "invalid method")))))))


#(define make-outline-counter
   (lambda (list-styles)

     ;; make the styles vector of functions from the passed in list specification.
     ;; this may go in the next let. later.
     (let ((styles (make-vector 10)))
       (let loop ((i 0) (l list-styles))
         (if (not (null? l))
             (begin
              (cond
               ((eq? (car l) 'number)
                (vector-set! styles i (simple-counter)))

               ((eq? (car l) 'roman-lower)
                (vector-set! styles i (roman-counter 'lowercase)))

               ((eq? (car l) 'roman-upper)
                (vector-set! styles i (roman-counter 'uppercase)))

               ((eq? (car l) 'alpha-lower)
                (vector-set! styles i (alpha-counter 'lowercase)))

               ((eq? (car l) 'alpha-upper)
                (vector-set! styles i (alpha-counter 'uppercase))))

              (loop (1+ i) (cdr l)))))

       (let* ((lst (make-vector 31)) ;; guile does not appear to expand on demand
               (indent-level 0))

         (define counter
           (lambda (method)

             (define (inc)
               ;; increment counter
               (vector-set! lst indent-level (((vector-ref styles indent-level) 'inc)))
               (output-list-item-number))

             (define (indent)
               ;; indent level by one
               (set! indent-level (+ indent-level 1))
               (vector-set! lst indent-level (((vector-ref styles indent-level) 'inc)))
               (output-list-item-number))

             (define (unindent)
               ;; outdent level by one
               (if (> indent-level 0)
                   (begin
                    ; reset level
                    (vector-set! lst indent-level (((vector-ref styles indent-level) 'reset)))
                    (set! indent-level (- indent-level 1))
                    (inc)
                    (output-list-item-number))))

             (define (reset)
               ;; reset counter to first level, initial value. Do this for all levels
               ;; in use.
               ;; ...
               (let loop((i 0))
                 (if (< i (length list-styles))
                     (begin
                      (format #t "~a\n" i)
                      (vector-set! lst i (((vector-ref styles i) 'reset)))
                      (loop (1+ i)))
                     )
                 )
               (set! indent-level 0)
               (inc)
               (output-list-item-number))

             (define (reset-level num)
               ;; TODO - redo this for new fancy version.
               ;; Do not use.
               ;; reset counter to first level, starting at given value
               (set! lst (drop-right lst (- (length lst) 1)))
               (list-set! lst 0 num)
               (set! indent-level 0)
               (output-list-item-number))

             (define (output-list-item-number)
               ;; output complete formatted string
               (let loop ((i 0) (l (vector->list lst)) (str ""))
                 (if (> i indent-level)
                     str
                     (begin
                      (loop
                       (1+ i)
                       (cdr l)
                       (string-append str (format #f "~a." (car l)))
                       )))))

             (cond
              ((eq? method 'inc) inc)
              ((eq? method 'indent) indent)
              ((eq? method 'reset) reset)
              ((eq? method 'unindent) unindent)
              ((eq? method 'reset-level) reset-level))
             ))
         counter))))

#(define counters (make-hash-table))

createOutline =
#(define-void-function (name list-styles)
   (symbol? list?)
   "Make an outline counter. Give it a name - a symbol.
    The creation function stores the name of the counter procedure in
    a global hash table, with its associated counter class."
   (hashq-set! counters name (make-outline-counter list-styles)))

#(define-markup-command (inc layout props name)
   (symbol?)
   "Increment the counter by one."
   (interpret-markup layout props
     (let ((a (hashq-ref counters name)))
       (markup ((a 'inc))))))

#(define-markup-command (indentOutline layout props name)
   (symbol?)
   "Indent the level by one."
   (interpret-markup layout props
     (let ((a (hashq-ref counters name)))
       (markup ((a 'indent))))))

#(define-markup-command (outdentOutline layout props name)
   (symbol?)
   "Outdent the level by one."
   (interpret-markup layout props
     (let ((a (hashq-ref counters name)))
       (markup ((a 'unindent))))))

#(define-markup-command (resetOutline layout props name)
   (symbol?)
   "Reset the counter to one."
   (interpret-markup layout props
     (let ((a (hashq-ref counters name)))
       (markup ((a 'reset))))))

#(define-markup-command (resetOutlineLevel layout props name level)
   (symbol? number?)
   "Reset the counter to given number."
   (interpret-markup layout props
     (let ((a (hashq-ref counters name)))
       (markup ((a 'reset-level) level)))))

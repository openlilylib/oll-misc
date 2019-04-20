\version "2.19.83"

\header {
  snippet-title = "Outline Counter"
  snippet-author = "Andrew Bernard"
  snippet-author-email = "andrew.bernard@gmail.com"
  snippet-source = ""
  snippet-description = \markup {
    Provides functions for using outline numbered lists in markups.
  }
  tags = "outline counter"
  status = "ready"
}

#(define make-outline-counter
   (lambda ()
     (let ((lst (list 0))
           (indent-level 0))
       (define counter
         (lambda (method)
           (define (inc)
             (list-set! lst indent-level (+ (list-ref lst indent-level) 1))
             (output-list-item-number))

           (define (indent)
             (set! lst (append lst (list 1)))
             (set! indent-level (+ indent-level 1))
             (output-list-item-number))

           (define (unindent)
             (if (> indent-level 0)
                 (begin
                  (set! indent-level (- indent-level 1))
                  (set! lst (drop-right lst 1))
                  (inc)
                  (output-list-item-number))))

           (define (reset)
             (set! lst (drop-right lst (- (length lst) 1)))
             (list-set! lst 0 1)
             (set! indent-level 0)
             (output-list-item-number))

           (define (reset-level num)
             (set! lst (drop-right lst (- (length lst) 1)))
             (list-set! lst 0 num)
             (set! indent-level 0)
             (output-list-item-number))

           (define (output-list-item-number)
             (let loop ((l lst) (str ""))
               (if (null? l)
                   str
                   (begin
                    (loop (cdr l)
                      (string-append str (format #f "~a." (car l)))
                      )))))

           (cond
            ((eq? method 'inc) inc)
            ((eq? method 'indent) indent)
            ((eq? method 'reset) reset)
            ((eq? method 'unindent) unindent)
            ((eq? method 'reset-level) reset-level))
           ))
       counter)))

% top level counters hash table

#(define counters (make-hash-table))

% creation

createOutline =
#(define-void-function (name)
   (symbol?)
   "Make an outline counter. Give it a name - a symbol.
    The creation function stores the name of the counter procedure in
    a global hash table, with its associated counter class."
   (hashq-set! counters name (make-outline-counter)))

% markups

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


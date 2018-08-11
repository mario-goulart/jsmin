(module jsmin (jsmin-string jsmin-file)

(import scheme)
(cond-expand
 (chicken-4
  (import chicken ports))
 (chicken-5
  (import (chicken base)
          (chicken port)))
 (else (error "Unsupported CHICKEN version.")))

(define the-lookahead #!eof)
(define the-a #f)
(define the-b #f)

(define (alphanum? char)
  ;; Return true if the character is a letter, digit, underscore, dollar
  ;; sign, or non-ASCII character.
  (and (not (eof-object? char))
       (or (char-alphabetic? char)
           (char-numeric? char)
           (> (char->integer char) 126)
           (memq char '(#\_ #\$)))))

(define (get-char)
  ;; Return the next character from stdin. Watch out for lookahead. If
  ;; the character is a control character, translate it to a space or
  ;; linefeed.
  (let ((c the-lookahead))
    (set! the-lookahead #!eof)
    (when (eof-object? c)
      (set! c (read-char)))

    (cond ((or (eof-object? c)
               (char=? c #\newline)
               (>= (char->integer c) (char->integer #\space)))
           c)
          ((char=? c #\return)
           #\newline)
          (else #\space))))

(define (peek)
  ;; Get the next character without getting it.
  (set! the-lookahead (get-char))
  the-lookahead)

(define (next)
  ;; Get the next character, excluding comments. peek() is used to see
  ;; if a '/' is followed by a '/' or '*'.
  (call/cc (lambda (return)
             (let ((c (get-char)))
               (if (and (char? c) (char=? c #\/))
                   (let ((p (peek)))
                     (when (and (char? p) (char=? p #\/))
                       (let loop ()
                         (let ((c (get-char)))
                           (if (<= (char->integer c) (char->integer #\newline))
                               (return c)
                               (loop)))))
                     (when (char=? p #\*)
                       (get-char)
                       (let loop ()
                         (case (get-char)
                           ((#\*) (when (char=? (peek) #\/)
                                    (get-char)
                                    (return #\space)))
                           ((#!eof) (error "JSMIN Unterminated comment.")))
                         (loop)))
                     (return c))
                   (return c))))))

(define (action d)
  ;; Do something! What you do is determined by the argument:
  ;;      1   Output A. Copy B to A. Get the next B.
  ;;      2   Copy B to A. Get the next B. (Delete A).
  ;;      3   Get the next B. (Delete B).
  ;; action treats a string as a single character. Wow!
  ;; action recognizes a regular expression if it is preceded by ( or , or =.
  (when (= d 1)
    (display the-a))

  (when (<= d 2)
    (call/cc (lambda (return)
               (set! the-a the-b)
               (if (memq the-a '(#\' #\"))
                   (let loop ()
                     (display the-a)
                     (set! the-a (get-char))
                     (if (char=? the-a the-b)
                         (return 'nothing)
                         (begin
                           (when (char=? the-a #\\)
                             (display the-a)
                             (set! the-a (get-char)))
                           (when (eof-object? the-a)
                             (error "JSMIN unterminated string literal."))))
                     (loop))))))

  (when (<= d 3)
    (call/cc (lambda (return)
               (set! the-b (next))
               (when (and (char? the-b)
                          (char=? the-b #\/)
                          (memq the-a '(#\( #\, #\= #\: #\[ #\! #\&
                                        #\| #\? #\{ #\} #\; #\newline)))
                 (display the-a)
                 (display the-b)
                 (let loop ()
                   (set! the-a (get-char))
                   (unless (char=? the-a #\/)
                     (cond ((char=? the-a #\\)
                            (display the-a)
                            (set! the-a (get-char)))
                           ((eof-object? the-a)
                            (error "JSMIN unterminated Regular Expression literal.")))
                     (display the-a)
                     (loop)))
                 (set! the-b (next)))))))

(define (jsmin)
  ;; Copy the input to the output, deleting the characters which are
  ;; insignificant to JavaScript. Comments will be removed. Tabs will
  ;; be replaced with spaces. Carriage returns will be replaced with
  ;; linefeeds.  Most spaces and linefeeds will be removed.
  (set! the-a #\newline)
  (action 3)
  (let loop ()
    (unless (eof-object? the-a)
      (case the-a
        ((#\space) (if (alphanum? the-b)
                       (action 1)
                       (action 2)))
        ((#\newline) (case the-b
                       ((#\{ #\[ #\( #\+ #\-) (action 1))
                       ((#\space) (action 3))
                       (else (if (alphanum? the-b)
                                 (action 1)
                                 (action 2)))))
        (else (case the-b
                ((#\space) (if (alphanum? the-a)
                               (action 1)
                               (action 3)))
                ((#\newline) (case the-a
                               ((#\} #\] #\) #\+ #\- #\" #\') (action 1))
                               (else (if (alphanum? the-a)
                                         (action 1)
                                         (action 3)))))
                (else (action 1)))))
      (loop))))

(define (jsmin-string str)
  (with-output-to-string (cut with-input-from-string str jsmin)))

(define (jsmin-file file)
  (with-output-to-string (cut with-input-from-file file jsmin)))

); end module

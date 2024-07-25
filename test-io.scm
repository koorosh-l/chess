#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -L /home/quasikote/proj/chess/ -s "$0"
!#
(use-modules (utils)
	     (io)
	     (ice-9 textual-ports))
(define failed-counter 0)
(define total 0)
(define (s* n str)
  (if (zero? n)
      ""
      (string-append str (s* (1- n) str))))
(define-syntax io-test
  (syntax-rules ()
    ([_ thunk inp ret]
     (io-test thunk inp ret ""))
    [(_ thunk inp ret out)
     (let ()
       (set! total (1+ total))
       (define res)
       (define str
	 (with-output-to-string (lambda () (with-input-from-string inp (lambda () (set! res (thunk)))))))
       (when (or (not (equal? res ret))
		 (not (string=? out str)))
	 (set! failed-counter (1+ failed-counter)))
       (when (not (equal? res ret))
	 (format (current-error-port) "return FAIL AT:: ~a\n" (current-source-location)))
       (when (not (string=? out str))
	 (format (current-error-port) "output FAIL AT:: ~a\n" (current-source-location))))]))
;;watch for eof when throwing

(eval-when (compile)
  (define (move->str l fmt)
    (define (num->sym n)
      (string->symbol (format #f "~a" (integer->char (+ n (char->integer #\a))))))
    (let ([scl   (num->sym (list-ref l 0))]
	  [srank (1+ (list-ref l 1))]
	  [dcl   (num->sym (list-ref l 2))]
	  [drank (1+ (list-ref l 3))])
      (format #f fmt scl srank dcl drank)))
  (define (delete-more items l)
    (let loop ([res l] [items items])
      (cond
       [(null? items) res]
       [else (loop (delete (car items) res)
		   (cdr items))])))
  (define all-possible-moves
    (map (lambda (m) (move->str m "~a~a~a~a"))
	 (apply append (map (lambda (i1)
			      (apply append (map (lambda (j1)
						   (apply append (map (lambda (i2)
									(map (lambda (j2)
									       (list i1 i2 j1 j2))
									     (delete-more `(,j1) (iota 8))))
								      (delete-more `(,i1) (iota 8)))))
						 (iota 8))))
			    (iota 8))))))

(io-test get-piece "r" 'r)
(io-test get-piece "h" 'h)
(io-test get-piece "b" 'b)
(io-test get-piece "q" 'q)
(io-test get-piece "\nr" 'r wrong-pice-msg)
(io-test get-piece "\nh" 'h wrong-pice-msg)
(io-test get-piece "\nb" 'b wrong-pice-msg)
(io-test get-piece "\nq" 'q wrong-pice-msg)

(io-test get-piece "a\nr" 'r wrong-pice-msg)
(io-test get-piece "a\nh" 'h wrong-pice-msg)
(io-test get-piece "a\nb" 'b wrong-pice-msg)
(io-test get-piece "a\nq" 'q wrong-pice-msg)

(io-test get-piece "a\nr\n" 'r wrong-pice-msg)
(io-test get-piece "a\nh\n" 'h wrong-pice-msg)
(io-test get-piece "a\nb\n" 'b wrong-pice-msg)
(io-test get-piece "a\nq\n" 'q wrong-pice-msg)

(io-test get-piece "a\ns\nh\n" 'h (s* 2 wrong-pice-msg))
(io-test get-piece "a\ns\nb\n" 'b (s* 2 wrong-pice-msg))
(io-test get-piece "a\ns\nq\n" 'q (s* 2 wrong-pice-msg))

(format (current-output-port) "failed: ~a\t total: ~a\n" failed-counter total)

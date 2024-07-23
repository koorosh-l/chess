(define-module (io)
  #:use-module (utils)
  #:use-module (chess)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match))

(define cl-set (list->char-set '(#\a #\b #\c #\d #\e #\f #\g)))
(define-public (clear)
  (format (current-output-port) "~ac" (integer->char #x1b)))
(define-inlinable (rank->index num)
  (if (and (> num 7) (<= num 0))
      (throw 'wrong-move)
      (- 8 num)))
(define-inlinable (cl->index char)
  (if (char-set-contains? cl-set char)
      (-  (char->integer char) (char->integer #\a))
      (throw 'wrong-move)))

(define-public (get-move)
  (catch 'wrong-move
    (lambda ()
      (let ([str (string-downcase (get-line (current-input-port)))])
	(when (or (not (= 4 (string-length str))))
	  (throw 'wrong-move))
	(let ([scl   (string-ref str 0)]
	      [srank (string->number (substring str 1 2))]
	      [dcl   (string-ref str 2)]
	      [drank (string->number (substring str 3 4))])
	  (when (not srank) (not drank)
		(throw 'wrong-move))
	  (list (rank->index srank) (cl->index scl)
		(rank->index drank) (cl->index dcl)))))
    (lambda a
      (display "wrong input, correct format is: <rank><column><rank><column> character and number in orther\n")
      (get-move))))
(define-public (get-piece)
  (catch 'wrong-pice
    (lambda ()
      (let* ([str (get-line (current-input-port))]
	     [p (string->symbol (substring str 0 1))])
	(if (or (equal? p 'r) (equal? p 'h) (equal? p 'b) (equal? p 'q))
	    p
	    (throw 'wrong-pice))))
    (lambda a
      (display "wrong pice should be a character one of r,h,b,q") (newline)
      (get-piece))))

(define (sym->unicode sym)
  (match (symbol->string sym)
    ["ee" "   "]
    ["bp" " ♟︎ "]
    ["wp" " ♙ "]
    ["br" " ♜ "]
    ["wr" " ♖ "]
    ["bh" " ♞ "]
    ["wh" " ♘ "]
    ["bb" " ♝ "]
    ["wb" " ♗ "]
    ["bq" " ♛ "]
    ["wq" " ♕ "]
    ["bk" " ♚ "]
    ["wk" " ♔ "]))
(define (colored-str i j sym)
  (let*  ([esq (integer->char #x1b)]
	  [reset   "[0m"]
	  [whitebg "[106m"]
	  [blackbg "[46m"]
	  [whitefg "[31m"]
	  [blackfg "[30m"]
	  [bg (if (even? i)
		  (if (even? j) whitebg blackbg)
		  (if (even?  j) blackbg whitebg))]
	  [fg (cond
	       [(equal? 'ee sym) blackfg]
	       [(black? sym) blackfg]
	       [(white? sym) whitefg])])
    (format #f "~a~a~a~a~a~a~a" esq bg esq fg (sym->unicode sym) esq reset)))
(define-public (print-board brd)
  (call-with-output-string
    (lambda (p)
      (define c (current-output-port))
      (set-current-output-port p)
      (display "    A  B  C  D  E  F  G  H") (newline)
      (display "══════════════════════════") (newline)
      (define i 0) (define j 0)
      (while (< i 8)
	(set! j 0)
	(while (< j 8)
	  (when (= j 0) (display (- 8 i)) (display "│ "))
	  (display (colored-str i j
				(chess-ref brd i j)))
	  (set! j (1+ j)))
	(newline)
	(set! i (1+ i)))
      (set-current-output-port c))))

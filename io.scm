(define-module (io)
  #:use-module (chess)
  #:use-module (ncurses curses)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match))
(define scr)
(define-public (prepare)
  (set! scr (initscr)))
(define-public (finalize)
  (endwin))

(define-public (get-move)
  (catch 'wrong-move
    (lambda ()
      (let ([str (get-line (current-input-port))])
	(when (or (not (= 4 (string-length str))))
	  (throw 'wrong-move))
	(let ([scl   (string->symbol (string-downcase (format #f "~a" (string-ref str 0))))]
	      [srank (string->number (format #f "~a" (string-ref str 1)))]
	      [dcl   (string->symbol (string-downcase (format #f "~a" (string-ref str 2))))]
	      [drank (string->number (format #f "~a" (string-ref str 3)))])
	  (when (not srank) (not drank)
		(throw 'wrong-move))
	  (list srank scl drank dcl))))
    (lambda a
      (display "wrong input correct format is <rank><column><rank><column> character and number in orther\n")
      (get-move))))
(define-public (get-promote)
  (display "PROMOTE") (newline)
  (string->symbol (get-string-n (current-input-port) 1)))
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
    (format #f "~a~a~a~a~a~a~a" esq bg esq fg (sym->unicode sym)  esq reset)))

(define-public (print-board brd)
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
    (set! i (1+ i))))

(define-module (board)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (isin? <board>
		  chess-ref
		  chess-set!
		  board-get-board   board-set-board!
		  board-get-height  board-set-height!
		  board-get-width   board-set-width!
		  board-get-turn    board-set-turn!
		  board-get-move-no board-set-move-no!
		  board-get-history board-set-history!
		  new-chess-board
		  black? white?))
;;board
(define-record-type <board>
  (_make-board height width turn move-no history)
  board?
  (board   board-get-board   board-set-board!)
  (height  board-get-height  board-set-height!)
  (width   board-get-width   board-set-width!)
  (turn    board-get-turn    board-set-turn!)
  (move-no board-get-move-no board-set-move-no!)
  (history board-get-history board-set-history!))
(define on-board?
  (case-lambda
    [(brd p) (array-in-bounds? (board-get-board brd) (car p) (cdr p))]
    [(brd i j) (array-in-bounds? (board-get-board brd) i j)]))
(define-inlinable (chess-ref brd i j)
  (array-ref (board-get-board brd) i j))
(define-inlinable (chess-set! brd p i j)
  (array-set! (board-get-board brd) p i j)
  brd)
(define-inlinable (empty? brd i j) (equal? (chess-ref brd i j) 'ee))
(define checker-template #2((br bh bb bq bk bb bh br)
			    (bp bp bp bp bp bp bp bp)
			    (ee ee ee ee ee ee ee ee)
			    (ee ee ee ee ee ee ee ee)
			    (ee ee ee ee ee ee ee ee)
			    (ee ee ee ee ee ee ee ee)
			    (wp wp wp wp wp wp wp wp)
			    (wr wh wb wq wk wb wh wr)))
(define (new-chess-board)
  (let ([res (_make-board 8 8 'white 0 '())]
	[arr (make-array 'ee 8 8)])
    (array-copy! checker-template arr)
    (board-set-board! res arr)
    res))
;;utils
(define (pice-index brd p)
  (define res '())
  (array-index-map! (board-get-board brd)
		    (lambda (i j)
		      (let ([c (chess-ref brd i j)])
			(when (equal? p c)
			  (set! res (cons `(,i . ,j) res)))
			c)))
  res)
(define-inlinable (pice-color sym)
  (match (string-ref (symbol->string sym) 0)
    [#\b 'black]
    [#\w 'white]
    [else (display "--------") (display sym) (newline) (error "some how sym is not a pice")]))
(define-inlinable (pice-class sym)
  (string->symbol (format #f "~a" (string-ref (symbol->string sym) 1))))
(define-inlinable (black? sym) (equal? 'black (pice-color sym)))
(define-inlinable (white? sym) (equal? 'white (pice-color sym)))
(define-inlinable (captureable brd scl srank i j)
  (let ([color (pice-color (chess-ref brd i j))]
	[destp (chess-ref brd i j)])
    (cond
     [(equal? destp 'ee) #t]
     [(equal? color (pice-color destp)) #f]
     [else #t])))
(define-inlinable (enumarte brd next i j)
  (let ([sclr (pice-color (chess-ref brd i j))])
    (let loop ([n (next (cons i j))] [res '()])
      (cond
       [(not (on-board? brd n)) res]
       [(and (not (empty? brd (car n) (cdr n)))
	     (equal? (pice-color (chess-ref brd (car n) (cdr n))) sclr))
	res]
       [(and (not (empty? brd (car n) (cdr n)))
	     (not (equal? (pice-color (chess-ref brd (car n) (cdr n))) sclr)))
	(cons n res)]
       [else (loop (next n) (cons n res))]))))
(define-inlinable (opz-clr? brd i1 j1 i2 j2)
  (not (equal? (pice-color (chess-ref brd i1 j1))
	       (pice-color (chess-ref brd i2 j2)))))
(define (switch-turn brd)
  (board-set-turn! brd (if (white? (board-get-turn brd)) 'black 'white)))

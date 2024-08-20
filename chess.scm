(define-module (chess)
  #:use-module (utils)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (board? piece?  white? black?
		   <board>
		   new-chess-board
		   chess-ref
		   board-get-board   board-set-board!
		   board-get-height  board-set-height!
		   board-get-width   board-set-width!
		   board-get-turn    board-set-turn!
		   board-get-move-no board-set-move-no!
		   board-get-history board-set-history!
		   valid-move? switch-turn))

(define (isin? x l)
  (cond
   [(null? l) #f]
   [(equal? x (car l)) #t]
   [else (isin? x (cdr l))]))
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

(define (pawn-moves brd i j)
  (let* ([pc  (chess-ref brd i j)]
	 [clr (pice-color pc)]
	 [dir (if (black? pc) 1+ 1-)]
	 [p-atk (filter (lambda (p)
			  (and (on-board? brd p)
			       (not (empty? brd (car p) (cdr p)))
			       (not (equal? clr (pice-color (chess-ref brd (car p) (cdr p)))))))
			`((,(dir i) . ,(1- j))
			  (,(dir i) . ,(1+ j))))]
	 [first-move  (if (or (and (= i 1) (equal? clr 'black))
			      (and (= i 6) (equal? clr 'white)))
			  `((,(dir (dir i)) . ,j))
			  '())]
	 [normal-move (filter (lambda (p)
				(and (on-board? brd p)
				     (empty? brd (car p) (cdr p))))
			      `((,(dir i) . ,j)))])
    (append normal-move first-move p-atk)))
(define (rook-moves brd i j)
  (append (enumarte brd
		    (lambda (a)
		      `(,(1+ (car a)) . ,(cdr a)))
		    i j)
	  (enumarte brd
		    (lambda (a)
		      `(,(1- (car a)) . ,(cdr a)))
		    i j)
	  (enumarte brd
		    (lambda (a)
		      `(,(car a) . ,(1+ (cdr a))))
		    i j)
	  (enumarte brd
		    (lambda (a)
		      `(,(car a) . ,(1- (cdr a))))
		    i j)))
(define (bishop-moves brd i j)
  (append (enumarte brd (lambda (a)
			  `(,(1+ (car a)) . ,(1+ (cdr a))))
		    i j)
	  (enumarte brd (lambda (a)
			  `(,(1+ (car a)) . ,(1- (cdr a))))
		    i j)
	  (enumarte brd (lambda (a)
			  `(,(1- (car a)) . ,(1+ (cdr a))))
		    i j)
	  (enumarte brd (lambda (a)
			  `(,(1- (car a)) . ,(1- (cdr a))))
		    i j)))
(define (horse-moves brd i j)
  (filter (lambda (p)
	    (and (on-board? brd p)
		 (or (empty? brd (car p) (cdr p))
		     (opz-clr? brd i j (car p) (cdr p)))))
	  (list (cons (+ 2 i) (1+ j))
		(cons (+ 2 i) (1- j))
		(cons (- i 2) (1+ j))
		(cons (- i 2) (1- j))
		(cons (1+ i) (+ 2 j))
		(cons (1- i) (+ 2 j))
		(cons (1+ i) (- j 2))
		(cons (1- i) (- j 2)))))
(define (queen-moves brd i j)
  (append (rook-moves brd i j)
	  (bishop-moves brd i j)))
(define (king-moves brd i j)
  (let ([b (board-get-board brd)])
    (filter (lambda (p)
	      (and (on-board? brd p)
		   (if (empty? brd (car p) (cdr p))
		       #t
		       (opz-clr? brd i j (car p) (cdr p)))))
	    (list (cons (1+ i) j)
		  (cons (1- i) j)
		  (cons i (1+ j))
		  (cons i (1- j))
		  (cons (1+ i) (1+ j))
		  (cons (1+ i) (1- j))
		  (cons (1- i) (1+ j))
		  (cons (1- i) (1- j))))))
(define move-dispatch
  `((p . ,pawn-moves)
    (r . ,rook-moves)
    (h . ,horse-moves)
    (b . ,bishop-moves)
    (q . ,queen-moves)
    (k . ,king-moves)))
(define (get-mover pice-sym)
  (match-let ([(clr nm) (string->list (symbol->string pice-sym))])
    (assoc-ref move-dispatch (string->symbol (format #f "~a" nm)))))
(define-inlinable (legal-move? brd mv)
  (match-let* ([(scl srank dcl drank) mv]
      [arr   (board-get-board brd)]
      [mover (get-mover (chess-ref brd scl srank))])
    (isin? (cons dcl drank) (mover brd scl srank))))
(define-inlinable (valid-move? brd mv)
  (match-let* ([(i1 j1 i2 j2) mv]
      [arr (board-get-board brd)])
    (and (array-in-bounds? arr i1 j1) ;;out'o bound
	 (array-in-bounds? arr i2 j2)
	 (not (empty? brd i1 j1))
	 (equal? (board-get-turn brd) (pice-color (chess-ref brd i1 j1)))
	 ;;king killer
	 (not (equal? (chess-ref brd i2 j2) 'bk))
	 (not (equal? (chess-ref brd i2 j2) 'wk))
	 (legal-move? brd mv))))
;; test get-type
(define-public (get-type brd mv)
  (match-let* ([(si sj di dj) mv]
      [pice (chess-ref brd si sj)]
      [cls (pice-class pice)]
      [dir (if (black? pice) 1+ 1-)]
      [limit (if (black? pice)
		 7 0)])
    (cond
     [(and (equal? cls 'p) (= di limit)) 'promote]
     [else 'move/aot]))
  'mv/aot)

;;unsafe and no policies
(define occupy
  (case-lambda
    [(brd i1 j1 i2 j2)
     (chess-set! brd (chess-ref brd i1 i2) i2 j2)
     (chess-set! brd 'ee i1 j1)]
    [(brd i j p)
     (chess-set! brd p i j)]))

(define-public (move/aot brd mv)
  (if (not (valid-move? brd mv))
      #f
      'TBI))
(define-public (promote brd pice)
  (board-get-turn brd))
(define-public (winner . a) 'TBI)

(define-module (chess)
  #:use-module (board)
  #:use-module (utils)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (board? piece?  white? black?
		   new-chess-board
		   chess-ref
		   valid-move? switch-turn))

(define (isin? x l)
  (cond
   [(null? l) #f]
   [(equal? x (car l)) #t]
   [else (isin? x (cdr l))]))
;;board
(define (pawn-moves brd i j)
  (define (en-passant-mv? mv)
    (match-let* ([(si sj di dj) mv]
	[($ <board> board _ _ turn _ _) brd]
	[p (chess-ref brd di dj)]
	[pice  (pice-class p)]
	[color (pice-color p)])
      (and (equal? 'p (pice-class (chess-ref brd di dj)))
	   (equal? (if (white? turn) 'black 'turn) color)
	   (= si (if (black? color) 1 6))
	   (= di (if (black? color) 3 4)))))
  (match-let* ([pc  (chess-ref brd i j)]
      [clr (pice-color pc)]
      [dir (if (black? pc) 1+ 1-)]
      [(si sj di dj) (if (null? (board-get-history brd))
			 '(0 0 0 0)
			 (car (board-get-history brd)))]
      [p-atk       (filter (lambda (p)
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
			   `((,(dir i) . ,j)))]
      [en-passant  (lnr (if (en-passant-mv? `(,si ,sj ,di ,dj))
			    `(,(dir i) . ,(cond
					   [(= dj (1- j)) (1- j)]
					   [(= dj (1+ j)) (1+ j)]))
			    '()))])
    (append normal-move first-move p-atk en-passant)))
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
     [else 'move/aot])))
;;unsafe and no policies
(define occupy
  (case-lambda
    [(brd i1 j1 i2 j2)
     (chess-set! brd (chess-ref brd i1 j1) i2 j2)
     (chess-set! brd 'ee i1 j1)]
    [(brd i j p)
     (chess-set! brd p i j)]))

(define-public (move/aot brd mv)
  (apply occupy (cons brd mv))
  (board-set-history! brd (cons mv (board-get-history brd)))
  (match-let* ([(si sj di dj) mv]
      [turn (board-get-turn brd)]
      [piece (chess-ref brd di dj)])
    (if (and (or (equal? piece 'bp) (equal? piece 'bp)) ;;pawn of the turn color
	     (and (black? turn) (= di 7))
	     (and (white? turn) (= di 0)))
	#t #f)))
(define-public (promote brd pice)
  (let* ([turn (if (equal? (board-get-turn brd) 'white) 'w 'b)]
	 [pawn (if (white? turn) 'wp 'bp)]
	 [i (if (white? turn) 0 7)]
	 [j (let loop ([j 0])
	      (cond
	       [(= j 8) (error "no thing to promote to")]
	       [(equal? (chess-ref brd i j) pawn) j]
	       [else (loop (1+ j))]))]
	 [p (string->symbol (format #f "~a~a" turn pice))])
    (occupy brd i j p)))

(define (possible-moves brd i j)
  1)
;;output (('bp (i . j) (i . j)))
(define (under-attack? brd i j)
  (match-let* ([($ <board> board _ _ turn _ _) brd]
      [opp-pices-moves (map (lambda (s)
			      (let* ([pice (string->symbol (string-append (if (black? turn) "w" "b") (symbol->string s)))])
				(cons pice
				      (apply append
					     (map (match-lambda
						    [(i . j) ((get-mover pice) brd i j)])
						  (pice-index brd pice))))))
			    '(p h b r q k))])
    (fold (lambda (a b)
	    (if (null? (cdr a))
		(let ([fmoves (filter (match-lambda [(a . b) (and (= a i) (= j b))]) (cdr a))])
		  (if (null? fmoves)
		      b
		      (cons (cons (car a)
				  fmoves)
			    b)))))
	  '()
	  opp-pices-moves)))

(define-public (ended? brd)
  (match-let* ([($ <board> board height width turn move-no history) brd]
      [p       (match turn ['white 'wk] ['black 'bk])]
      [(i . j) (car (pice-index brd p))]
      [moves   (get-mover p)])
    (and (under-attack? brd i j) (null? moves))))
(define-public (winner . a) 'TBI)

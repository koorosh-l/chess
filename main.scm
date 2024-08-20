#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -L /home/quasikote/proj/chess/ -s "$0"
!#
(use-modules ((chess) #:prefix ch:)
	     ((io) #:prefix io:)
	     (utils)
	     (ice-9 match)
	     (ice-9 textual-ports)
	     (ice-9 suspendable-ports))

(define brd (ch:new-chess-board))
(define-inlinable (get-valid-move brd)
  (let ([mv (io:get-move)])
    (if (ch:valid-move? brd mv)
	mv
	(begin
	  (display "invalid move\n")
	  (get-valid-move brd)))))
(io:clear)
(display (io:print-board brd))
(let gl ([mv (get-valid-move brd)])
  (if (ch:move/aot brd mv)
      (ch:promote brd (io:get-piece)))
  (ch:switch-turn brd)
  (io:clear)
  (display (io:print-board brd))
  (when (not (ch:ended? brd))
    (gl (get-valid-move brd))))

(display (ch:winner brd))  (newline)

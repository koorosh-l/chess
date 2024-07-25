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
(display (io:print-board brd))
(let loop ([mv (io:get-move)])
  ;; (display "----------") (write mv) (newline)
  (match (ch:get-type brd mv)
    ['promote (ch:promote brd (io:get-pice))]
    ['mv/aot (ch:move/aot brd mv)]
    [a (display a) (newline)])
  (io:print-board brd)
  (loop (io:get-move)))

(display (ch:winner brd))  (newline)

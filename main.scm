#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -L /home/quasikote/proj/chess/ -s "$0"
!#
(use-modules ((chess) #:prefix ch:)
	     ((io) #:prefix io:)
	     (ice-9 match))

(define brd (ch:new-chess-board))
(io:print-board brd)

;; (while (not (ch:ended? brd))
;;   (let* ([mv   (io:get-move)]
;;	 [type (ch:get-type brd mv)])
;;     (match type
;;       ['mv/aot  (ch:move/aot brd mv)]
;;       ['promote (ch:move/aot brd mv) (ch:promote  brd (io:get-promote))]))
;;   (io:print-board brd))

(let loop ([mv (io:get-move)])
  (match (ch:get-type brd mv)
    ['promote (ch:promot brd (io:get-pice))]
    ['mv/aot (ch:move/aot brd mv)]
    [a (display a) (newline)])
  (io:print-board brd)
  (loop (io:get-move)))

(display (ch:winner brd))  (newline)

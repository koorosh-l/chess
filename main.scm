#!/home/koorosh/.guix-profile/bin/guile \ 
-L /home/koorosh/proj/
-s
!#
(use-modules (chess chess))
(let main ([board (make-chess-board)]
	   [turn 'white])
  board
  (move-piece! board turn (sq 2 1) (sq 2 2)))

#!/usr/bin/racket
#lang racket		;; These lines required to run the program without DrRacket

; =======================
; || Christopher Myers ||
; =======================


; A cmd is one of:
;  -(make-JMPOBJ object number number)		-Jump object to x,y
;  -(make-JMPOBJRAND object)			-Jump object to random coords
;  -(make-STOPOBJ object)			-Stop object from movement
;  -(make-ADDOBJ object)			-Add object
;  -(make-UDTOBJ object)			-Update object
;  -(make-DELOBJ object)			-Delete object
;  -(make-COLLIDE? object object)		-Object colliding with object?
;  -(make-CLEAR? object)			-Object not colliding with edge?
;  -(make-WHILE cmd list[cmd])			-Do while cmd returns true
;  -(make-IFCOND cmd list[cmd] list[cmd])	-If cmd returns true, execute first cmdlist, otherwise the second cmdlist
;  -(make-DEFUN symbol list[cmd])		-Define function
;  -(make-CALL symbol)				-Call function
;  -(make-GOTO symbol)				-Goto label
;  -(make-LABEL symbol)				-Define label
;  -(make-GENCIRCLE number symbol)		-Generate circle
;  -(make-GENRECT number number symbol)		-Generate rectangle
;
;
; An object is one of
;  -symbol
;  -(make-entity symbol graphic number number number number)
;
; A graphic is one of
;  -symbol
;  -(make-GENCIRCLE number symbol)
;  -(make-GENRECT number number symbol)


; A JMPOBJ is (make-JMPOBJ object number number)
(define-struct JMPOBJ (obj nx ny))

; A JMPOBJ is (make-JMPOBJRAND object)
(define-struct JMPOBJRAND (obj))

; A STOPOBJ is (make-STOPOBJ object)
(define-struct STOPOBJ (obj))

; A ADDOBJ is (make-ADDOBJ object)
(define-struct ADDOBJ (obj))

; A UDTOBJ is (make-UDTOBJ object)
(define-struct UDTOBJ (obj))

; A DELOBJ is (make-DELOBJ object)
(define-struct DELOBJ (obj))

; A COLLIDE? is (make-COLLIDE? object object)
(define-struct COLLIDE? (obj1 obj2))

; A CLEAR? is (make-CLEAR? object)
(define-struct CLEAR? (obj))

; A WHILE is (make-WHILE cmd list[cmd])
(define-struct WHILE (cnd cmds))

; A IFCOND is (make-IFCOND cmd list[cmd] list[cmd])
(define-struct IFCOND (cnd ctrue cfalse))

; A DEFUN is (make-DEFUN symbol number list[cmd])
(define-struct DEFUN (name cmds))

; A CALL is (make-CALL symbol)
(define-struct CALL (name))

; A GOTO is (make-GOTO symbol)
(define-struct GOTO (label))

; A LABEL is (make-LABEL symbol)
(define-struct LABEL (name))

; A GENCIRCLE is (make-GENCIRCLE number symbol)
(define-struct GENCIRCLE (rad color))

; A GENRECT is (make-GENRECT number number symbol)
(define-struct GENRECT (h w color))

; An object is (make-object symbol graphic number number number number)
(define-struct object (name sprite posx posy velx vely))


#| Animation 1: 
	"A red ball moving at a angle towards a blue wall until it hits the wall. 
	At that point, the wall disappears and the ball moves back towards the left 
	edge of the canvas, stopping when it hits the left edge of the canvas."
|#
(define anim-sample1 (list
		       (make-ADDOBJ (make-object 'rcirc (make-GENCIRCLE 20 'red) 100 100 5 3))
		       (make-ADDOBJ (make-object 'bwall (make-GENRECT 400 20 'blue) 600 50 0 0))
		       (make-LABEL 'loop1)		;; Demonstrate GOTO functionality
		       (make-UDTOBJ 'rcirc)
		       (make-IFCOND (make-COLLIDE? 'rcirc 'bwall)
				    empty
				    (list (make-GOTO 'loop1)))
		       (make-DELOBJ 'bwall)
		       (make-DELOBJ 'rcirc) ;; Needs to be recreated. I assume that the animator would know the location of the collision?
		       (make-ADDOBJ (make-object 'rcirc (make-GENCIRCLE 20 'red) 600 340 -5 0))
		       (make-WHILE (CLEAR? 'rcirc)
				   (make-UDTOBJ 'rcirc))
		       (make-STOPOBJ 'rcirc)))



#| Animation 2:
	"A purple circle jumping to random locations around the canvas until it hits 
	the top edge of the canvas."
|#
(define anim-sample2 (list
		       (make-ADDOBJ (make-object 'pcirc (make-GENCIRCLE 20 'purple) 400 300 0 0))
		       (make-WHILE (make-CLEAR? 'pcirc) (list
							 (make-JMPOBJRAND 'pcirc)))
		       (make-STOPOBJ 'pcirc)))



#| Animation 3:
	"An orange circle dropping straight down until it hits the green rectangle.
	At that point, the red rectangle appears and the circle moves right until
	it hits the red rectangle, after which the orange circle jumps to a random
	location and stops."
|#
(define anim-sample3 (list
		       (make-ADDOBJ (make-object 'ocirc (make-GENCIRCLE 20 'orange) 100 1 0 5))
		       (make-ADDOBJ (make-object 'grect (make-GENRECT 50 750 'green) 25 540 0 0))
		       (make-LABEL 'loop1)
		       (make-UDTOBJ 'ocirc)
		       (make-IFCOND (make-COLLIDE? 'ocirc 'grect)
				    empty
				    (list (make-GOTO 'loop1)))
		       (make-ADDOBJ (make-object 'rrect (make-GENRECT 540 50 'red) 750 25 0 0))
		       (make-DELOBJ 'ocirc)
		       (make-ADDOBJ (make-object 'ocirc (make-GENCIRCLE 20 'orange) 100 740 5 0))
		       (make-LABEL 'loop2)
		       (make-UDTOBJ 'ocirc)
		       (make-IFCOND (make-COLLIDE? 'ocirc 'rrect)
				    empty
				    (list (make-GOTO 'loop2)))
		       (make-STOPOBJ 'ocirc)
		       (make-JMPOBJRAND 'ocirc)))



#| Animation 4 (custom)
	"A black ball bounces endlessly up and down."
|#
(define anim-sample4 (list
		       (make-WHILE true (list
					  (make-ADDOBJ (make-object 'bcirc (make-GENCIRCLE 20 'black) 300 1 0 5))
					  (make-WHILE (make-CLEAR? 'bcirc) (list
									     (make-UDTOBJ 'bcirc)))
					  (make-DELOBJ 'bcirc)
					  (make-ADDOBJ (make-object 'bcirc (make-GENCIRCLE 20 'black) 300 599 0 -5))
					  (make-WHILE (make-CLEAR? 'bcirc) (list
									     (make-UDTOBJ 'bcirc)))
					  (make-DELOBJ 'bcirc)))))
					  


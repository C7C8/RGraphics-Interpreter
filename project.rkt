#!/usr/bin/racket
#lang racket		;; These lines required to run the program without DrRacket
(require test-engine/racket-tests)

; =======================
; || Christopher Myers ||
; =======================


; =====================
; || LANGUAGE DESIGN ||
; =====================

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
;  -(make-NOTCOND cmd)				-Inverts the return from any given boolean command.
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

; A NOT is (make-NOTCOND cmd)
(define-struct NOTCOND (cnd))

; A GENCIRCLE is (make-GENCIRCLE number symbol)
(define-struct GENCIRCLE (rad color))

; A GENRECT is (make-GENRECT number number symbol)
(define-struct GENRECT (h w color))

; An object is (make-object symbol graphic number number number number)
(define-struct object (name sprite posx posy velx vely))



; ========================
; || EXAMPLE ANIMATIONS ||
; ========================


#| Animation 1: 
	"A red ball moving at a angle towards a blue wall until it hits the wall. 
	At that point, the wall disappears and the ball moves back towards the left 
	edge of the canvas, stopping when it hits the left edge of the canvas."
|#
(define anim-sample1 (list
		       (make-ADDOBJ (make-object 'rcirc (make-GENCIRCLE 20 'red) 100 100 5 3))
		       (make-ADDOBJ (make-object 'bwall (make-GENRECT 400 20 'blue) 600 50 0 0))
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'rcirc 'bwall))
				   (list (make-UDTOBJ 'rcirc)))
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
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'ocirc 'grect))
				   (list (make-UDTOBJ 'ocirc)))
		       (make-ADDOBJ (make-object 'rrect (make-GENRECT 540 50 'red) 750 25 0 0))
		       (make-DELOBJ 'ocirc)
		       (make-ADDOBJ (make-object 'ocirc (make-GENCIRCLE 20 'orange) 100 740 5 0))
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'ocirc 'rrect))
				   (list (make-UDTOBJ 'ocirc)))
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



; =================
; || INTERPRETER ||
; =================


; =============================
; CORE MEMORY FUNCTIONS


;; list[object]
(define core empty)


;; in-core?: symbol -> boolean
;; Consumes a symbol and returns true if that an object by that names exists
;; in core memory.
(check-expect (in-core? 'DNE) false) ;; Works because core is defined as empty above.
(define (in-core? name)
  (not (empty? (filter (lambda (obj)(symbol=? name (object-name obj))) core))))


;; get-object: symbol -> object
;; Conusmes a symbol and returns the object associated with that symbol.
;; No test cases because this relies on a global variable.
(define (get-object name)
  (if (not (in-core? name))
    (error (format "Object \"~a\" does not exist!" (symbol->string name))) ;; ooh, this doesn't look like normal racket syntax!
    (first (filter (lambda (obj)(symbol=? name (object-name obj))) core)))) ;; is symbol->string macro-involved?
	

;; stor-object: object -> void
;; Consumes an object and pushes it to core memory, overwriting
;; anything else under the same name already there. Once again, no test
;; cases as this too relies on a global variable AND returns void.
(define (stor-object obj)
  (set! core
    (if (in-core? (object-name obj)) ;; Allows for addition of new variables
      (map (lambda (o)	
	     (if (symbol=? (object-name o) (object-name obj))
	       obj ; Replace prior-existing variable under given name
	       o))
	   core)
      (append obj core))))


;; del-obj: symbol -> void
;; Consumes an object name (symbol) and deletes it entirely
;; from core memory.
(define (del-obj name)
  (set! core
    (map (lambda (obj)
	   (if (symbol=? (object-name obj) name)
	     empty
	     obj)))))


; =============================
; INTERPRETER FUNCTIONS
; (for real this time!)


;; mov-obj: number number symbol -> void
;; Consumes two numbers and an object name, and moves
;; the object to the coordinates given by the number pair.
(define (move-obj newx newy name)
  (stor-object
    (make-object
      name
      newx
      newy
      (object-velx (get-object name))
      (object-vely (get-object name)))))



;; big-crunch: list[cmd] -> void
;; Runs the program contained within a list of commands.
#|(define (big-crunch cmdlist)
  (for-each exec-cmd cmdlist))|#


;; exec-cmd: cmd -> void
;; Executes a given command. Doesn't accept commands that
;; return booleans.
#|(define (exec-cmd cmd)
  (cond [(JMPOBJ? cmd)
	 
	[(JMPOBJRAND? cmd)
	 ...]
	[(STOPOBJ? cmd)
	 ...]
	[(ADDOBJ? cmd)
	 ...]
	[(UDTOBJ? cmd)
	 ...]
	[(DELOBJ? cmd)
	 ...]
	[(WHILE? cmd)
	 ...]
	[(IFCOND? cmd)
	 ...]
	[(NOTCOND? cmd)
	 ...]
	[(GENCIRCLE? cmd)
	 ...]
	[(GENRECT? cmd)
	 ...]
	[else
	  (error "invalid command")]))
|#

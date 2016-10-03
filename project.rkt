#!/usr/bin/racket
#lang racket		;; These lines required to run the program without DrRacket
(require test-engine/racket-tests)
(require htdp/image)
(require "world-cs1102.rkt")

; =======================
; || Christopher Myers ||
; =======================


; =====================
; || LANGUAGE DESIGN ||
; =====================

; A cmd is one of:
;  -(make-JMPOBJ symbol number number)		-Jump object to x,y
;  -(make-JMPOBJRAND symbol)			-Jump object to random coords
;  -(make-STOPOBJ symbol)			-Stop object from movement
;  -(make-ADDOBJ symbol)			-Add object
;  -(make-UDTOBJ symbol)			-Update object
;  -(make-DELOBJ symbol)			-Delete object
;  -(make-WHILE condcmd list[cmd])		-Do while cmd returns true
;  -(make-IFCOND condcmd list[cmd] list[cmd])	-If cmd returns true, execute first cmdlist, otherwise the second cmdlist
;
;
; A condcmd is one of
;  -(make-COLLIDE? symbol symbol)
;  -(make-EDGECOLLIDE? symbol)
;  -(make-NOTCOND condcmd)
;
; An object is one of
;  -(make-object symbol graphic number number number number)
;
; A graphic is one of
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

; A EDGECOLLIDE? is (make-EDGECOLLIDE? object)
(define-struct EDGECOLLIDE? (obj))

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
		       (make-WHILE ((make-NOTCOND (make-EDGECOLLIDE? 'rcirc))
				   (make-UDTOBJ 'rcirc))
		       (make-STOPOBJ 'rcirc)))



#| Animation 2:
	"A purple circle jumping to random locations around the canvas until it hits 
	the top edge of the canvas."
|#
(define anim-sample2 (list
		       (make-ADDOBJ (make-object 'pcirc (make-GENCIRCLE 20 'purple) 400 300 0 0))
		       (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'pcirc) (list
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
					  (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'bcirc)) (list
												  (make-UDTOBJ 'bcirc)))
					  (make-DELOBJ 'bcirc)
					  (make-ADDOBJ (make-object 'bcirc (make-GENCIRCLE 20 'black) 300 599 0 -5))
					  (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'bcirc)) (list
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
    (error (format "Cannot retrieve object \"~a\" - does not exist!" (symbol->string name))) ;; ooh, this doesn't look like normal racket syntax!
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


(define WIN_X 800)
(define WIN_Y 600)


;; mov-obj: number number symbol -> void
;; Consumes two numbers and an object name, and moves
;; the object to the coordinates given by the number pair.
(define (move-obj nx ny name)
  (if (in-core? name)
    (stor-object
      (make-object
        name
        nx
        ny
        (object-velx (get-object name))
        (object-vely (get-object name))))
    (error (format "Cannot move object \"~a\" - does not exist!" (symbol->string name)))))



;; big-crunch: list[cmd] -> void
;; Runs the program contained within a list of commands.
(define (big-crunch cmdlist)
  (for-each exec-cmd cmdlist))


;; exec-cmd: cmd -> void
;; Executes a given command. Doesn't accept commands that
;; return booleans.
(define (exec-cmd cmd)
  (cond [(JMPOBJ? cmd)
	 (move-obj (JMPOBJ-name cmd) (JUMPOBJ-nx cmd) (JMPOBJ-ny cmd))]

	[(JMPOBJRAND? cmd)
	 (move-obj (JMPOBJ-name cmd) (random WIN_X) (random WIN_Y))]

	[(STOPOBJ? cmd)
	 (stor-obj
	   (make-object
	     (STOPOBJ-obj cmd)					; Name
	     (object-sprite (get-object (STOPOBJ-obj cmd)))	; Sp(r)ite
	     (object-posx (get-object (STOPOBJ-obj cmd)))	; Posx
	     (object-posy (get-object (STOPOBJ-obj cmd)))	; Posy
	     0							; Velx
	     0))]						; Vely

	[(ADDOBJ? cmd)
	 (stor-obj (ADDOBJ-obj cmd))]

	[(UDTOBJ? cmd)	; maybe split this off into an exec-UDTOBJ?
	 (stor-obj
	   (make-object
	     (UDTOBJ-obj cmd)					; Name
	     (object-sprite (get-object (UDTOBJ-obj cmd)))	; Sprite
	     (+ 
	       (object-posx (get-object (UDTOBJ-obj cmd)))	; Update posx
	       (object-velx (get-object (UDTOBJ-obj cmd))))
	     (+ 
	       (object-posy (get-object (UDTOBJ-obj cmd)))	; Update posy
	       (object-vely (get-object (UDTOBJ-obj cmd))))
	     (object-velx (get-object (UDTOBJ-obj cmd)))	; Velx
	     (object-vely (get-object (UDTOBJ-obj cmd)))))]	; Vely

	[(DELOBJ? cmd)
	 (del-obj (DELOBJ-name cmd))]

	[(WHILE? cmd)
	 (exec-while cmd)]

	[(IFCOND? cmd)
	 (if (eval-condcmd (IFCOND-cond cnd))
	   (big-crunch (IFCOND-ctrue cmd))
	   (big-crunch (IFCOND-cfalse cmd)))]

	[else
	  (error "invalid command")]))


;; eval-condcmd: cmd -> boolean
;; Consumes a conditional command (COLLIDE?, EDGECOLLIDE?, NOTCOND)
;; and returns the evaluation of that command in boolean form.
(define (eval-condcmd cmd)
  (cond [(NOTCOND? cmd)
	 (not (eval-condcmd cmd))]
	[(EDGECOLLIDE?? cmd) ; This feels strange
	 (or
	   (> 0 (object-posx (get-object (EDGECOLLIDE?-obj cmd))))
	   (< WIN_X (object-posx (get-object (EDGECOLLIDE?-obj cmd))))
	   (> 0 (object-posy (get-object (EDGECOLLIDE?-obj cmd))))
	   (< WIN_Y (object-posy (get-object (EDGECOLLIDE?-obj cmd)))))]
	[(COLLIDE?? cmd) ; Yeah, really strange!
	 (overlap? (COLLIDE?-obj1 cmd) (COLLIDE?-obj2 cmd))]))


;; overlap?: object object -> boolean
;; Consumes an two objects and returns true if their graphics overlap.
;; This is supposed to be fairy high-precision, it should be "simple".
(define (overlap? obj1 obj2)
  (local [(define (circ-to-rect circ)			; Cheat at circle collision detection. Circles have corners, right?
	   (make-object
	     (object-name circ)
	     (make-GENRECT (GENCIRCLE-rad (object-sprite circ))
	        	   (GENCIRCLE-rad (object-sprite circ))
			   (GENCIRCLE-color (object-sprite circ)))
	      (object-posx circ)
	      (object-posy circ)
	      0 0))
	  (define (intersect-rect obj1 obj2)
	    (nand	; Return true if none of the failure conditions are true
	      (> (object-posx obj1)			; obj2 1 is to the right of obj2
	         (GENRECT-w (object-sprite obj2)))
              (> (object-posx obj2)			; obj2 1 is to the right of obj1
	         (GENRECT-w (object-sprite obj1)))
              (> (object-posy obj1)
	         (GENRECT-h (object-sprite obj2)))
       	      (> (object-posy obj2)
	         (GENRECT-h (object-sprite obj1)))))]
    (intersect-rect (if (GENCIRCLE? obj1)
		      (circ-to-rect obj1)
		      obj1)
		    (if (GENCIRCLE? obj2)
		      (circ-to-rect obj2)
		      obj2))))

;; exec-while: WHILE -> void
(define (exec-while cmd)
  (if (eval-condcmd (WHILE-cnd))
    (begin
      (big-cruch (WHILE-cmds cmd))
      (exec-while cmd)))) ; no else case. Surely this works fantastically!

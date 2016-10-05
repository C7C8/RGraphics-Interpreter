#!/usr/bin/racket
(require test-engine/racket-tests)
(require "world-cs1102.rkt")

; =======================
; || Christopher Myers ||
; =======================


; =====================
; || LANGUAGE DESIGN ||
; =====================

; A cmd is one of:
;  -(make-JMPOBJ symbol number number)		-Jump gobject to x,y
;  -(make-JMPOBJRAND symbol)			-Jump gobject to random coords
;  -(make-STOPOBJ symbol)			-Stop gobject from movement
;  -(make-ADDOBJ symbol)			-Add gobject
;  -(make-UDTOBJ symbol)			-Update gobject
;  -(make-DELOBJ symbol)			-Delete gobject
;  -(make-WHILE condcmd list[cmd])		-Do while cmd returns true
;  -(make-IFCOND condcmd list[cmd] list[cmd])	-If cmd returns true, execute first cmdlist, otherwise the second cmdlist
;
;
; A condcmd is one of
;  -(make-COLLIDE? symbol symbol)
;  -(make-EDGECOLLIDE? symbol)
;  -(make-NOTCOND condcmd)
;
; An gobject is one of
;  -(make-gobject symbol graphic number number number number)
;
; A graphic is one of
;  -(make-GENCIRCLE number symbol)
;  -(make-GENRECT number number symbol)


; A JMPOBJ is (make-JMPOBJ gobject number number)
(define-struct JMPOBJ (obj nx ny))

; A JMPOBJ is (make-JMPOBJRAND gobject)
(define-struct JMPOBJRAND (obj))

; A STOPOBJ is (make-STOPOBJ gobject)
(define-struct STOPOBJ (obj))

; A ADDOBJ is (make-ADDOBJ gobject)
(define-struct ADDOBJ (obj))

; A UDTOBJ is (make-UDTOBJ gobject)
(define-struct UDTOBJ (obj) (make-inspector))

; A DELOBJ is (make-DELOBJ gobject)
(define-struct DELOBJ (obj))

; A COLLIDE? is (make-COLLIDE? gobject gobject)
(define-struct COLLIDE? (obj1 obj2))

; A EDGECOLLIDE? is (make-EDGECOLLIDE? gobject)
(define-struct EDGECOLLIDE? (obj))

; A WHILE is (make-WHILE cmd list[cmd])
(define-struct WHILE (cnd cmds) (make-inspector))

; A IFCOND is (make-IFCOND cmd list[cmd] list[cmd])
(define-struct IFCOND (cnd ctrue cfalse))

; A NOT is (make-NOTCOND cmd)
(define-struct NOTCOND (cnd))

; A GENCIRCLE is (make-GENCIRCLE number symbol)
(define-struct GENCIRCLE (rad color) (make-inspector))

; A GENRECT is (make-GENRECT number number symbol)
(define-struct GENRECT (h w color) (make-inspector))

; An gobject is (make-gobject symbol graphic number number number number)
(define-struct gobject (name sprite posx posy velx vely) (make-inspector))



; ========================
; || EXAMPLE ANIMATIONS ||
; ========================


#| Animation 1: 
	"A red ball moving at a angle towards a blue wall until it hits the wall. 
	At that point, the wall disappears and the ball moves back towards the left 
	edge of the canvas, stopping when it hits the left edge of the canvas."
|#
(define anim-sample1 (list
		       (make-ADDOBJ (make-gobject 'rcirc (make-GENCIRCLE 20 'red) 100 100 1 0.5))
		       (make-ADDOBJ (make-gobject 'bwall (make-GENRECT 400 20 'blue) 600 50 0 0))
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'rcirc 'bwall))
				   (list (make-UDTOBJ 'rcirc)))
		       (make-DELOBJ 'bwall)
		       (make-DELOBJ 'rcirc) ;; Needs to be recreated. I assume that the animator would know the location of the collision?
		       (make-ADDOBJ (make-gobject 'rcirc (make-GENCIRCLE 20 'red) 600 340 -5 0))
		       (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'rcirc))
				   (make-UDTOBJ 'rcirc))
		       (make-STOPOBJ 'rcirc)))



#| Animation 2:
	"A purple circle jumping to random locations around the canvas until it hits 
	the top edge of the canvas."
|#
(define anim-sample2 (list
		       (make-ADDOBJ (make-gobject 'pcirc (make-GENCIRCLE 20 'purple) 400 300 0 0))
		       (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'pcirc)) (list
									      (make-JMPOBJRAND 'pcirc)))
		       (make-STOPOBJ 'pcirc)))



#| Animation 3:
	"An orange circle dropping straight down until it hits the green rectangle.
	At that point, the red rectangle appears and the circle moves right until
	it hits the red rectangle, after which the orange circle jumps to a random
	location and stops."
|#
(define anim-sample3 (list
		       (make-ADDOBJ (make-gobject 'ocirc (make-GENCIRCLE 20 'orange) 100 1 0 5))
		       (make-ADDOBJ (make-gobject 'grect (make-GENRECT 50 750 'green) 25 540 0 0))
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'ocirc 'grect))
				   (list (make-UDTOBJ 'ocirc)))
		       (make-ADDOBJ (make-gobject 'rrect (make-GENRECT 540 50 'red) 750 25 0 0))
		       (make-DELOBJ 'ocirc)
		       (make-ADDOBJ (make-gobject 'ocirc (make-GENCIRCLE 20 'orange) 100 740 5 0))
		       (make-WHILE (make-NOTCOND (make-COLLIDE? 'ocirc 'rrect))
				   (list (make-UDTOBJ 'ocirc)))
		       (make-STOPOBJ 'ocirc)
		       (make-JMPOBJRAND 'ocirc)))



#| Animation 4 (custom)
	"A black ball bounces endlessly up and down."
|#
(define anim-sample4 (list
		       (make-WHILE true (list
					  (make-ADDOBJ (make-gobject 'bcirc (make-GENCIRCLE 20 'black) 300 10 0 5))
					  (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'bcirc)) (list
												  (make-UDTOBJ 'bcirc)))
					  (make-DELOBJ 'bcirc)
					  (make-ADDOBJ (make-gobject 'bcirc (make-GENCIRCLE 20 'black) 300 590 0 -5))
					  (make-WHILE (make-NOTCOND (make-EDGECOLLIDE? 'bcirc)) (list
												  (make-UDTOBJ 'bcirc)))
					  (make-DELOBJ 'bcirc)))))



; =================
; || INTERPRETER ||
; =================


; =============================
; CORE MEMORY FUNCTIONS


;; list[gobject]
(define core empty)


;; in-core?: symbol -> boolean
;; Consumes a symbol and returns true if that an gobject by that names exists
;; in core memory.
(define (in-core? name)
  (obj-in-list? name core))


;; obj-in-list? symbol list[obj] -> boolean
;; Consumes a symbol and a list of gobjects and retrusn true
;; if that gobject is present in the given list.
(check-expect (obj-in-list? 'DNE core) false) ;; Works because core is defined as empty above.
(check-expect (obj-in-list? 'DE (list (make-gobject 'DE 0 0 0 0 0))) true)
(define (obj-in-list? name lst)
  (not (empty? (filter (lambda (obj)(symbol=? name (gobject-name obj))) lst))))


;; get-gobject: symbol -> gobject
;; Conusmes a symbol and returns the gobject associated with that symbol.
;; No test cases because this relies on a global variable.
(define (get-gobject name)
  (if (in-core? name)
    (first (filter (lambda (obj)(symbol=? name (gobject-name obj))) core))
    (error (format "Cannot retrieve gobject \"~a\" - does not exist!~n" (symbol->string name)))))
	

;; stor-obj: gobject -> void
;; Consumes an gobject and pushes it to core memory, overwriting
;; anything else under the same name already there.
(define (stor-obj obj)
  (set! core (stor-obj-in-list obj core)))


;; stor-obj-in-list: gobject list[gobject] -> list[gobject]
;; Consumes an gobject and pushes it to the given list, overwriting
;; anything else under the same name already there.
(check-expect (stor-obj-in-list (make-gobject 'O1 0 0 0 0 0) empty)				; Storing gobjects in an empty list
	      (list (make-gobject 'O1 0 0 0 0 0)))
(check-expect (stor-obj-in-list (make-gobject 'O2 0 0 0 0 0) (list (make-gobject 'O1 0 0 0 0 0)))	; Storing gobjects in a list with one gobject
	      (list (make-gobject 'O2 0 0 0 0 0)
               (make-gobject 'O1 0 0 0 0 0)))
(check-expect (stor-obj-in-list (make-gobject 'O3 0 0 0 0 0) (list (make-gobject 'O1 0 0 0 0 0)	; Storing gobjects in a list with two gobjects
								  (make-gobject 'O2 0 0 0 0 0)))
	      (list (make-gobject 'O3 0 0 0 0 0)
               (make-gobject 'O1 0 0 0 0 0)
		    (make-gobject 'O2 0 0 0 0 0)))
(check-expect (stor-obj-in-list (make-gobject 'O1 1 0 0 0 0) (list (make-gobject 'O1 0 0 0 0 0)))  ; Overwriting an gobject
	      (list (make-gobject 'O1 1 0 0 0 0)))

(define (stor-obj-in-list obj lst)
  (if (obj-in-list? (gobject-name obj) lst) ;; Allows for addition of new variables
    (map (lambda (o)
	   (if (symbol=? (gobject-name o) (gobject-name obj))
	     obj ; Replace prior-existing variable under given name
	     o))
	 lst)
    (if (empty? lst)	;; If the core is empty, make a list out of an incoming gobject
      (list obj)
      (cons obj lst))))

;; del-obj: symbol -> void
;; Consumes an gobject name (symbol) and deletes it entirely
;; from core memory.
(define (del-obj name)
  (set! core
    (filter (lambda (obj)
	      (not (symbol=? name (gobject-name obj))))
	 core)))


; =============================
; INTERPRETER FUNCTIONS
; (for real this time!)


(define WIN_X 800)
(define WIN_Y 600)
(define SKIPTIME 0.016)


;; big-crunch: list[cmd] -> void
;; Runs the program contained within a list of commands.
(define (big-crunch cmdlist)
  (for-each (lambda (cmd)
	      (begin
		(exec-cmd cmd)
		(core-dump)
		(sleep/yield SKIPTIME)))
	    cmdlist))


;; mov-obj: number number symbol -> void
;; Consumes two numbers and an gobject name, and moves
;; the gobject to the coordinates given by the number pair.
(define (move-obj nx ny name)
  (if (in-core? name)
    (stor-obj
      (make-gobject
        name
	(gobject-sprite (get-gobject name))
        nx
        ny
        (gobject-velx (get-gobject name))
        (gobject-vely (get-gobject name))))
    (error (format "Cannot move gobject \"~a\" - does not exist!" (symbol->string name)))))


;; exec-cmd: cmd -> void
;; Executes a given command. Doesn't accept commands that
;; return booleans.
(define (exec-cmd cmd)
  (cond [(JMPOBJ? cmd)
	 (move-obj (JMPOBJ-nx cmd) (JMPOBJ-ny cmd) (JMPOBJ-obj cmd))]

	[(JMPOBJRAND? cmd)
	 (move-obj (random WIN_X) (random WIN_Y) (JMPOBJRAND-obj cmd))]

	[(STOPOBJ? cmd)
	 (stor-obj
	   (make-gobject
	     (STOPOBJ-obj cmd)					; Name
	     (gobject-sprite (get-gobject (STOPOBJ-obj cmd)))	; Sp(r)ite
	     (gobject-posx (get-gobject (STOPOBJ-obj cmd)))	; Posx
	     (gobject-posy (get-gobject (STOPOBJ-obj cmd)))	; Posy
	     0							; Velx
	     0))]						; Vely

	[(ADDOBJ? cmd)
	 (stor-obj (ADDOBJ-obj cmd))]

	[(UDTOBJ? cmd)	; maybe split this off into an exec-UDTOBJ?
	 (move-obj
	     (+ 
	       (gobject-posx (get-gobject (UDTOBJ-obj cmd)))	; Update posx
	       (gobject-velx (get-gobject (UDTOBJ-obj cmd))))
	     (+ 
	       (gobject-posy (get-gobject (UDTOBJ-obj cmd)))	; Update posy
	       (gobject-vely (get-gobject (UDTOBJ-obj cmd))))
	     (UDTOBJ-obj cmd))]

	[(DELOBJ? cmd)
	 (del-obj (DELOBJ-obj cmd))]

	[(WHILE? cmd)
	 (exec-while cmd)]

	[(IFCOND? cmd)
	 (if (eval-condcmd (IFCOND-cnd cmd))
	   (big-crunch (IFCOND-ctrue cmd))
	   (big-crunch (IFCOND-cfalse cmd)))]

	[else
	  (error (format "invalid command: ~a~n" cmd))]))

;; eval-condcmd: cmd -> boolean
;; Consumes a conditional command (COLLIDE?, EDGECOLLIDE?, NOTCOND)
;; and returns the evaluation of that command in boolean form.
;; Note: EDGECOLLIDE defines its edges as 10px inside the actual window border.
(define (eval-condcmd cmd)
  (cond [(NOTCOND? cmd)
         (not (eval-condcmd (NOTCOND-cnd cmd)))] ; This used to read "(eval-condcmd cmd)", resulting in an infinitely expanding stack. FUN!
        [(EDGECOLLIDE?? cmd)
         (or
          (< (gobject-posx (get-gobject (EDGECOLLIDE?-obj cmd))) 10)
          (> (gobject-posx (get-gobject (EDGECOLLIDE?-obj cmd))) (- WIN_X 10))
          (< (gobject-posy (get-gobject (EDGECOLLIDE?-obj cmd))) 10 )
          (> (gobject-posy (get-gobject (EDGECOLLIDE?-obj cmd))) (- WIN_Y 10)))]
        [(COLLIDE?? cmd) 
         (overlap? (get-gobject (COLLIDE?-obj1 cmd)) (get-gobject (COLLIDE?-obj2 cmd)))]))


;; overlap?: gobject gobject -> boolean
;; Consumes an two gobjects and returns true if their graphics overlap.
(define (overlap? o1 o2)
  (local [(define (to-rect circ)  ; Cheat at circle collision detection. Circles have corners, right?
            (if (GENCIRCLE? circ)
                (make-GENRECT (GENCIRCLE-rad circ)
                              (GENCIRCLE-rad circ)
                              (GENCIRCLE-color circ))
                circ))]
    (nand	                                        ; Return true if none of the failure conditions are true
     (> (gobject-posx o1)		              	; obj2 1 is to the right of obj2
        (GENRECT-w (to-rect (gobject-sprite o2))))
     (> (gobject-posx o2)		                ; obj2 1 is to the right of obj1
        (GENRECT-w (to-rect (gobject-sprite o1))))
     (> (gobject-posy o1)
        (GENRECT-h (to-rect (gobject-sprite o2))))
     (> (gobject-posy o2)
        (GENRECT-h (to-rect (gobject-sprite o1)))))))

;; exec-while: WHILE -> void
;; Executes a WHILE command, recursively.
(define (exec-while cmd)
  (if (eval-condcmd (WHILE-cnd cmd))
      (begin
        (big-crunch (WHILE-cmds cmd)) ; what the f**k?
        (exec-while cmd))
      (void)))


;; core-dump: void -> void
;; Takes the core and dumps it. Just kidding, this displays all gobjects
;; in the core and writes them to the screen.
;; Yes, all the "core" terminology from earlier was a buildup to this.
(define (core-dump)
  (local [(define (sprite-to-img spr)
            (cond [(GENRECT? spr)
                   (rectangle (GENRECT-w spr)
                              (GENRECT-h spr)
                              "solid"
                              (GENRECT-color spr))]
                  [(GENCIRCLE? spr)
                   (circle (GENCIRCLE-rad spr)
                           "solid"
                           (GENCIRCLE-color spr))]))
          (define (render-objlist lst) ;; Returns a scene.
            (cond [(cons? lst)
                   (place-image (sprite-to-img (gobject-sprite (first lst)))
                                (gobject-posx (first lst))
                                (gobject-posy (first lst))
                                (render-objlist (rest lst)))]
                  [else (empty-scene WIN_X WIN_Y)]))]
    (update-frame (render-objlist core))))


(create-canvas WIN_X WIN_Y) 
(big-crunch anim-sample4)

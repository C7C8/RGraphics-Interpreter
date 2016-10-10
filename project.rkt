(require test-engine/racket-tests)
(require "world-cs1102.rkt")

#| Christopher Myers 2016

====================
|| PROJECT REPORT ||
====================

1. In order to run this program, you need only execute "(run-animation anim)" where
   "anim" is a list of commands. The four defined animations (detailed near the end
   of this file) are anim-sample1, anim-sample2, anim-sample3, and anim-sample4.
 
2. This interpreter is fully functional - all required features work flawlessly as
   far as I can tell. It includes error checking for invalid commands and a series
   of macros to make the language usable without any racket syntax or keyword
   involvement. In the event of bad code, it will either throw the appropriate error
   (e.g. "error: bad conditional command JMPOBJ") or will yield a syntax error.

3. I changed a few things in my design since the original design submission, most
   notably in the removal of GOTO/LABEL and the extraction of conditional commands
   where relevant. GOTO/LABEL were removed as their implementation would've been tough
   and they were ultimately unnecessary (plus a late night code comment "hello again
   spaghetti" recovered in the morning gave me an idea of where this would lead).
   Conditional commands were ultimately extracted into a separate category of structs
   "condcmd" for clarity. Additionally, the command "NOT" was added to clear up cases
   where checking for when something is *not* the case applies.

4. I am largely satisfied with my interpreter. I believe a few things could be a bit
   cleaner ("exec-cmd" in particular makes me cringe, but the alternative of extracting
   helper functions from it is worse), but the code is mostly good in my humble opinion.
   I regret that I was unable to add more break conditions (like "break when object <x>
   arrives at point y"), but I feel that loss is somewhat compensated by the
   macro-generating macro I wrote for defining the deracketified syntax of my language.

   I almost wrote the definition for that macro-generating macro in terms of itself, but
   right about then I came down with a killer headache. Can't figure out why. Oh well.

|#


; =====================
; || LANGUAGE DESIGN ||
; =====================

; A cmd is one of:
;  -(make-JMPOBJ symbol number number)		-Jump gobject to x,y
;  -(make-JMPOBJRAND symbol)			-Jump gobject to random coords
;  -(make-STOPOBJ symbol)			        -Stop gobject from movement
;  -(make-ADDOBJ gobject)			        -Add gobject
;  -(make-UDTOBJ symbol)			        -Update gobject
;  -(make-DELOBJ symbol)			        -Delete gobject
;  -(make-WHILE condcmd list[cmd])		        -Do while cmd returns true
;  -(make-IFCOND condcmd list[cmd] list[cmd])	-If cmd returns true, execute first cmdlist, otherwise the second cmdlist
;
; A condcmd is one of
;  -(make-COLLIDE? symbol symbol)
;  -(make-EDGECOLLIDE? symbol)
;  -(make-NOTCOND condcmd)
;
; A graphic is one of
;  -(make-GENCIRCLE number symbol)
;  -(make-GENRECT number number symbol)

; Note: as all structs here are very simple, templates have been omitted.

; A JMPOBJ is (make-JMPOBJ symbol number number)
(define-struct JMPOBJ (obj nx ny))

; A JMPOBJ is (make-JMPOBJRAND symbol)
(define-struct JMPOBJRAND (obj))

; A STOPOBJ is (make-STOPOBJ symbol)
(define-struct STOPOBJ (obj))

; A ADDOBJ is (make-ADDOBJ gobject)
(define-struct ADDOBJ (obj))

; A UDTOBJ is (make-UDTOBJ symbol)
(define-struct UDTOBJ (obj))

; A DELOBJ is (make-DELOBJ symbol)
(define-struct DELOBJ (obj))

; A COLLIDE? is (make-COLLIDE? symbol symbol)
(define-struct COLLIDE? (obj1 obj2))

; A EDGECOLLIDE? is (make-EDGECOLLIDE? symbol)
(define-struct EDGECOLLIDE? (obj))

; A WHILE is (make-WHILE condcmd list[cmd])
(define-struct WHILE (cnd cmds))

; A IFCOND is (make-IFCOND condcmd list[cmd] list[cmd])
(define-struct IFCOND (cnd ctrue cfalse))

; A NOT is (make-NOTCOND condcmd)
(define-struct NOTCOND (cnd))

; A GENCIRCLE is (make-GENCIRCLE number symbol)
(define-struct GENCIRCLE (rad color) (make-inspector))

; A GENRECT is (make-GENRECT number number symbol)
(define-struct GENRECT (w h color) (make-inspector))

; An gobject is (make-gobject symbol graphic number number number number)
(define-struct gobject (name sprite posx posy velx vely) (make-inspector))



; =================
; || INTERPRETER ||
; =================


; =============================
; MEMORY FUNCTIONS


;; list[gobject]
(define mem empty)


;; in-mem?: symbol -> boolean
;; Consumes a symbol and returns true if that a gobject by that name exists
;; inmemory.
(define (in-mem? name)
  (obj-in-list? name mem))


;; obj-in-list? symbol list[obj] -> boolean
;; Consumes a symbol and a list of gobjects and retrusn true
;; if that gobject is present in the given list.
(check-expect (obj-in-list? 'DNE mem) false) ;; Works because mem is defined as empty above.
(check-expect (obj-in-list? 'DE (list (make-gobject 'DE 0 0 0 0 0))) true)
(define (obj-in-list? name lst)
  (not (empty? (filter (lambda (obj)(symbol=? name (gobject-name obj))) lst))))


;; get-gobject: symbol -> gobject
;; Conusmes a symbol and returns the gobject associated with that symbol.
;; No test cases because this relies on a global variable.
(define (get-gobject name)
  (if (in-mem? name)
      (first (filter (lambda (obj)(symbol=? name (gobject-name obj))) mem))
      (error (format "Cannot retrieve gobject \"~a\" - does not exist!~n" (symbol->string name)))))


;; stor-obj: gobject -> void
;; Consumes an gobject and pushes it to memory, overwriting
;; anything else under the same name already there.
(define (stor-obj obj)
  (set! mem (stor-obj-in-list obj mem)))


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
      (if (empty? lst)	;; If mem is empty, make a list out of an incoming gobject
          (list obj)
          (cons obj lst))))

;; del-obj: symbol -> void
;; Consumes an gobject name (symbol) and deletes it entirely
;; from memory.
(define (del-obj name)
  (set! mem
        (filter (lambda (obj)
                  (not (symbol=? name (gobject-name obj))))
                mem)))


;; =============================
;; INTERPRETER FUNCTIONS
;; (for real this time!)

;; Constants - change these to change general animation parameters.
(define WIN_X 800)
(define WIN_Y 600)
(define SKIPTIME 0.016) ; How many seconds to skip between frames


;; run-animation: list[cmd] -> void
;; Runs the program contained within a list of commands.
(define (run-animation cmdlist)
  (for-each (lambda (cmd)
              (begin
                (exec-cmd cmd)
                (render-objects)
                (sleep/yield SKIPTIME)))
            cmdlist))


;; mov-obj: number number symbol -> void
;; Consumes two numbers and an gobject name, and moves
;; the gobject to the coordinates given by the number pair.
(define (move-obj nx ny name)
  (if (in-mem? name)
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
;; return booleans. Most of these cmds aren't separated into
;; helper functions as the are simple enough on their own.
(define (exec-cmd cmd)
  (cond [(JMPOBJ? cmd)
         (move-obj (JMPOBJ-nx cmd) (JMPOBJ-ny cmd) (JMPOBJ-obj cmd))]
        [(JMPOBJRAND? cmd)
         (move-obj (random WIN_X) (random WIN_Y) (JMPOBJRAND-obj cmd))]
        [(STOPOBJ? cmd)
         (stor-obj
          (make-gobject
           (STOPOBJ-obj cmd)				; Name
           (gobject-sprite (get-gobject (STOPOBJ-obj cmd))); Sprite
           (gobject-posx (get-gobject (STOPOBJ-obj cmd)))	; Posx
           (gobject-posy (get-gobject (STOPOBJ-obj cmd)))	; Posy
           0						; Velx
           0))]						; Vely
        [(ADDOBJ? cmd)
         (stor-obj (ADDOBJ-obj cmd))]
        [(UDTOBJ? cmd)
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
             (run-animation (IFCOND-ctrue cmd))
             (run-animation (IFCOND-cfalse cmd)))]
        [else
         (error (format "invalid command: ~a~n" cmd))]))

;; eval-condcmd: cmd -> boolean
;; Consumes a conditional command (COLLIDE?, EDGECOLLIDE?, NOTCOND)
;; and returns the evaluation of that command in boolean form.
;; Note: EDGECOLLIDE defines its edges as 10px inside the actual window border.
(check-expect (eval-condcmd true) true)
(check-expect (eval-condcmd false) false)
(check-expect (eval-condcmd (make-NOTCOND false)) true)
(define (eval-condcmd cmd)
  (cond [(boolean? cmd)
         cmd]
        [(NOTCOND? cmd)
         (not (eval-condcmd (NOTCOND-cnd cmd)))]
        [(EDGECOLLIDE?? cmd) ; Everything below here cannot be tested as they rely on a
         (or                 ; globally-defined memory
          (< (gobject-posx (get-gobject (EDGECOLLIDE?-obj cmd))) 10)
          (> (gobject-posx (get-gobject (EDGECOLLIDE?-obj cmd))) (- WIN_X 10))
          (< (gobject-posy (get-gobject (EDGECOLLIDE?-obj cmd))) 10 )
          (> (gobject-posy (get-gobject (EDGECOLLIDE?-obj cmd))) (- WIN_Y 10)))]
        [(COLLIDE?? cmd) 
         (overlap? (get-gobject (COLLIDE?-obj1 cmd)) (get-gobject (COLLIDE?-obj2 cmd)))]
        [else
         (error (format "invalid conditional command ~a~n" cmd))]))


;; overlap?: gobject gobject -> boolean
;; Consumes an two gobjects and returns true if their graphics overlap.
;; There are a lot of check-expects for this one because it's very complex.
(check-expect (overlap? (make-gobject 'o1 (make-GENRECT 40 40 'red) 604 142 3 0.25)  ; Retangles overlap
                        (make-gobject 'o2 (make-GENRECT 20 400 'blue) 600 50 0 0))
              true)
(check-expect (overlap? (make-gobject 'o1 (make-GENRECT 10 10 'red) 0 0 0 0)  ; Rectangles don't overlap
                        (make-gobject 'o2 (make-GENRECT 10 10 'red) 30 30 0 0))
              false)
(check-expect (overlap? (make-gobject 'o1 (make-GENRECT 10 10 'red) 0 0 0 0)  ; Rectangle corners overlap
                        (make-gobject 'o2 (make-GENRECT 10 10 'red) 5 5 0 0))
              true)
(check-expect (overlap? (make-gobject 'o1 (make-GENCIRCLE 5 'red) 0 0 0 0)    ; Circle corners overlap
                        (make-gobject 'o2 (make-GENCIRCLE 5 'red) 5 0 0 0))
              true)
(check-expect (overlap? (make-gobject 'o1 (make-GENCIRCLE 10 'red) 0 0 0 0)   ; Circles don't overlap
                        (make-gobject 'o2 (make-GENCIRCLE 10 'red) 30 30 0 0))
              false)
(check-expect (overlap? (make-gobject 'rcirc (make-GENCIRCLE 20 'red) 100 100 3 0.25)   ; Rectangle doesn't overlap with circle
                        (make-gobject 'bwall (make-GENRECT 20 400 'blue) 600 50 0 0))
              false)
(check-expect (overlap? (make-gobject 'rcirc (make-GENCIRCLE 20 'red) 604 142 3 0.25)   ; Rectangle overlaps with circle
                        (make-gobject 'bwall (make-GENRECT 20 400 'blue) 600 50 0 0))
              true)

(define (overlap? o1 o2)
  (local [(define (to-rect circ)  ; Cheat at circle collision detection. Circles have corners, right?
            (if (GENCIRCLE? circ)
                (make-GENRECT (* 2 (GENCIRCLE-rad circ))
                              (* 2 (GENCIRCLE-rad circ))
                              (GENCIRCLE-color circ))
                circ))]
    (nor	                                        ; Return true if none of the failure conditions are true
     (> (gobject-posx o1)
        (+ (gobject-posx o2) (GENRECT-w (to-rect (gobject-sprite o2)))))
     (> (gobject-posx o2)
        (+ (gobject-posx o1) (GENRECT-w (to-rect (gobject-sprite o1)))))
     (> (gobject-posy o1)
        (+ (gobject-posy o2) (GENRECT-h (to-rect (gobject-sprite o2)))))
     (> (gobject-posy o2)
        (+ (gobject-posy o1) (GENRECT-h (to-rect (gobject-sprite o1))))))))


;; exec-while: WHILE -> void
;; Executes a WHILE command, recursively.
(define (exec-while cmd)
  (if (eval-condcmd (WHILE-cnd cmd))
      (begin
        (run-animation (WHILE-cmds cmd))
        (exec-while cmd))
      (void)))


;; render-objects: void -> void
;; Renders all objects stored in memory.
(define (render-objects)
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
          (define (gxcord obj)
            (+ (gobject-posx obj) (if (GENRECT? (gobject-sprite obj))
                                      (/ (GENRECT-w (gobject-sprite obj)) 2)
                                      (GENCIRCLE-rad (gobject-sprite obj)))))
          (define (gycord obj)
            (+ (gobject-posy obj) (if (GENRECT? (gobject-sprite obj))
                                      (/ (GENRECT-h (gobject-sprite obj)) 2)
                                      (GENCIRCLE-rad (gobject-sprite obj)))))
          (define (render-objlist lst) ;; Returns a scene.
            (cond [(cons? lst)
                   (place-image (sprite-to-img (gobject-sprite (first lst)))
                                (gxcord (first lst))
                                (gycord (first lst))
                                (render-objlist (rest lst)))]
                  [else (empty-scene WIN_X WIN_Y)]))]
    (update-frame (render-objlist mem))))



;; =====================
;; || LANGUAGE MACROS ||
;; =====================


;; Defines a "keyword" macro, generating macros on the fly for
;; arbitrary keywords. It's realy just a simplification of the
;; define-syntax/syntax-rules construct, but it's mucher cleaner
;; and easier to read.
(define-syntax kw
  (syntax-rules (: ->)
    [(kw id : [orig-form -> new-form] ...)     
     (define-syntax id
        (syntax-rules ()
          [orig-form
           new-form] ...))]))

(kw PROGRAM      : [(PROGRAM cmd ...)   -> (list cmd ...)]) ; This one is just to obscure the underlying "list" call
(kw DELOBJ       : [(DELOBJ id)         -> (make-DELOBJ 'id)])
(kw UDTOBJ       : [(UDTOBJ id)         -> (make-UDTOBJ 'id)])
(kw STOPOBJ      : [(STOPOBJ id)        -> (make-STOPOBJ 'id)])
(kw JMPOBJ       : [(JMPOBJ id nx ny)   -> (make-JMPOBJ 'id nx ny)])
(kw JMPOBJRAND   : [(JMPOBJRAND id)     -> (make-JMPOBJRAND 'id)])
(kw COLLIDE?     : [(COLLIDE? id1 id2)  -> (make-COLLIDE? 'id1 'id2)])
(kw EDGECOLLIDE? : [(EDGECOLLIDE? id)   -> (make-EDGECOLLIDE? 'id)])
(kw NOTCOND      : [(NOTCOND condition) -> (make-NOTCOND condition)])
(kw WHILE        : [(WHILE condition actions ... ) -> (make-WHILE condition (list actions ...))])
(kw IFCOND       : [(IFCOND condition (ctrueact ...) (cfalseact ...)) ->
                    (make-IFCOND condition (list ctrueact ...) (list cfalseact ...))])
(kw ADDOBJ       : [(ADDOBJ name (GENCIRCLE rad col) posx posy velx vely) ->
                    (make-ADDOBJ (make-gobject 'name (make-GENCIRCLE rad 'col) posx posy velx vely))]
                   [(ADDOBJ name (GENRECT w h col) posx posy velx vely) ->
                    (make-ADDOBJ (make-gobject 'name (make-GENRECT w h 'col) posx posy velx vely))])


; ========================
; || EXAMPLE ANIMATIONS ||
; ========================


#| Animation 1: 
	"A red ball moving at a angle towards a blue wall until it hits the wall. 
	At that point, the wall disappears and the ball moves back towards the left 
	edge of the canvas, stopping when it hits the left edge of the canvas."
|#
(define anim-sample1 (PROGRAM
                      (ADDOBJ rcirc (GENCIRCLE 20 red) 100 100 1.5 0.25)
                      (ADDOBJ bwall (GENRECT 20 400 blue) 600 50 0 0)
                      (UDTOBJ rcirc)
                      (WHILE (NOTCOND (COLLIDE? rcirc bwall))
                                  (UDTOBJ rcirc))
                      (IFCOND (NOTCOND (COLLIDE? rcirc bwall))
                              ((UDTOBJ rcirc))
                              ((DELOBJ rcirc)))
                      (DELOBJ bwall)
                      (DELOBJ rcirc)
                      (ADDOBJ rcirc (GENCIRCLE 20 red) 560 175 -1.52 0)
                      (WHILE (NOTCOND (EDGECOLLIDE? rcirc))
                                  (UDTOBJ rcirc))
                      (STOPOBJ rcirc)))


#| Animation 2:
	"A purple circle jumping to random locations around the canvas until it hits 
	the top edge of the canvas."

   Note: due to the frame update time here (0.016 seconds, for about 60FPS) the
   purple circle may reach an edge and stop very quickly, or seemingly instantly
   from the viewer's perspective. If you want to see this slowed down, change the
   constant SKIPTIME to a higher number (e.g. 0.5)
|#
(define anim-sample2 (PROGRAM
                      (ADDOBJ pcirc (GENCIRCLE 20 purple) 400 300 0 0)
                      (WHILE (NOTCOND (EDGECOLLIDE? pcirc)) (JMPOBJRAND pcirc))
                      (STOPOBJ pcirc)))


#| Animation 3:
	"An orange circle dropping straight down until it hits the green rectangle.
	At that point, the red rectangle appears and the circle moves right until
	it hits the red rectangle, after which the orange circle jumps to a random
	location and stops."
|#
(define anim-sample3 (PROGRAM
                      (ADDOBJ ocirc (GENCIRCLE 20 orange) 100 1 0 5)
                      (ADDOBJ grect (GENRECT 750 50 green) 25 540 0 0)
                      (WHILE (NOTCOND (COLLIDE? ocirc grect))
                             (UDTOBJ ocirc))
                      (ADDOBJ rrect (GENRECT 50 512 red) 750 25 0 0)
                      (DELOBJ ocirc)
                      (ADDOBJ ocirc (GENCIRCLE 20 orange) 100 500 5 0)
                      (WHILE (NOTCOND (COLLIDE? ocirc rrect))
                                  (UDTOBJ ocirc))
                      (STOPOBJ ocirc)
                      (JMPOBJRAND ocirc)))


#| Animation 4 (custom)
	"A black ball bounces endlessly up and down."
|#
(define anim-sample4 (PROGRAM
                      (WHILE true
                             (ADDOBJ bcirc (GENCIRCLE 20 black) 300 10 0 5)
                             (WHILE (NOTCOND (EDGECOLLIDE? bcirc))
                                    (UDTOBJ bcirc))
                             (DELOBJ bcirc)
                             (ADDOBJ bcirc (GENCIRCLE 20 black) 300 590 0 -5)
                             (WHILE (NOTCOND (EDGECOLLIDE? bcirc))
                                    (UDTOBJ bcirc))
                             (DELOBJ bcirc))))



;; =========================
;; || ANIMATION EXECUTION ||
;; =========================


(check-expect (create-canvas WIN_X WIN_Y) true) ; This always outputs an annoying "true" to the console,
(test)                                          ; so it's a check-expect now.
(run-animation anim-sample3)

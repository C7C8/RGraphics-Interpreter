#!/usr/bin/racket
#lang racket		;; These lines required to run the program without DrRacket

; =======================
; || Christopher Myers ||
; =======================


; A cmd is one of:
;  -(make-JMPOBJ object number number)		-Jump object to x,y
;  -(make-ADDOBJ object)			-Add object
;  -(make-UDTOBJ object)			-Update object
;  -(make-DELOBJ object)			-Delete object
;  -(make-COLLIDE? object object)		-Object colliding with object?
;  -(make-EDGE? object)				-Object colliding with edge?
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
;  -(make-entity symbol graphic posx posy velx vely)
;
; A graphic is one of
;  -symbol
;  -(make-GENCIRCLE number symbol)
;  -(make-GENRECT number number symbol)
;  -[a user-specified image]


; A JMPOBJ is (make-JMPOBJ object number number)
(define-struct JMPOBJ (obj nx ny))

; A ADDOBJ is (make-ADDOBJ object)
(define-struct ADDOBJ (obj))

; A UDTOBJ is (make-UDTOBJ object)
(define-struct UDTOBJ (obj))

; A DELOBJ is (make-DELOBJ object)
(define-struct DELOBJ (obj))

; A COLLIDE? is (make-COLLIDE? object object)
(define-struct COLLIDE? (obj1 obj2))

; A EDGE? is (make-EDGE? object)
(define-struct EDGE? (obj))

; A WHILE is (make-WHILE cmd list[cmd])
(define-struct WHILE (cnd cmds))

; A IFCOND is (make-IFCOND cmd list[cmd] list[cmd])
(define-struct (IFCOND cnd ctrue cfalse))

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

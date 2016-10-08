;; Is this a macro that writes macros? Why yes, yes it is!
;; More specifically, this macro defines a "keyword" macro,
;; generating macros on the fly for arbitrary keywords.
(define-syntax kw
  (syntax-rules (: ->)
    [(kw id : [orig-form -> new-form] ...)     
     (define-syntax id
        (syntax-rules ()
          [orig-form
           new-form] ...))]))

(kw PROGRAM      : [(PROGRAM (cmd ...)) -> (list (cmd ...))])
(kw DELOBJ       : [(DELOBJ id)  -> (make-DELOBJ 'id)])
(kw UDTOBJ       : [(UDTOBJ id)  -> (make-UDTOBJ 'id)])
(kw STOPOBJ      : [(STOPOBJ id) -> (make-STOPOBJ 'id)])
(kw ADDOBJ       : [(ADDOBJ name (GENCIRCLE rad col) posx posy velx vely) ->
                    (make-ADDOBJ (make-gobject 'name (make-GENCIRCLE rad 'col) posx posy velx vely))]
                   [(ADDOBJ name (GENRECT w h col) posx posy velx vely) ->
                    (make-ADDOBJ (make-gobject 'name (make-GENRECT w h 'col) posx posy velx vely))])
(kw COLLIDE?     : [(COLLIDE? id1 id2)  -> (make-COLLIDE? 'id1 'id2)])
(kw EDGECOLLIDE? : [(EDGECOLLIDE? id)   -> (make-EDGECOLLIDE? 'id)])
(kw NOTCOND      : [(NOTCOND condition) -> (make-NOTCOND condition)])
(kw WHILE        : [(WHILE condition actions ... ) -> (make-WHILE condition (list actions ...))])
(kw IFCOND       : [(IFCOND condition (ctrueact ...) (cfalseact ...)) ->
                    (make-IFCOND condition (list ctrueact ...) (list cfalseact ...))])


(define anim-sample1 (program
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
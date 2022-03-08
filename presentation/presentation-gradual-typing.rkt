#lang slideshow

(require racket/match)
(require pict)
(require slideshow/latex)


;; LANGUAGE OF EQUATIONS
(define exp1
  '(consistent
    (type "int")
    (type-var "rho")))

(define exp2
  '(has-type
    "e"
    (type-var "rho")))

(define exp3
  '(consistent
    (object
     (method "x"
             (signature (type "int") (dyn-type)))
     (method "y"
             (signature (dyn-type) (type "bool"))))
    (object
     (method "y"
             (signature (type "bool") (dyn-type)))
     (method "x"
             (signature (dyn-type) (type "int"))))))

(define exp4
  '(rule
    ; Premisses
    (and
     (implies
      env
      (has-type "e" (type-var "sigma")))
     (subtype
      (type-var "sigma")
      (type-var "tau"))
     )
    ; Consequences
    (implies
      env ;; Gamma
      (has-type "e" (type-var "tau")))))


;; COMPILE TO LATEX / PICT

(add-preamble #<<latex
\usepackage{amsmath, amssymb}
latex
              )

(define (latex-symbol sim)
  (string-append "\\" sim))

(define (latex exp)
  (match exp
    [`(consistent ,x ,y) (string-append (latex x) (latex-symbol "sim") (latex y))]
    [`(restrict ,t ,t1) (format "~a|_{~a}" (latex t) (latex t1))]
    [`(inconsistent ,x ,y) (string-append (latex  x) (latex-symbol "nsim") (latex y))]
    [(list 'object method ...) (format "[~a]" (string-join (map latex method) ",~"))]
    [`(method ,name (signature ,it ,ot)) (latex `(has-type ,name (fun-type ,it ,ot)))]
    [`(has-type ,e ,t) (format "~a : ~a" (latex e) (latex t))]
    [`(fun-type ,it ,ot) (format "~a \\rightarrow ~a" (latex it) (latex ot))]
    [`(type ,t) (format "\\texttt{~a}" t)]
    [`(type-var (prime ,t)) (string-append (latex `(type-var ,t)) "'")]
    [`(type-var (second ,t)) (string-append (latex `(type-var ,t)) "''")]
    [`(type-var ,t) (latex-symbol t)]
    [`(dyn-type) "\\textrm{?}"]
    [`(subtype ,t1 ,t2) (string-join (list (latex t1) "<:" (latex t2)) " ")]
    [`(implies env ,exp) (string-join (list (latex-symbol "Gamma")
                                            "\\vdash"
                                            (latex exp)) " ")]
    [s s]))

;; Size of a space in LaTeX
(define ltx-space
  128/9)

(define (ltx-concat . args)
  (apply
   (curry hbl-append ltx-space)
   args))
              
  

(define (dyn-type)
  ($ (latex '(dyn-type))))

(define (type t)
  ($ (latex `(type ,t))))

(define (subtype t1 t2)
  (hbl-append ltx-space t1 ($ "<:") t2))

(define (type-var v)
  ($ (latex `(type-var ,v))))

(define (consistent x y)
  (ltx-concat x ($ (latex-symbol "sim")) y))

(define (inconsistent x y)
  (ltx-concat x ($ (latex-symbol "nsim")) y))

(define (has-type e t)
  (ltx-concat e ($ ":") t))

(define (fun-type it ot)
  (ltx-concat it ($ "\\rightarrow") ot))

(define (method name it ot)
  (has-type ($ name)
            (fun-type it ot)))


(define (list-join c list)
  (if (= (length list) 1)
      list
      (cons
       (car list)
       (cons
        c
        (list-join c (cdr list))))))          
  
(define (object . methods)
  (apply ltx-concat
         (append (list ($ "["))
                 (list-join ($ ",") methods)
                 (list ($ "]")))))


(define (fast-pict exp)
  (match exp
    [`(rule ,premisse ,consequence)
     (let [(p (fast-pict premisse))]
       (vc-append
        p
        (hline (pict-width p) 10)
        (fast-pict consequence)))]
    [`(and ,p1 ,p2) (hb-append 50.0 (fast-pict p1) (fast-pict p2))]
    [`(implies env ,cons) ($ (latex `(implies env ,cons)))]
    [`(inconsistent ,x ,y) ($ (latex `(inconsistent ,x ,y)))]
    [`(consistent ,x ,y) ($ (latex `(consistent ,x ,y)))]
    [`(type-var ,t) ($ (latex `(type-var ,t)))]
    [`(subtype ,t1 ,t2) ($ (latex `(subtype ,t1 ,t2)))]
    [`(type ,(? string? t)) ($ (latex `(type ,t)))]
    [`(dyn-type) (dyn-type) ]
    ))

;; PRESENTATION

(current-main-font "Lato Medium")

(slide
 (bt "Gradual typing for Objects")
 (t "Noric Couderc"))

(slide
 #:title "The Problem"
 'next
 (item (bt "Static typing"))
 (subitem "full coverage")
 (subitem "efficient execution")
 (subitem "machine-checked documentation")
 'next
 (item (bt "Dynamic typing"))
 (subitem "more flexibility")
 )

(slide
 (bt "Why not have both?"))

(slide
 (bt "What you want")
 'next
 (item "Omit type annotations")
 (subitem "Run program whenever you want")
 'next
 (item "Add type annotation when you need")
 (subitem "Fully annotated program = static type checking"))

(slide
 #:title "Alternatives"
 (item "Type inference")
 'next
 (subitem "Must typecheck before run")
 'next
 (item "Type annotations")
 'next
 (subitem "No proof of typing prevents all type errors")
 'next
 (item "Soft typing / static analysis")
 'next
 (subitem "Only warnings")
 )

(slide
 #:title "The idea"
 (item "add a type \"dynamic\":" (fast-pict '(dyn-type)))
 (item "add relations between types and" (fast-pict '(dyn-type))))

(slide
 #:title "Restriction"
 (item "Notation:" ($ "\\sigma|_{\\tau}"))
 'next
 (item "Slogan: More question marks")
 'next
 (item ($ (string-join (list
                        (latex '(restrict (type "int")
                                          (dyn-type)))
                        (latex '(dyn-type)))
                       "=")))
 'next
 (item ($ (format "~a = ~a"
                  (latex '(restrict (type "int")
                                    (type "bool")))
                  (latex '(type "int")))))
)

(slide
 #:title "Restriction II"
 (t "Object with two methods x and y")
 'next
 ($ (latex '(restrict (object
                       (method "x"
                               (signature (type "int") (type "int")))
                       (method "y"
                               (signature (type "int") (type "int"))))
                      (object
                       (method "x"
                               (signature (dyn-type) (dyn-type)))
                       (method "y"
                               (signature (type "int") (type "int")))))))
 'next
 (t "=")
 'next
 ($ (latex '(object
             (method "x"
                     (signature (dyn-type) (dyn-type)))
             (method "y"
                     (signature (type "int") (type "int")))))
    ))

(slide
 #:title "Type consistency"
 ($ (latex '(consistent (type-var "sigma")
                        (type-var "tau"))))
 'next
 (t "=")
 ($ (format "~a = ~a "
            (latex '(restrict (type-var "sigma")
                              (type-var "tau")))
            (latex '(restrict (type-var "tau")
                              (type-var "sigma")))))
 )


(slide
 (item (fast-pict '(consistent (type "int") (type "int"))))
 (item (fast-pict '(consistent (type "int") (dyn-type))))
 (item ($ (latex '(consistent
                   (fun-type (type "int") (dyn-type))
                   (fun-type (dyn-type) (type "int")))))))


(slide
 ($ (latex '(consistent
             (object
              (method "x"
                      (signature (type "int") (dyn-type)))
              (method "y"
                      (signature (dyn-type) (type "bool"))))
             (object
              (method "y"
                      (signature (type "bool") (dyn-type)))
              (method "x"
                      (signature (dyn-type) (type "int"))))))))

(slide
 (inconsistent
  (object
   (method "x"
           (type "int")
           (type "int"))
   (colorize
    (method "y"
            (dyn-type) (dyn-type))
    "red"))
  (object
   (method "x"
           (type "int") (type "int")))))


(define inconsistent-example
  (let* [(note (t "different types"))
         (spacing 200)
         (x (method "x" (type "int") (type "int")))
         (x1 (method "x" (type "bool") (type "int")))
         (y (method "y" (dyn-type) (type "bool")))]
    ((compose
      (lambda (p)
        (linewidth 3 p))
      (lambda (p)
        (pin-arrow-line
         10
         ; Under
         p
         note
         ct-find
         x
         cb-find))
      (lambda (p)
        (pin-arrow-line
         10
         p
         note
         ct-find
         x1
         cb-find)))
     ; Base drawing
     (vc-append
      200
      (vc-append
       100
       (object x y)
       ($ (latex-symbol "nsim"))
       (object x1 y))
      note))))


(slide
 inconsistent-example)





(slide
 #:title "Properties"
 (let [(sim ($ (latex-symbol "sim")))]
   (vl-append
     (item sim "reflexive")
     (item sim "symmetric")
     (item sim "not transitive"
           (subitem (fast-pict '(consistent (type "int") (dyn-type)))
                    "and"
                    (fast-pict '(consistent (type "bool") (dyn-type)))
                    "but"
                    (fast-pict '(inconsistent (type "bool") (type "int")))))
     (item ($ (latex '(consistent (type-var "tau")
                                  (restrict (type-var "tau")
                                            (type-var "sigma"))))))
 )))


(define sub-type-table1
 (let* [(tau    (inset (fast-pict '(type-var "tau")) 10))
        (sigma  (inset (fast-pict '(type-var "sigma")) 10))
        (sigma1 (inset (fast-pict '(type-var (prime "sigma"))) 10))
        (sigma2 (inset (fast-pict '(type-var (second "sigma"))) 10))
        (combined
         (table 2
                (list
                 sigma2
                 tau
                 sigma
                 sigma1
                 )
                cc-superimpose
                cbl-superimpose
                200
                200))]
   ((compose
     ; Set linewidth
     (lambda (p)
       (linewidth 3 p))
     ; First arrow
     (lambda (p)
       (pin-arrow-line 10
                       p
                       sigma
                       rt-find
                       tau
                       lb-find
                       #:label (scale ($ "\\lesssim") 0.8)
                       #:x-adjust-label -25))
     ; Second arrow
      (lambda (p)
         (pin-arrow-line 10
                         p
                         sigma
                         rbl-find
                         sigma1
                         lbl-find
                         #:label (scale ($ "<:") 0.8)
                         #:y-adjust-label -10
                         #:color "darkred"
                         ))
     ; Third arrow
     (lambda (p)
        (pin-arrows-line 10
                         p
                         sigma
                         ct-find
                         sigma2
                         cb-find
                         #:label (scale ($ (latex-symbol "sim")) 0.8)
                         #:x-adjust-label -25
                         #:color "steelblue"))
     (lambda (p)
        (pin-arrow-line 10
                       p
                       sigma2
                       rbl-find
                       tau
                       lbl-find
                       #:label (scale ($ "<:") 0.8)
                       #:y-adjust-label -10
                       #:color "darkred"))
     (lambda (p)
        (pin-arrows-line 10
                         p
                         sigma1
                         ct-find
                         tau
                         cb-find
                         #:label (scale ($ (latex-symbol "sim")) 0.8)
                         #:x-adjust-label 25
                         #:color "steelblue")))
    combined
   )))

(slide
 (bt "Consistent subtyping")
  sub-type-table1)

;; Section 5: Ob<:?
(define gradual-type-system-pict
  ($ "\\textbf{Ob}_{<:}^{?}"))

(define intermediate-type-system-pict
  ($ "\\textbf{Ob}_{<:}^{\\langle\\cdot\\rangle}"))

;; Section 6: Ob<:<.>
(define sub-type-table3
  (let* [(tau    (inset (fast-pict '(type-var "tau")) 10))
         (sigma  (inset (fast-pict '(type-var "sigma")) 10))
         (rho (inset (fast-pict '(type-var "rho")) 10))
         (combined
          (table 2
                 (list
                  (blank)
                  tau
                  sigma
                  rho
                  )
                 cc-superimpose
                 cbl-superimpose
                 100
                 100))]
    ((compose
      ; Set linewidth
      (lambda (p)
        (linewidth 3 p))
      ; First arrow
      (lambda (p)
        (pin-arrow-line 10
                        p
                        sigma
                        rt-find
                        tau
                        lb-find
                        #:label (scale ($ "\\lesssim") 0.8)
                        #:x-adjust-label -25))
      ; Second arrow
      (lambda (p)
        (pin-arrows-line 10
                         p
                         sigma
                         rbl-find
                         rho
                         lbl-find
                         #:label (scale ($ (latex-symbol "sim")) 0.8)
                         #:y-adjust-label -10
                        ))
      ; The line
      (lambda (p)
        (pin-arrow-line 10
         p
         rho
         ct-find
         tau
         cb-find
         #:label (scale ($ "<:") 0.8)
         #:x-adjust-label 25
         )))
     combined
     )))

(slide
 (hbl-append 20 (bt "Two ways to") ($ "\\lesssim"))
 sub-type-table3
 'next
 (t "Insert type casts"))

;; Section 7, type system is safe
(slide
 (ltx-concat gradual-type-system-pict (t "=")
             intermediate-type-system-pict (t "+") (t "casts"))
 )

(tidy-latex-cache)
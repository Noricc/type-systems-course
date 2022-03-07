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

(define (pict exp)
  (match exp
    [`(rule ,premisse ,consequence)
     (let [(p (pict premisse))]
       (vc-append
        p
        (hline (pict-width p) 10)
        (pict consequence)))]
    [`(and ,p1 ,p2) (hb-append 50.0 (pict p1) (pict p2))]
    [`(implies env ,cons) ($ (latex `(implies env ,cons)))]
    [`(consistent ,x ,y) (hb-append (pict x) ($ (latex-symbol "sim")) (pict y))]
    [`(type-var ,t) ($ (latex `(type-var ,t)))]
    [`(subtype ,t1 ,t2) ($ (latex `(subtype ,t1 ,t2)))]
    [`(type ,(? string? t)) ($ (latex `(type ,t)))]))

  

;; PRESENTATION
(define sub-type-table1
 (let* [(tau    (inset (pict '(type-var "tau")) 10))
        (sigma  (inset (pict '(type-var "sigma")) 10))
        (sigma1 (inset (pict '(type-var (prime "sigma"))) 10))
        (combined
         (table 2
                (list
                 tau
                 (blank)
                 sigma
                 sigma1
                 )
                cc-superimpose
                cb-superimpose
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
                       ct-find
                       tau
                       cb-find
                       #:label (scale ($ "\\lesssim") 0.8)))
     ; Second arrow
     (lambda (p)
       (pin-arrow-line 10
                       p
                       sigma
                       rbl-find
                       sigma1
                       lbl-find
                       #:label (scale ($ "<:") 0.8)
                       #:y-adjust-label 25
                       ))
     ; The line
     (lambda (p)
       (pin-line
        p
        sigma1
        lt-find
        tau
        rb-find
        #:label (scale ($ (latex-symbol "sim")) 0.8)
        #:x-adjust-label 10
        #:y-adjust-label -10
     )))
    combined
   )))

(define sub-type-table2
  (let* [(tau    (inset (pict '(type-var "tau")) 10))
         (sigma  (inset (pict '(type-var "sigma")) 10))
         (sigma2 (inset (pict '(type-var (second "sigma"))) 10))
         (combined
          (table 2
                 (list
                  tau
                  (blank)
                  sigma
                  sigma2
                  )
                 cc-superimpose
                 cb-superimpose
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
                        ct-find
                        tau
                        cb-find
                        #:label (scale ($ "\\lesssim") 0.8)))
      ; Second arrow
      (lambda (p)
        (pin-arrow-line 10
                        p
                        sigma
                        rbl-find
                        sigma2
                        lbl-find
                        #:label (scale ($ (latex-symbol "sim")) 0.8)
                        #:y-adjust-label 25
                        ))
      ; The line
      (lambda (p)
        (pin-line
         p
         sigma2
         lt-find
         tau
         rb-find
         #:label (scale ($ "<:") 0.8)
         #:x-adjust-label 10
         #:y-adjust-label -10
         )))
     combined
     )))

(slide
 (hc-append
  sub-type-table1
  (vline 100 200)
  sub-type-table2))


(tidy-latex-cache)
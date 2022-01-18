# Iso

![Iso](Iso.png)

```scheme
#lang racket
(require plot)
 
(define xg0 1)
 
(define (g x)
  (/ 3 (- x xg0)))
 
(define Xs
  '(0.0
    2.302785323219288
    3.501885860538783
    4.387531553378729
    5.116359864244201
    5.748205229427828))
 
(define aspect-ratio 2.)
(define xmax 7)
(define ymax (/ xmax aspect-ratio))
 
(parameterize ([plot-x-ticks no-ticks]
               [plot-y-ticks no-ticks])
  (plot
   #:aspect-ratio aspect-ratio
   #:y-min 0 #:y-max ymax
   #:x-max xmax
   #:width 700
   #:x-label #f
   #:legend-anchor 'top-right
   (list (function g #:color 1 #:label "g(x)")
         (x-ticks (list (tick 0 #t "X₀=0")))
         (lines-interval (list (list 0 0) (list 0 ymax))
                         (list (list xg0 0) (list xg0 ymax))
                         #:line1-style 'transparent
                         #:line2-color 1 #:line2-style 'long-dash
                         #:color 1 #:style 'fdiagonal-hatch #:alpha 1)
         (for/list ([X Xs]
                    [X2 (rest Xs)]
                    [t (in-naturals 1)]
                    [t-1_idx '(₀ ₁ ₂ ₃ ₄ ₅ ₆)]
                    [t_idx '(₁ ₂ ₃ ₄ ₅ ₆ ₇)])
           (define gX2 (g X2))
           (define X2+δ (+ X2 (/ (+ t 1.)) #;(* .4 (- X2 X))))
           (list
            (function (λ (x) (- x X))
                      #:color "black"
                      X X2+δ)
            (vrule X2 0 gX2 #:style 'long-dash #:color "black")
            (point-label (list X2 gX2) (format "iso~a" t_idx) #:anchor 'left)
            (x-ticks (list (tick X2 #t (format "X~a" t_idx))))
            (point-label (list X2+δ (- X2+δ X)) (format "y = x - X~a" t-1_idx)
                         #:anchor 'left #:point-size 0)))
         )))
         
```

http://pasterack.org/pastes/28373

***

#lang at-exp racket
(require infix
         plot/pict
         threading)

(define (f a x) @${sin[(10+a)*x]})
(define (g a x) @${x / (a+5)})

(parameterize ([line-width 2]
               [plot-x-label "Degrees"]
               [plot-y-label "Magnitude"]
               [plot-y-ticks (linear-ticks #:number 5)]
               [plot-width 800]
               [plot-aspect-ratio 2]
               [plot-legend-anchor 'outside-right-top])

  (plot (for/list ([a (inclusive-range 1 6)])
          (function (λ~>> degrees->radians
                          (f a)
                          (g a))
                    0 50
                    #:color a
                    #:label (~a "1/" (+ a 5) " sin " (+ 10 a) "x")))))
# Plot-cookbook

Sarting simple; links to plots on Pasterack or gists

## Table of contents

* [Draw a graph of cos and deriv^3(cos)](#draw-a-graph-of-cos-and-deriv3cos)
* [Violin plot](#violin-plot)
* [Sines](#sines)

***

## Draw a graph of cos and deriv^3(cos)

![cos and deriv^3(cos) plot](scribblings/images/cosandderiv.png)

```scheme
#lang racket   ; draw a graph of cos
(require plot/pict) ; and deriv^3(cos)
(define ((deriv f) x)
  (/ (- (f x) (f (- x 0.001))) 0.001))
(define (thrice f) (lambda (x) (f (f (f x)))))
(plot (list (function ((thrice deriv) sin) -5 5)
            (function cos -5 5 #:color 'blue)))
            
```

http://pasterack.org/pastes/97561


## Violin plot

![Violin plot](scribblings/images/violin.png)

```scheme
#lang racket
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require
 
(require math/statistics
         plot/pict
         plot/utils)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; violin
 
(define (violin vals
                #:bandwidth [bandwidth (silverman (second vals))]
                #:x-min [x-min #f]
                #:x-max [x-max #f]
                #:y-min [y-min #f]
                #:y-max [y-max #f]
                #:color [color (interval-color)]
                #:style [style (interval-style)]
                #:line1-color [line1-color (interval-line1-color)]
                #:line1-width [line1-width (interval-line1-width)]
                #:line1-style [line1-style (interval-line1-style)]
                #:line2-color [line2-color (interval-line2-color)]
                #:line2-width [line2-width (interval-line2-width)]
                #:line2-style [line2-style (interval-line2-style)]
                #:alpha [alpha (interval-alpha)]
                #:label [label #f])
  (define y-shift (first vals))
  (define-values (f low high)
    (kde (second vals) bandwidth))
  (define x-axis (const 0))
  (define x-min* (or x-min low))
  (define x-max* (or x-max high))
  (define settings
    `([#:y-min . ,y-min]
      [#:y-max . ,y-max]
      [#:color . ,color]
      [#:style . ,style]
      [#:line1-color . ,line1-color]
      [#:line1-width . ,line1-width]
      [#:line1-style . ,line1-style]
      [#:line2-color . ,line2-color]
      [#:line2-width . ,line2-width]
      [#:line2-style . ,line2-style]
      [#:alpha . ,alpha]
      [#:label . ,label]))
  (list (keyword-apply/dict function-interval settings
                            (shift-up (invert f) y-shift)
                            (shift-up f y-shift)
                            x-min* x-max* null)))
 
(define (shift-up f shift)
  (λ (x)
    (+ (f x) shift)))
 
(define ((invert f) x)
  (- (f x)))
 
(define (silverman vals)
  (define iqr (interquartile-range vals))
  (define n (length vals))
  (* 0.9
     (min (stddev vals) (/ iqr 1.34))
     (expt n -0.2)))
 
(define (interquartile-range vals)
  (- (quantile 3/4 < vals)
     (quantile 1/4 < vals)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example
;;
;; `(violin (list y-center x-data))`
;;
 
(parameterize ([plot-y-ticks no-ticks]
               [plot-y-label #f]
               [plot-x-far-ticks no-ticks]
               [plot-x-label "Time (sec)"])
  (plot (list (violin `[0.00 (0 1 1 2 3 4 4 4 5 6 7 9 10 10 10 11 13)])
              (violin `[0.30 (15 16 17 18 19 20 20 21 23 30)]))))
```

http://pasterack.org/pastes/66996


## Sines

![Sines plot](scribblings/images/sines.png)

```scheme
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
               [plot-legend-anchor 'outside-right-top]
               [plot-pen-color-map 'tab10])
  (plot #:x-min 0
        #:x-max 50
        (for/list ([a (inclusive-range 1 6)])
          (function (λ~>> degrees->radians
                          (f a)
                          (g a))
                    #:color (sub1 a)
                    #:label (~a "1/" (+ a 5) " sin " (+ 10 a) "x")))))
                    
```

https://gist.github.com/hunkyjimpjorps/9e512686230c6c1472f4a0d73b81d9dc

## logo plot


![Sines plot](scribblings/images/logo-plot.png)

```scheme
#lang racket

(require plot)

(let ([blue '(0 0 164)] [red '(164 0 0)])
  (parameterize ([plot-decorations? #f])
    (plot3d
     (list
      (surface3d (λ(x y)(/ x 10)) 0 5 0 1 #:color red #:line-color red)
      (surface3d (λ(x y)(- 1 (/ (+ 1 (exp (- 5 x)))))) 0 10 0 1 #:color blue #:line-color blue))
     #:angle 335 #:altitude 5)))
     
```

https://github.com/standard-fish/summer-competititon-2019/blob/c5af58e2b1a55733e5e66ca550ebb73420737c4c/entries/Metaxal/racket-logo-plot.rkt



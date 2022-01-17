# Plot-cookbook
<img src="scribblings/images/logo-plot.png" width="200px;" alt="" align="right"/>
Welcome to the Racket Plot Cookbook. This is a growing collection of recipes for the tastiest plots.

If you would like to contribute please create an issue or pull request with your contribution. Please include image, code, and short description.

Contributions are accepted on the condition they are licenced under the same terms as Racket: MIT or Apache 2.

## Table of contents

* [Draw a graph of cos and deriv^3(cos)](#draw-a-graph-of-cos-and-deriv3cos)
* [Violin plot](#violin-plot)
* [Sines](#sines)
* [Logo plot](#logo-plot)
* [Iso](#Iso)


<table>
  <tr>
    <td align="center"><a href="#draw-a-graph-of-cos-and-deriv3cos"><img src="scribblings/images/cosandderiv.png" width="200px;" alt=""/><br /><sub><b>Draw a graph of cos and deriv^3(cos)</b></sub></a><br /></td>
    <td align="center"><a href="#violin-plot"><img src="scribblings/images/violin.png" width="200px;" alt=""/><br /><sub><b>Violin plot</b></sub></a><br /></td>
        <td align="center"><a href="#sines"><img src="scribblings/images/sines.png" width="200px;" alt=""/><br /><sub><b>Sines</b></sub></a><br /></td>
        <td align="center"><a href="#Iso"><img src="scribblings/images/Iso.png" width="200px;" alt=""/><br /><sub><b>Iso</b></sub></a><br /></td>
  </tr>
  <tr>
      <td align="center"><a href="#logo-plot"><img src="scribblings/images/logo-plot.png" width="200px;" alt=""/><br /><sub><b>Logo plot</b></sub></a><br /></td>

  <tr>
</table>

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


***

## Iso

![Iso](scribblings/images/Iso.png)

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

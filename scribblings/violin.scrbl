#lang scribble/manual

@(require "common.rkt" plot/pict math/statistics)

@title[#:tag "Violin"]{Violin}

Violin plot.

@;@racketinput[]
@interaction[#:eval plot-eval
                    (eval:alts (require math/statistics
                                        plot/pict
                                        plot/utils) (void))


 
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
              (violin `[0.30 (15 16 17 18 19 20 20 21 23 30)]))))]

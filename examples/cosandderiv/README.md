# cosandderiv

## Draw a graph of cos and deriv^3(cos)

![cos and deriv^3(cos) plot](cosandderiv.png)

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


# Logo plot

![Logo plot](logo-plot.png)

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


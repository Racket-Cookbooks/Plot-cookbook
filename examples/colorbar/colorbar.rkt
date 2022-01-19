#lang racket
(require plot plot/utils racket/draw colormaps/tol pict flomat)

(define (mismatch q ecc q0 ecc0)
  (+ (sqr (- q q0)) (sqr (- ecc ecc0))))
(define qs (for/list ([id (build-list 1000 values)]) (random)))
(define eccs (for/list ([id (build-list 1000 values)]) (random)))
(define mismatches (for/list ([ecc eccs]
                              [q qs])
                     (mismatch q ecc 0.5 0.5)))

(define (color-map->list-of-colors cm)
  (parameterize ([plot-pen-color-map cm])
    (for/list ([c (in-range (color-map-size cm))])
      (match-define (list r g b) (->pen-color c))
      (make-object color% r g b))))

(define (get-levels min-val max-val color-count)
  (let* ([intervals-count (sub1 color-count)]
         [step (/ (- max-val min-val) intervals-count)])
    (inclusive-range min-val (+ step max-val) step)))

(define (get-color-index val min-val max-val color-count)
  (let* ([intervals (get-levels min-val max-val color-count)])
    (index-of intervals val >=)))

(define color-list (color-map->list-of-colors 'tol-sd))
(define (get-color val min-val max-val color-list)
  (let* ((color-count (length color-list))
         (color-index (get-color-index val min-val max-val (sub1 color-count))))
    (list-ref color-list color-index)))

(define min-mm (apply min mismatches))
(define max-mm (apply max mismatches))

(define scatter (plot-pict 
                 (for/list ([ecc eccs]
                            [q qs]
                            [mm mismatches])
                   (points (map vector (list ecc) (list q)) #:color (get-color mm min-mm max-mm color-list) #:sym 'fullcircle2))))

(define (get-color-picts list-of-colors)
  (for/list ([c list-of-colors])
    (filled-rectangle 10 (/ (- (plot-height) 33) (length list-of-colors)) #:draw-border? #f #:color c)))

(define (get-label-picts min-val max-val color-count)
  (for/list ([l (in-list (flatten (flomat->lists (linspace min-val max-val color-count))))]
             [idx (in-list (range 0 color-count))])
    (cc-superimpose
     (ghost (filled-rectangle 10 (/ (- (plot-height) 33) color-count))) (text (~a l #:width 4)))))

(define label-picts (apply vl-append (get-label-picts min-mm max-mm (add1 (length color-list)))))
(define color-picts (apply vl-append (get-color-picts color-list)))
(define pad-below (ghost (rectangle 20 33)))
(define colorbar (vl-append (hc-append 5 color-picts label-picts) pad-below))

(define scatter+colorbar (hc-append 10 scatter colorbar))

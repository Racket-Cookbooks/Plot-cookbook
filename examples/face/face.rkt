#lang racket/base

(require plot
         racket/match
         racket/file
         math)

(module+ test (require rackunit))
(define (^ b [e 2])(expt b e))

;--------------------------------------------
;ribbon coordinates
;--------------------------------------------
(define R 100)
(define P  90)
(define π (angle -1))
(define 2π (* 2 π))

(define (xyz->ribbon x y z)
  (define ϕ0 (atan y x))
  (define n0 (/ (- z (* P (/ ϕ0 2π))) P))
  (define n (round n0))
  (define ϕ (+ ϕ0 (* n 2π)))
  (define t (- z (* P (/ ϕ 2π))))
  (define d (- (sqrt (+ (* x x) (* y y))) R))
;(println (list ϕ0 n0 n ϕ t d))
  (list ϕ t d))
(module+ test
  (check-within (xyz->ribbon 100 0 0) '(0 0 0) 1e-15)
  (check-within (xyz->ribbon  90 0 0) '(0 0 -10) 1e-15)
  (check-within (xyz->ribbon 110 0 0) '(0 0 10) 1e-15)
  
  (check-within (xyz->ribbon 100 0  4) '(0 4 0) 1e-15)
  (check-within (xyz->ribbon 100 0 -4) '(0 -4 0) 1e-15)
  (check-within (xyz->ribbon 100 0 (/ P  3)) (list 0 (/ P  3) 0) 1e-15)
  (check-within (xyz->ribbon 100 0 (/ P -3)) (list 0 (/ P -3) 0) 1e-15)

  (check-within (xyz->ribbon -100 0 (/ P 2)) (list π 0 0) 1e-15)
  (check-within (xyz->ribbon -100 0 (+ (/ P 2)(/ P  3))) (list π (/ P  3) 0) 1e-14)
  (check-within (xyz->ribbon -100 0 (+ (/ P 2)(/ P -3))) (list π (/ P -3) 0) 1e-14)

  (check-within (xyz->ribbon 100   0 P) (list 2π 0 0) 1e-15)
  (check-within (xyz->ribbon   0 100 (/ P 4)) (list (/ π 2) 0 0) 1e-15)
  (check-within (xyz->ribbon   0  90 (/ P 4)) (list (/ π 2) 0 -10) 1e-15)
  (check-within (xyz->ribbon 100 100 (* P (+ 2 1/8))) (list (+ (* (+ 2 1/8) 2π)) 0 (* 100 (- (sqrt 2) 1))) 1e-13)
  )

(define (ribbon->xyz ϕ t d)
  (list (* (+ R d) (cos ϕ))
        (* (+ R d) (sin ϕ))
        (+ (* P (/ ϕ 2π)) t)))
(module+ test
  (let ([x (* (random) 100)]
        [y (* (random) 100)]
        [z (* (random) 100)])
    (check-within (apply ribbon->xyz (xyz->ribbon x y z)) (list x y z) 1e-10)))

;--------------------------------------------
;egg-face (in cilinder coordinates)
;--------------------------------------------
(define (skull θ z)
  (define Z0 10)(define Z1 290)
  (define C0 90)(define C1 -10)
  (define C (+ (* (/ (- C0 C1)(expt (- Z1 Z0) 2))(expt (- Z1 z) 2)) C1))
  (define A/B0 .2)
  (define A/B (+ 1 (* A/B0 (/ (- Z0 z)(- Z0 Z1)))))
  (define R0 130)
  (define R (* R0 (sqrt (/ (* 4 (- z Z0)(- Z1 z)) (^ (- Z1 Z0))))))

  (define a (+ (^ (cos θ))(^ (* A/B (sin θ)))))
  (define b (* -2 C (cos θ)))
  (define c (- (^ C)(^ (* R A/B))))
  (define D (sqrt (+ (^ b) (* -4 a c))))

  (and (<= Z0 z Z1)
       (real? D)
       (max (/ (- (- b) D) 2 a)(/ (+ (- b) D) 2 a))))
(define (neck θ z)
  (define Z0  0)(define Z1 90)
  (define R0 80)(define R1 70)
  (and (<= Z0 z Z1)
       (- R0 (* (- R0 R1)(* 4 (/ (- z Z0) (- Z1 Z0)) (- 1 (/ (- z Z0) (- Z1 Z0))))))))
(define (nose θ z)
  (define Z0  120)(define Z1 200)(define Z2 480)
  (define θ0 .15)
  (define R0 140)(define R1 180)
  (and (<= Z0 z Z1)
       (<= (- θ0) θ (+ θ0))
       (* (- 1 (/ (-  z Z0)
                  (- Z2 Z0)))
          (+ R0 (* (- R1 R0)
                   (let ([θ* (/ (+ θ θ0) 2 θ0)])(* 4 θ* (- 1 θ*))))))))
(define (lips θ z)
  (define Z0  70)(define Z1 110)
  (define θ0 .4)
  (define R0 150)(define R1 170)
  (define Z/2 (/ (+ Z0 Z1) 2))
  (and (<= Z0 z Z1)
       (<= (- θ0) θ (+ θ0))
       (+ R0 (* (- R1 R0)
                (if (< z Z/2)
                    (let ([z* (/ (- z Z0)(- Z/2 Z0))])( * -2.25 z* (- z* 4/3)))
                    (let ([z* (/ (- z Z/2)(- Z1 Z/2))])(* -2.25 (+ z* 1/3) (- z* 1))))
                (let ([θ* (/ (+ θ θ0) 2 θ0)])(* 4 θ* (- 1 θ*)))))))
(define (eyes θ z)
  (define Z0 170)(define Z1 190)
  (define θ0 .15)(define θ1 .35)
  (define R0 140)(define R1 150)
 (and (<= Z0 z Z1)
      (<= θ0 (abs θ) θ1)
      (+ R0 (* (- R1 R0)
               (let ([z* (/ (- z Z0)(- Z1 Z0))])(* 4 z* (- 1 z*)))
               (let ([θ* (/ (- (abs θ) θ0)(- θ1 θ0))])(* 4 θ* (- 1 θ*)))))))
(define (eyebrow θ z)
  (define Z0 195)(define Z1 205)
  (define θ0 .55)
  (define R0 130)(define R1 145)
  (and (<= Z0 z Z1)
       (<= (- θ0) θ (+ θ0))
       (+ R0 (* (- R1 R0)
                (let ([z* (/ (- z Z0)(- Z1 Z0))])(* 4 z* (- 1 z*)))
                (let ([θ* (/ (+ θ θ0) 2 θ0)])(* 4 θ* (- 1 θ*)))))))
(define (ears1 θ z)
  (define Z0 110)(define Z1 200)
  (define θ0 (* .35 (sqrt (/ (* 4 (- z Z0)(- Z1 z)) (^ (- Z1 Z0))))))
  (define R0 140)
  (and (<= Z0 z Z1)
       (<= (- (/ π 2) θ0) (abs θ) (+ (/ π 2) θ0))
       R0))
(define (ears2 θ z)
  (define Z0 190)(define Z1 295)
  (define θ0 .1)
  (and (<= Z0 z Z1)
       (<= (- (/ π 2) θ0) (abs θ) (+ (/ π 2) θ0))
       (+ (or (skull θ z) 0) 10)))
;combine
(define (face θ z)
  (apply
   max
   (filter
    values
    (list
     (skull θ z) (neck θ z) (nose θ z) (lips θ z)
     (eyes θ z)  (eyebrow θ z)
     (ears1 θ z) (ears2 θ z)
     0))))

;--------------------------------------------
;some plot functions to show the face
;--------------------------------------------
(module+ main
  (define (ribonFct f
                    #:ϕ-min [ϕ-min -inf.0]
                    #:ϕ-max [ϕ-max +inf.0]
                    #:t-min [t-min -inf.0]
                    #:t-max [t-max +inf.0])
    (λ (x y z)
      (match-define (list ϕ t d0) (xyz->ribbon x y z))
      (if (and (<= ϕ-min ϕ ϕ-max)
               (<= t-min t t-max))
          (- d0 (f ϕ t))
          +inf.0)))
  (define (memo-parapoint f u0 u1 v0 v1 file #:nr [nr 10000])
    (define memo (if (file-exists? file) (file->list file read) '()))
    (define len (length memo))
    (append
     memo
     (for*/list ([i (in-range (max 0 (- nr len)))]
                 [u (in-value (+ u0 (* (- u1 u0) (random))))]
                 [v (in-value (+ v0 (* (- v1 v0) (random))))]
                 [xyz (in-value (with-handlers ([exn:fail? (λ (e) #f)])(f u v)))]
                 #:when xyz)
       (call-with-output-file file (λ (out) (writeln xyz out)) #:exists 'append)
       xyz)))
  (define (parapoint f u0 u1 v0 v1 #:nr [nr 10000] #:memo-it [file #f])
    (if file
        (memo-parapoint f u0 u1 v0 v1 file #:nr nr)
        (for*/list ([i (in-range nr)]
                [u (in-value (+ u0 (* (- u1 u0) (random))))]
                [v (in-value (+ v0 (* (- v1 v0) (random))))]
                [xyz (in-value (with-handlers ([exn:fail? (λ (e) #f)])(f u v)))]
                #:when xyz)
      xyz)))
  )

#;(module+ main
  (plot3d
   (list
    (points3d (parapoint (λ (θ z) (define r (skull    θ z)) (list (* r (cos θ))(* r (sin θ)) z)) (- π)  π  10 290) #:size 1 #:color 'brown)
    (points3d (parapoint (λ (θ z) (define r (neck     θ z)) (list (* r (cos θ))(* r (sin θ)) z)) (- π)  π   0  90) #:size 1 #:color 'green)
    (points3d (parapoint (λ (θ z) (define r (nose     θ z)) (list (* r (cos θ))(* r (sin θ)) z))  -.2  .2 120 200) #:size 1 #:color 'red)
    (points3d (parapoint (λ (θ z) (define r (lips     θ z)) (list (* r (cos θ))(* r (sin θ)) z))  -.4  .4  70 110) #:size 1 #:color 'yellow)
    (points3d (parapoint (λ (θ z) (define r (eyes     θ z)) (list (* r (cos θ))(* r (sin θ)) z))  -.4  .4 170 190) #:size 1 #:color 'blue)
    (points3d (parapoint (λ (θ z) (define r (eyebrow  θ z)) (list (* r (cos θ))(* r (sin θ)) z)) (- π)  π 195 205) #:size 1 #:color 'blue)
    (points3d (parapoint (λ (θ z) (define r (ears1    θ z)) (list (* r (cos θ))(* r (sin θ)) z)) (- π)  π 100 210) #:size 1 #:color 'gray)
    (points3d (parapoint (λ (θ z) (define r (ears2    θ z)) (list (* r (cos θ))(* r (sin θ)) z)) (- π)  π 195 300) #:size 1 #:color 'gray))
          #:width 800
          #:height 800
          #:angle 197
          #:altitude 7))

;--------------------------------------------
;result: EEF (escher-egg-face)
;--------------------------------------------
(module+ main
  (plot3d
   (points3d
    (parapoint
     (λ (θ z) (define r (face θ z))
       (match-define (list x y _)(list (* r (cos θ))(* r (sin θ)) z))
       (match-define (list ϕ t d)(xyz->ribbon x y z))
       (if (< (abs t) 10) (list x y z) #f))
     (- π)  π  0 300
     #:nr 30000)
    #:size 1)
   #:x-min -300 #:x-max 300
   #:y-min -300 #:y-max 300
   #:z-min    0 #:z-max 600
   #:width 800
   #:height 800
   #:angle 197
   #:altitude 7))
(module+ main
  (parameterize ([plot-decorations? #f])
    (plot3d
     (parametric-surface3d
      (λ (ϕ t)
        (match-define (list x y z)(ribbon->xyz ϕ t 0))
        (ribbon->xyz ϕ t (- (face (atan y x) z) R)))
      0   (* (/ 290 P) 2π) #:s-samples 1500
      -7 +40 #:t-samples 30
      #:color "navajowhite"
      #:line-color "navajowhite")
     #:x-min -300 #:x-max 300
     #:y-min -300 #:y-max 300
     #:z-min    0 #:z-max 600
     #:width 800
     #:height 800
     #:angle 210
     #:altitude 10)))

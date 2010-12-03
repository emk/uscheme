;;=========================================================================
;; Mandelbrot set generation.
;;
;; We assume the following external procedures: write-header, write-bit,
;; and flush-bits.  These allow us to generate *.pbm files without having
;; to subtantially increase the size of the language.

;;; Is the complex number cr,ci in the Mandelbrot set?
(define (mandelbrot? cr ci)
  (let loop [[zr 0.0] [zi 0.0] [n 0]]
    (if (>= n 50)
      ;; We haven't escaped in 50 iterations, so assume we're in the set.
      #t
      ;; Computer t = z*z+c.
      (let [[tr (+ (- (* zr zr) (* zi zi)) cr)]
            [ti (+ (* 2 (* zr zi)) ci)]]
        ;; If the magnitude of t is greater than 2.0, it's not in the
        ;; set (we compare against 4.0 to avoid a sqrt).  Otherwise, keep
        ;; looping.
        (if (>= (+ (* tr tr) (* ti ti)) 4.0)
          #f
          (loop tr ti (+ n 1)))))))

;;; Calculate a Mandelbrot set sz pixels square and print it to standard output
;;; as a PBM image.
(define (mandelbrot-set sz)
  (write-header sz)
  (let loop-y [[y 0]]
    (when (< y sz)
      (let loop-x [[x 0]]
        (when (< x sz)
          (let [[cr (- (* 2.0 (/ x sz)) 1.5)]
                [ci (- (* 2.0 (/ y sz)) 1.0)]]
            (write-bit (mandelbrot? cr ci))
            (loop-x (+ x 1)))))
      (flush-bits)
      (loop-y (+ y 1)))))

;; Run our program.
(mandelbrot-set 1024)

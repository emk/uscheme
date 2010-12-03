#lang scheme

;;=========================================================================
;; Low-level routines for printing a PBM image file.

;;; Write a PBM "rawbits" format header for an image sz pixels square.
(define (write-header sz)
  (printf "P4\n~a ~a\n" sz sz))

(define *byte-accum* 0)
(define *bits* 0)

;;; Write a single bit to our output.
(define (write-bit b)
  (set! *byte-accum*
        (bitwise-ior (arithmetic-shift *byte-accum* 1) (if b 1 0)))
  (set! *bits* (+ *bits* 1))
  (when (= *bits* 8)
    (write-byte *byte-accum*)
    (set! *byte-accum* 0)
    (set! *bits* 0)))

;;; Flush bits until we have a whole byte.
(define (flush-bits)
  (when (not (zero? *bits*))
    (write-bit 0)
    (flush-bits)))

;;=========================================================================
;; Mandelbrot set generation.

;;; Is the complex number cr,ci in the Mandelbrot set?
(define (mandelbrot? cr ci)
  (let loop [[zr 0.0] [zi 0.0] [n 0]]
    (if (>= n 50)
      ;; We haven't escaped in 50 iterations, so assume we're in the set.
      #t
      ;; Compute t = z*z+c.
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

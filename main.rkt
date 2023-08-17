#lang racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;;private
;;functions
;;all vectors returned are `immutable?`
(define (n:char->integer char)
  (cond
    ((char-ci=? char #\X) 10)
    (else (- (char->integer char) 48))))
(define (n:integer->char int)
  (if (= int 10) #\X (integer->char (+ int 48))))

(define (call-with-immutable-vector-returned num func)
  (let ((new (make-vector num)))
    (func new)
    (vector->immutable-vector new)))
(define (identity->vector str)
  (call-with-immutable-vector-returned
   identity-length
   (lambda (vec)
     (for (((char index) (in-indexed (in-string str))))
       (vector-set! vec index (n:char->integer char))))))
(define (vector-mul vec1 vec2)
  (call-with-immutable-vector-returned
   identity-length
   (lambda (new)
     (for (((v1 i) (in-indexed (in-vector vec1)))
           (v2 (in-vector vec2)))
       (vector-set! new i (* v1 v2))))))

(define (vector-sum vec)
  (for/fold ((r 0)) ((v (in-vector vec)))
    (+ r v)))

(define (invert v)
  (let/cc return
    (for ((n (in-naturals)))
      (cond ((= 1 (remainder (* n v) base)) (return n))))))

(define (plain-valid-identity? str . others)
  (define (vector-of-valid-integers? vec)
    (let/cc break
      (for ((v (in-vector vec)))
        (cond ((or (< v 0) (> v 10)) (break #f))))
      #t))

  (define (check-in-order v l)
    (andmap (lambda (p) (p v)) l))

  (and (= identity-length (string-length str))
       (let ((int-vector (identity->vector str)))
         (and (vector-of-valid-integers? int-vector)
              (check-in-order int-vector others)))))
;;constants
(define base 11)
(define identity-length 18)
(define weight-vector (vector-immutable 7 9 10 5 8 4 2 1 6 3 7 9 10 5 8 4 2 1))
(define inverted-weight-vector (vector->immutable-vector (vector-map invert weight-vector)))

;;exported functions
(define (valid-identity? str . others)
  (apply plain-valid-identity? str (lambda (int-vector) (= 1 (modulo (vector-sum (vector-mul int-vector weight-vector)) base))) others))
(define (correct-slot1 identity i)
  (cond ((plain-valid-identity?
          identity
          (lambda (int-vector)
            (let* ((product-vector (vector-mul int-vector weight-vector))
                   (sum (- (vector-sum product-vector) (vector-ref product-vector i))))
              (n:integer->char (modulo (* (- 1 sum) (vector-ref inverted-weight-vector i)) base))))))
        (else (error 'identity "Identity string ~s is invalid because it has incorrect length or contains invalid characters" identity))))

(require racket/contract racket/vector)
(provide (contract-out (valid-identity? (-> string? (-> any/c boolean?) ... any))
                       (correct-slot1 (-> string? (and/c exact-nonnegative-integer? (lambda (index) (< index 18))) any))))

(module* test racket/base
  (require racket/vector rackunit (submod ".."))

  (check-true (valid-identity? "11010519491231002X" (lambda (vec) (equal? (vector-immutable 1 9 4 9) (vector-copy vec 6 10)))))
  (check-false (valid-identity? "110105194912310020"))
  (check-true (char-ci=? #\X (correct-slot1 "110105194912310020" 17)))
  (check-exn exn:fail? (lambda () (correct-slot1 "11010519491231002" 17))))

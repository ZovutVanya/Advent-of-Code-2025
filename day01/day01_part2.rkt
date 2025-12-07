#lang racket

(define lines-list (file->lines "input"))

(define pass-by-counter 0)
(define full-rotations 0)

(define (process-line letter number position)
  (let-values ([(q r) (quotient/remainder number 100)])
    (set! full-rotations (+ full-rotations q))
    (cond
      [(eq? letter '#\L)
       (let ([new-position (- position r)])
         (if (< new-position 0)
             (begin
               (cond
                 [(not (eq? position 0)) (set! pass-by-counter (+ pass-by-counter 1))])
               (+ new-position 100))
             new-position))]
      [(eq? letter '#\R)
       (let ([new-position (+ position r)])
         (if (> new-position 99)
             (begin
               (cond
                 [(and (not (eq? position 0)) (not (eq? new-position 100)))
                  (set! pass-by-counter (+ pass-by-counter 1))])
               (- new-position 100))
             new-position))])))

(define (check-pass lst current-position)
  (if (empty? lst)
      '()
      (let* ([letter (string-ref (car lst) 0)]
             [number (string->number (substring (car lst) 1))]
             [new-position (process-line letter number current-position)])
        (cons new-position (check-pass (cdr lst) new-position)))))

(define results (check-pass lines-list 50))
(printf "~nPass by counter: ~a" pass-by-counter)
(printf "~nFull rotations: ~a" full-rotations)
(define number-of-zeros (count (λ (x) (= x 0)) results))
(printf "~nNumber of zeros: ~a" number-of-zeros)
(printf "~nResult: ~a" (+ pass-by-counter number-of-zeros full-rotations))

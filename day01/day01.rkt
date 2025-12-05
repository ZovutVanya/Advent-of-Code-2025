#lang racket

(define lines-list (file->lines "input"))

(displayln lines-list) ; (L68 L30 R48 L5 R60 L55 L1 L99 R14 L82)

(define (preprocess-number number)
  (if (> number 100)
      (remainder number 100)
      number))

(define (process-line letter number position)
  (cond
    [(eq? letter '#\L)
     (let ([new-position (- position (preprocess-number number))])
       (if (< new-position 0)
           (+ new-position 100)
           new-position))]
    [(eq? letter '#\R)
     (let ([new-position (+ position (preprocess-number number))])
       (if (>= new-position 100)
           (- new-position 100)
           new-position))]))

(define (check-code lst current-position)
  (if (empty? lst)
      '()
      (let* ([letter (string-ref (car lst) 0)]
             [number (string->number (substring (car lst) 1))]
             [new-position (process-line letter number current-position)])
        (cons new-position (check-code (cdr lst) new-position)))))

(define results (check-code lines-list 50))

(count (λ (x) (= x 0)) results) ; 1048

#| Part 2 |#

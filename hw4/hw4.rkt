
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define ones (lambda () (cons 1 ones)))
(define a 2)

(define (sequence low high stride) (if (> low high)
                                       null
                                       (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) (if (null? xs)
                                          null
                                          (map (lambda (x) (string-append x suffix)) xs)))

(define (list-nth-mod xs n) (cond
                              [(negative? n)  (error "list-nth-mod: negative number")]
                              [(null? xs)    (error "list-nth-mod: empty list")]
                              [ #t           (let ([i (remainder (length xs) n)])
                                               (car (list-tail xs (+ i 1))))]))

(define (stream-for-n-steps s n) (if (= n 0)
                                     null
                                     (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog

                                                  
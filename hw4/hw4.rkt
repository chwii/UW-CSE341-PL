
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) (if (> low high)
                                       null
                                       (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix) (if (null? xs)
                                          null
                                          (map (lambda (x) (string-append x suffix)) xs)))

(define (list-nth-mod xs n) (cond
                              [(negative? n)  (error "list-nth-mod: negative number")]
                              [(null? xs)    (error "list-nth-mod: empty list")]
                              [ #t           (let ([i (remainder n (length xs))])
                                               (car (list-tail xs i)))]))

(define (stream-for-n-steps s n) (if (= n 0)
                                     null
                                     (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if x
                              (cons "dan.jpg" (lambda () (f #f)))
                              (cons "dog.jpg" (lambda () (f #t)))))])
                       (lambda () (f #t))))

(define (stream-add-zero s)
  (letrec ([th (s)])
    (lambda () (cons (cons 0 (car th))
                     (lambda () (stream-add-zero (cdr th)))))))

(define (cycle-lists xs ys) 
  (define (helper n) (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                      (helper (+ n 1)))))
  (helper 0))

(define (vector-assoc v vec)
  (define len (vector-length vec))
  (define (helper idx) (if (< idx len)
                           (let ([p (vector-ref vec idx)])
                           (if (pair? p)
                               (if (equal? v (car p)) p (helper (+ idx 1)))
                               (helper (+ idx 1))))
                           #f))
  (helper 0))

(define (cached-assoc xs n)
  (letrec ([idx 0]
           [cache (make-vector n #f)]
           [f (lambda (v)
                (let ([flag (vector-assoc v cache)])
                  (if flag
                      flag
                      (let ([ans (assoc v xs)])
                        (if ans
                            (begin
                              (vector-set! cache idx ans)
                              (set! idx (remainder (+ idx 1) n))
                              ans)
                            #f)))))])
    f))

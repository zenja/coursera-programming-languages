
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ stride low) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (define (f x)
    (cons (if (= 0 (remainder x 5)) (- x) x)
          (lambda () (f (+ x 1)))))
  (f 1))

(define (dan-then-dog)
  (define (f x)
    (cons (if (= 0 (remainder x 2)) "dan.jpg" "dog.jpg")
          (lambda () (f (+ x 1)))))
  (f 2))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
                   (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (define (f n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

(define (vector-assoc v vec)
  (define (find-at pos)
    (if (>= pos (vector-length vec))
        #f
        (let ([ele (vector-ref vec pos)])
          (if (and (pair? ele) (equal? v (car ele)))
              ele
              (find-at (+ pos 1))))))
  (find-at 0))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next-pos 0]
           [f (lambda (v)
                (let ([hit (vector-assoc v cache)])
                  (if hit
                      hit
                      (let ([content (assoc v xs)])
                        (begin
                          (define content (assoc v xs))
                          (if content
                              (begin
                                ; set cache
                                (vector-set! cache (remainder next-pos n) content)
                                ; increment pos
                                (set! next-pos (+ 1 next-pos))
                                ; return the content
                                content)
                              #f))))))])
    f))
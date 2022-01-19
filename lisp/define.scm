(define a (lambda (x) (lambda () (lambda () x))))
(display (((a 5))))
;(define a '(1 2 3))
;(define b a)
;(set! a '(1 2 3 4))
;(display a)
;(display b)

;(define c (lambda (x) (lambda () (set! x (+ x 1)))))

;(define d (c 3))
;(display (d))
;(display (d))

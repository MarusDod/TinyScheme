(define fact (lambda (x)
  (if (= x 0)
      1
      (* x (fact (- x 1))))))

(displayln (fact 4))
(display x)

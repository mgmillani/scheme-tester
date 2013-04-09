(require racket/include)
(include "testsuite.rkt")
(require 'testSuite)
(define input (list (list 1 2) (list 3 4) (list 7 8)))
(define output (list 3 7 8))

(define (addem lst)

	(+ (first lst) (last lst))
)

(display (itemize (testFunctionEasy addem input output equal?)))
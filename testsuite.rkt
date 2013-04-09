

(module testSuite racket
(provide (all-defined-out))

;recebe uma funcao para ser testada com cada elemento de input.
;verifica se a compareFunction retorna true ao comparar a saida da funcao com o respectivo output esperado
;retorna uma lista com as mensagens no formato especificado por right e wrong
;a funcao wrong deve receber tres argumentos: o input, o valor da funcao e o valor esperado
;a funcao right recebe somente dois: o input e o resultado
;ambas devem retornar a mensagem adequada
;a funcao endMessage deve receber o numero de testes que passaram e que falharam e retornar a mensagem adequada
(define (testFunction func input output compareFunction right wrong endMessage)

	(testFormat (listCompare
					compareFunction
					(applyFunction func input)
					output
					input
					right
					wrong
					)
					endMessage
	)

)

(define (testFunctionEasy func input output compareFunction)

	(testFunction func input output compareFunction compareOk compareBad simpleMessage)

)


(define (applyFunction function input)
	(cond
		[(empty? input) empty]
		[else (cons (function (first input))
					(applyFunction function (rest input))
			)]
	)
)

(define (compareOk input given)

	(format "f(~a) is ~a" input given )

)

(define (compareBad input given correct)

	(format "f(~a) is ~a but should be ~a" input given correct)
)

(define (simpleMessage right wrong)

	(format "Tests: ~a passed, ~a failed" right wrong)
)

(define (listCompare compareFunction listA listB identifiers right wrong)

	(cond
	[(empty? listA)
		empty]
	[(empty? listB)
		empty]
	[(compareFunction (first listA) (first listB))
		(cons (list (right (first identifiers) (first listA)) #t) (listCompare compareFunction (rest listA) (rest listB) (rest identifiers) right wrong))
	]
	[else
		(cons (list (wrong (first identifiers) (first listA) (first listB)) #f) (listCompare compareFunction (rest listA) (rest listB) (rest identifiers) right wrong))]
	)
)

(define (testFormat results messageF)

	(testFormatAux results 0 0 messageF)
)

(define (testFormatAux results right wrong messageF)

	(cond
		[(empty? results) (cons (messageF right wrong) empty)]
		[else (cons (first (first results)) (cond
			[(last (first results)) (testFormatAux (rest results) (+ 1 right) wrong messageF)]
			[else (testFormatAux (rest results) right (+ 1 wrong) messageF)]
			))]
	)
)

(define (itemize lst)

	(cond
		[(empty? lst) ""]
		[else (string-append (format "~a\n" (first lst)) (itemize (rest lst)))]

	)
)
)

; The MIT-scheme itself evalutes operands from right to left

; The scheme evaluator in text varies, for the ones implemented with list-of-values

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))
; they are from right to left as well,


; For instance if we put the following code in ex4.14 or MIT-scheme, it will give us
; (sentence (noun-phrase (article eats) (noun cat)) (verb the))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))
(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))
(define (parse-word word-list)
  ;(require (not (null? *unparsed*)))
  ;(require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    ;(require (null? *unparsed*))
    sent))
(parse '(the cat eats))



; In a nutshell it's important because we expect the order of the list of input sentence
; corresponds to the order of the list of language strcture body, which means
; the forefront elements of the list of a strcture body
; gets evaled first, namely use parse-word to extract word from unparsed list first
; so it has to be left to right


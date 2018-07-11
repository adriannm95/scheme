;Scheme Examples

;Return true  if the list is even
(define (evenL L)
    (cond
        ((null? L)#t)
        ((null? (cdr L))#f)
        (else(evenL (cddr L)))
    )
)

;Return true if the list is odd
(define (oddL V)
    (cond
    ((null? V)'())
    ((null? (cdr V))(car V))
    (else(cons (car V)(oddL (cddr V))))
    )
)

;Return how deep is the character A
; not Contain C on A , return 0.

(define (deepC C A)
  (cond
    ((null? C)'())
    ((list? (car C))
     (let (( B(deepC (car C) A)))
       (if(= B 0)
       0
       (+ B 1))))
    ((equal? (car C)A)1)
    (else(deepC (cdr C)A))
  )
)

;Return the last list Element.
(define (lastN L)
    (cond
        ((null? L)L)
        ((null? (cdr L))'())
        (else (cons (car L)(lastN (cdr L))))
    )
)

;Print the N first Elements
(define (nFelements V N)
    (if(or(<= N 0)(null? V))
        '()
        (cons (car V) (nFelements (cdr V)(- N 1)))
    )
)
;Return list.length
(define (long L)
    (cond
        ((null? L)0)
        ((not (null? (car L)))(+ 1 (long (cdr L))))
    )
)
;Delete the L element of the list
(define (eliP L P)
    (cond
        ((null? L)'())
        ((<= P 0)L)
        ((= P 1)(cdr L))
        (else (cons (car L) (eliP (cdr L) (- P 1))))
    )
)

;Delete the N-N elements of the list
(define (ndelete lst n)
  (let recur ((i 1)
              (rest lst))
    (cond ((null? rest) '())
          ((= i n) (recur 1 (cdr rest)))
          (else (cons (car rest) (recur (+ i 1) (cdr rest)))))))
;Print Firts  equals N Elements
(define (firstNE V)
    (cond
        ((null? V)'())
        ((null? (cdr V))V)
        ((not (equal? (car V) (cadr V)))(list (car V)))
        ((equal? (car V) (cadr V)) (cons (car V) (firstNE (cdr V))))
    )
)

(define (profundidad C A)
    (cond
        ((null? C) 0)
        ((list? (car C))
                (let ((B (profundidad (car C) A)))
                    (if (= B 0)
                        (profundidad (cdr C) A)
                        (+ B 1)
                    )
                )
        )
        ((equal? (car C) A) 1)
        (else (profundidad (cdr C) A))
    )
)


(define (intersecc A B)
    (cond
        ((null? A)'())
        ((null? B)'())
        ((null? (cdr B))A)
        ((conteniIn (car A) B)(cons (car A) (intersecc (cdr A) B)))
        (else (intersecc (cdr A) (cdr B)))
    )
)

(define (conteniIn A B)
    (cond
        ((null? B)#f)
        ((equal? A (car B))#t)
        (else (conteniIn A (cdr B)))
    )
)

(define (zipp A B)
    (cond
        ((null? A)B)
        ((null? B)A)
        (else(cons (car A)(zipp B(cdr A))))
    )
)

(define (parF L)
    (cond
        ((null? L)#t)
        ((null? (cdr L))#f)
        (else ( parF (cddr L)))
    )
)

(define (long L)
    (cond
    ((null? L)0)
    (not(null? L)(+ 1 (long (cdr L))))
    )
)

(define (mitad L)
       (if(even? (long L))
        (midAuxL 1 (-  (long L) 1))
        (midAuxL 1 (long L))
       )
)

(define (midAuxL actu longi)
    (cond
    ((null? L) '())
    ((null? (cdr L)) (car L))
    ((= actu longi)(list (car L)))
    (else (cons (car L) (midAux(cdr L) (+ 1 actu ) ( - longi 1))))
    )
)

(define (union a b)
    (cond
        ((null? b) a)
        ((unionAux (car b) a) (union a (cdr b)))
        (else (union (cons (car b) a) (cdr b)))
    )
)

(define (unionAux n lst)
    (cond
        ((null? lst) #f)
        ((eqv? n (car lst)) #t)
        (else (unionAux n (cdr lst)))
    )
)



(define (contenidoU E B)
    (cond
    ((null? B)#f)
    ((equal? E (car B)))
    (else ( contenidoU E (cdr B)))
    )
)


(define (principio V)
    (cond
        ((null? V)'())
        ((null? (cdr V))V)
        ((not (equal? (car V) (cadr V)))(list (car V)))
        ((equal? (car V) (cadr V)) (cons (car V) (principio (cdr V))))
    )
)

(define (profundidad C A)
    (cond
        ((null? C) 0)
        ((list? (car C))
                (let ((B (profundidad (car C) A)))
                    (if (= B 0)
                        (profundidad (cdr C) A)
                        (+ B 1)
                    )
                )
        )
        ((equal? (car C) A) 1)
        (else (profundidad (cdr C) A))
    )
)

(define (final L)
   (aux (invierte L))
)


(define (aux A)
    (cond
        ((null? A)'())
        ((null? (cdr A))A)
        ((not(equal? (car A) (cadr A)))(list (car A)))
        ((equal? (car A) (cadr A))(cons (car A)(aux (cdr A))))
    )
)

;Invert the list
(define (invert A)
    (cond
        ((null? A) '())
        ((null? (cdr A)) A)
        (else
              (append (invert (cdr A)) (list (car A)))
        )
    )
)

;Return the max deep of the list
(define (deep List)
    (cond
        ((null? List) 1)
        ((not(list? List)) 0)
        ((list? (car List))
            (let
                ((A (+(deep (car List))1)) (B (deep (cdr List))))
                (if (> A B)
                    A
                    B
                )
            )
        )
        (else
            (deep (cdr List))
        )
    )
)
;Return true or false if the list is palindrome
(define (palindrome L)
    (if (compare L (invert L))
        #t
        #f
    )
)

(define (compare A B)
    (cond
        ((null? A)#t)
        ((equal? (car A) (car B)) (compare (cdr A) (cdr B)))
        ((not(equal? (car A) (car B))) #f)
    )
)

;Print the evenPos list
(define (evenPos V)
    (cond
        ((null? V)`())
        ((null?(cdr V))`())
        ((cons (cadr V) (evenPos (cddr V))))
    )

)

;Flatten a list
(define (flatten V)
    (if(null? V)
        '()
        (if(not(list?(car V)))
            (cons (car V) (flatten (cdr V)))
            (append (flatten (car V)) (flatten (cdr V)))
        )
    )
)

;Return true or false if B contains A
(define (containsL A B)
    (cond
        ((and(null? A) (null? B))#t)
        ((null? B)#f)
        ((null? A)#t)
        ((aux A B)#t)
        (else (containsL A (cdr B)))
    )
)
;Aux of Containns L
(define (aux A B)
    (cond
        ((null? A)#t)
        ((null? B)#f)
        ((equal?(car A)(car B))(aux (cdr A) (cdr B)))
        (else #f)
    )
)


;Difference A B
(define (difference A B)
    (cond
        ((null? A)'())
        ((null? B)A)
        ((containsD (car A) B) (difference (cdr A) B))
        (else (cons (car A)(difference (cdr A) B)))
    )
)

;Difference aux
(define (containsD E L)
    (cond
    ((null? L)#f)
    ((equal? E (car L))#t)
    (else (containsD E (cdr L) ))
    )
)

;Swap element of the list
(define (swap L A C)
    (cond
        ((null? L)`())
        ((equal? (car L) A)(cons C (swap (cdr L) A C)))
        (else (cons (car L) (swap (cdr L) A C)))
    )
)

(define (duplicate A)
    (cond
        ((null? A) '())
        (else (
        append (append (list (car A)) (list (car A)))
                (duplicate (cdr A))

        ))
    )
)

;Return the first-mid elements
(define (mid L)
       (if(even? (long L))
        (midAux L 1 (-  (long L) 1))
        (midAux L 1 (long L))
       )
)

(define (long L)
    (cond
    ((null? L)0)
    (not(null? L)(+ 1 (long (cdr L))))
    )
)

(define (midAux L actu longi)
    (cond
    ((null? L) '())
    ((null? (cdr L)) (car L))
    ((= actu longi)(list (car L)))
    (else (cons (car L) (midAux (cdr L) (+ 1 actu ) ( - longi 1))))
    )
)
(define (ordenada L)
    (cond
        ((null? L)#t)
        ((null? (cdr L))#t)
        ((<= (car L) (cadr L)) (ordenada (cdr L)))
        (else #f)
    )
)


(define (quitaRepetidos L)
    (cond
        ((null? L)'())
        ((null? (cdr L))(list (car L)))
        ((equal? (car L) (cadr L)) (quitaRepetidos(cdr L)))
        (else ( cons (car L) (quitaRepetidos (cdr L))))
    )
)


(define (count L  E)
    (containsC (flattenC L) E)
)

(define (flattenC L)
     (cond
        ((null? L)'())
        ((list? (car L))(append (flattenC (car L)) (flattenC (cdr L))))
        (else (cons (car L) (flattenC(cdr L))))
    )
)

(define (containsC L E)
    (cond
        ((null? L)0)
        ((equal? (car L ) E) (+ (containsC (cdr L) E) 1))
        (else (containsC (cdr L) E))
    )
)

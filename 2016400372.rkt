
#lang scheme
; compiling: yes
; complete: yes
; 2016400372

; All the code is written by Mehmet Sefa BALIK for CMPE260 project in Bogazici University
; No stolen codes here, there is just hours of hard work
; Thanks to Professor Albert Ali Salah and Assistant Ozlem for introducing me with such a nice functional programming language

; (card-color one-card) -> list?
; one-card: list?
;
; returns the color of one card
;
; > (card-color '(H . A))
; => 'red
(define (card-color one-card)
        (cond ((equal? (car one-card) 'D) 'red)
                ((equal? (car one-card) 'H) 'red)
                ((equal? (car one-card) 'S) 'black)
                ((equal? (car one-card) 'C) 'black)
                (else "not a card")))

; (card-rank one-card) -> number?
; one-card: list?
;
; returns the rank of one card
;
; > (card-rank '(H . A))
; 11
(define (card-rank one-card)
        (cond ((number? (cdr one-card)) (cdr one-card))
                ((equal? (cdr one-card) 'A) 11)
                ((equal? (cdr one-card) 'Q) 10)
                ((equal? (cdr one-card) 'J) 10)
                ((equal? (cdr one-card) 'K) 10)))

; (all-same-color list-of-cards) -> boolean?
; list-of-cards: list?
;
; returns #t if all the cards in list-of-cards have same color, #f otherwise
;
; > (all-same-color '((H . A) (H . 1) (D . 2))
; => #t
(define (all-same-color list-of-cards)
        (cond   ((equal? (cdr list-of-cards) '()) #t)
                ((equal? (card-color (car list-of-cards)) (card-color (car (cdr list-of-cards))))
                (all-same-color (cdr list-of-cards)))
               (else #f)))

; (fdraw list-of-cards held-cards) -> list?
; list-of-cards: list?
; held-cards: list?
;
; returns a new list of held-cards after the draw action is taken
;
; > (fdraw '((H . 3) (H . 1)))
; => '((H . 3))
(define (fdraw list-of-cards held-cards)
        (append (list (car list-of-cards)) held-cards))

; (fdiscard list-of-cards list-of-moves goal held-cards) -> list?
; list-of-cards: list?
; list-of-moves: list?
; goal: number?
; held-cards: list?
;
; returns a new list of held-cards after the action discard is taken
;
; >(fdiscard '((C . 3) (C . 2)) '(draw discard) 33 '((H . 3) (H . 2)))
; => '((H . 2))
(define (fdiscard list-of-cards list-of-moves goal held-cards)
        (define (calc-best-to-discard held-cards best)
                (cond   ((null? (cdr held-cards)) best)
                        ((< (card-rank best) (card-rank (car (cdr held-cards)))) (calc-best-to-discard (cdr held-cards) (car (cdr held-cards))))
                        ((>= (card-rank best) (card-rank (car (cdr held-cards)))) (calc-best-to-discard (cdr held-cards) best))))
        (remove (calc-best-to-discard held-cards (car held-cards)) held-cards))

; (find-steps list-of-cards list-of-moves goal) -> list?
; list-of-cards: list?
; list-of-moves: list?
; goal: number?
;
; returns a new list of steps that is a list of pairs of moves and corresponding cards along the game
;
; >(find-steps '((C . 3) (C . 2) (H . 1)) '(draw discard) 33)
; => '((draw (C . 3)) (draw (C . 2)) (discard (C . 3)))
(define (find-steps list-of-cards list-of-moves goal)
        (find-steps-worker list-of-cards list-of-moves goal '()))
(define (find-steps-worker list-of-cards list-of-moves goal listcard)
        (define (calc-best-to-discard held-cards best)
                (cond   ((null? (cdr held-cards)) best)
                        ((< (card-rank best) (card-rank (car (cdr held-cards)))) (calc-best-to-discard (cdr held-cards) (car (cdr held-cards))))
                        ((>= (card-rank best) (card-rank (car (cdr held-cards)))) (calc-best-to-discard (cdr held-cards) best))))
        (cond   ((null? list-of-moves) listcard)
                ((and (null? list-of-cards) (equal? (car list-of-moves) 'draw)) listcard)
                ((and (null? (find-held-cards listcard)) (equal? (car list-of-moves) 'discard)) listcard)
                ((< goal (calc-playerpoint (find-held-cards listcard))) listcard)
                ( (equal? (car list-of-moves) 'draw)
                        (find-steps-worker (cdr list-of-cards) (cdr list-of-moves) goal (append listcard (list (list (car list-of-moves) (car list-of-cards))))))
                ((equal? (car list-of-moves) 'discard)
                        (find-steps-worker (cdr list-of-cards) (cdr list-of-moves) goal (append listcard (list (list (car list-of-moves)
                        (calc-best-to-discard (find-held-cards listcard) (car (find-held-cards listcard))) )))))))

; (find-held-cards list-of-steps) -> list?
; list-of-steps: list?
;
; returns the list of held-cards after the list-of-steps is applied
;
; >(find-held-cards '((draw (C . 3)) (draw (C . 2)) (discard (C . 3))))
; => '((C . 2))
(define (find-held-cards list-of-steps)
        (find-held-cards-worker list-of-steps '()))
(define (find-held-cards-worker listofsteps heldcard)
         (cond   ((null? listofsteps) heldcard)
                 ((equal? (car (car listofsteps)) 'draw)
                        (find-held-cards-worker (cdr listofsteps) (append heldcard (cdr (car listofsteps)))))
                 ((equal? (car (car listofsteps)) 'discard)
                        (find-held-cards-worker (cdr listofsteps) (remove (car (cdr (car listofsteps))) heldcard)))))

; (calc-playerpoint list-of-cards) -> number?
; list-of-cards: list?
;
; calculates and returns the player point of list-of-cards
;
; > (list-of-cards '((H . A) (H . 1) (D . 2))
; => 13
(define (calc-playerpoint list-of-cards)
        (calc-playerpoint-worker list-of-cards 0))
(define (calc-playerpoint-worker list-of-cards score)
        (cond   ((null? list-of-cards) score)
                (else (calc-playerpoint-worker (cdr list-of-cards) (+ score (card-rank (car list-of-cards)))))))

; (calc-score list-of-cards goal) -> number?
; list-of-cards: list?
; goal: number?
;
; calculates and returns the final score
;
; >(calc-score '((C . 3) (C . 2)) 11)
; => 3
(define (calc-score list-of-cards goal)
        (define (calc-prescore list-of-cards goal)
        (cond   ((< (calc-playerpoint list-of-cards) goal) (- goal (calc-playerpoint list-of-cards)))
                ((> (calc-playerpoint list-of-cards) goal) (* 5 (- (calc-playerpoint list-of-cards) goal)))))
        (cond   ((null? list-of-cards) (floor (/ goal 2)))
                ((all-same-color list-of-cards) (floor (/ (calc-prescore list-of-cards goal) 2)))
                ((not (all-same-color list-of-cards)) (calc-prescore list-of-cards goal))))

; (play list-of-cards list-of-moves goal) -> number?
; list-of-cards: list?
; list-of-moves: list?
; goal: number?
;
; returns final score after processing the moves in the list-of-moves
;
; >(play '((C . 3) (C . 2) (C . 1)) '(draw draw discard) 11)
; => 4
(define (play list-of-cards list-of-moves goal)
        (play-worker list-of-cards list-of-moves goal '()))
(define (play-worker list-of-cards list-of-moves goal held-cards)
        (cond   ((null? list-of-moves) (calc-score held-cards goal))
                ((and (null? held-cards) (equal? (car list-of-moves) 'discard)) (calc-score held-cards goal))
                ((and (null? list-of-cards) (equal? (car list-of-moves) 'draw)) (calc-score held-cards goal))
                ((< goal (calc-playerpoint held-cards)) (calc-score held-cards goal))
                ((equal? (car list-of-moves) 'draw) (play-worker (cdr list-of-cards) (cdr list-of-moves) goal (fdraw list-of-cards held-cards)))
                ((equal? (car list-of-moves) 'discard) (play-worker list-of-cards (cdr list-of-moves) goal (fdiscard list-of-cards list-of-moves goal held-cards)))))

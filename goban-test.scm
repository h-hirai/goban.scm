#! /usr/bin/env gosh

(use gauche.test)
(use srfi-1)

(test-start "goban")
(add-load-path ".")
(load "goban")

(test-section "board-ref")
(define *board-ko-1*
  '((e e e e e)
    (e w w w e)
    (e w b w e)
    (e b e b e)
    (e b b b e)))
(test* "board-ref basic"
       '(b w e)
       (list
        (board-ref *board-ko-1* '(3 . 1))
        (board-ref *board-ko-1* '(2 . 1))
        (board-ref *board-ko-1* '(3 . 0))))
(test* "board-ref out of board"
       '(o o o o o o o o)
       (list
        (board-ref *board-ko-1* '(-1 .  2))
        (board-ref *board-ko-1* '( 5 .  2))
        (board-ref *board-ko-1* '( 2 . -1))
        (board-ref *board-ko-1* '( 2 .  5))
        (board-ref *board-ko-1* '(-1 . -1))
        (board-ref *board-ko-1* '(-1 .  5))
        (board-ref *board-ko-1* '( 5 . -1))
        (board-ref *board-ko-1* '( 5 .  5))))

(test-section "board-put")
(test* "board-put center"
       '((e e e e e)
         (e w w w e)
         (e w w w e)
         (e b e b e)
         (e b b b e))
       (board-put *board-ko-1* '(2 . 2) 'w))
(test* "board-put corner00"
       '((b e e e e)
         (e w w w e)
         (e w b w e)
         (e b e b e)
         (e b b b e))
       (board-put *board-ko-1* '(0 . 0) 'b))
(test* "board-put corner04"
       '((e e e e b)
         (e w w w e)
         (e w b w e)
         (e b e b e)
         (e b b b e))
       (board-put *board-ko-1* '(0 . 4) 'b))
(test* "board-put corner40"
       '((e e e e e)
         (e w w w e)
         (e w b w e)
         (e b e b e)
         (b b b b e))
       (board-put *board-ko-1* '(4 . 0) 'b))
(test* "board-put corner44"
       '((e e e e e)
         (e w w w e)
         (e w b w e)
         (e b e b e)
         (e b b b b))
       (board-put *board-ko-1* '(4 . 4) 'b))

(test-section "opponent-of?")
(test* "(2 . 2)(black) is opponent of white?"
       #t
       (opponent-of? 'w *board-ko-1* '(2 . 2)))
(test* "(2 . 1)(white) is opponent of white?"
       #f
       (opponent-of? 'w *board-ko-1* '(2 . 1)))
(test* "(2 . 0)(empty) is opponent of white?"
       #f
       (opponent-of? 'w *board-ko-1* '(2 . 0)))
(test* "(2 . 5)(out of board) is opponent of white?"
       #f
       (opponent-of? 'w *board-ko-1* '(2 . 5)))
(test* "(2 . 2)(black) is opponent of black?"
       #f
       (opponent-of? 'b *board-ko-1* '(2 . 2)))
(test* "(2 . 1)(white) is opponent of black?"
       #t
       (opponent-of? 'b *board-ko-1* '(2 . 1)))
(test* "(2 . 0)(empty) is opponent of black?"
       #f
       (opponent-of? 'b *board-ko-1* '(2 . 0)))
(test* "(2 . 5)(out of board) is opponent of black?"
       #f
       (opponent-of? 'b *board-ko-1* '(2 . 5)))

(test-section "get-chain")
(test* "get-chain alone stone"
       '((2 . 2))
       (get-chain *board-ko-1* '(2 . 2))
       (cut lset= equal? <> <>))
(test* "get-chain many stones"
       '((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 3))
       (get-chain *board-ko-1* '(1 . 1))
       (cut lset= equal? <> <>))
(test* "get-chain no stone"
       '()
       (get-chain *board-ko-1* '(3 . 2))
       (cut lset= equal? <> <>))
(test* "get-chain many stones at edge"
       '((3 . 1) (3 . 3) (4 . 1) (4 . 2) (4 . 3))
       (get-chain *board-ko-1* '(3 . 3))
       (cut lset= equal? <> <>))
(test* "get-chain out of board"
       '()
       (get-chain *board-ko-1* '(5 . 3))
       (cut lset= equal? <> <>))

(test-section "alive-at?")
(define *board-alive-test-1*
  '((e b w b e)
    (e w b w e)
    (e w b w e)
    (e w b w e)
    (e b w b e)))
(define *board-alive-test-2*
  '((e b w b e)
    (e b w b e)
    (e b w b e)
    (e w b w e)
    (e e e e e)))
(define *board-alive-test-3*
  '((e e e w b)
    (e e e e b)
    (e e e b w)
    (e e e b w)
    (e e e b w)))
(test* "center alive"
       #t
       (alive-at? *board-alive-test-1* '(2 . 1)))
(test* "center dead"
       #f
       (alive-at? *board-alive-test-1* '(2 . 2)))
(test* "edge alive"
       #t
       (alive-at? *board-alive-test-2* '(2 . 1)))
(test* "edge dead"
       #f
       (alive-at? *board-alive-test-2* '(2 . 2)))
(test* "corner alive"
       #t
       (alive-at? *board-alive-test-3* '(0 . 4)))
(test* "corner dead"
       #f
       (alive-at? *board-alive-test-3* '(4 . 4)))
(test* "no chain"
       #f
       (alive-at? *board-alive-test-3* '(4 . 0)))
(test* "out of board"
       #f
       (alive-at? *board-alive-test-1* '(5 . 5)))


(test-section "can-put?")
(define *board-canput-test-1*
  '((e w e e e e e)
    (w e w e e e e)
    (e w e e e e e)
    (w e e e e e b)
    (e e e e e b e)
    (e e e e b e b)
    (e e e e e b e)))
(define *board-canput-test-2*
  '((e b w e e)
    (b w e e e)
    (e e e b w)
    (e e b w b)
    (e e b w e)))
(test* "on stone"
       #f
       (can-put? *board-canput-test-1* '(0 . 1) 'b #f))
(test* "out of board"
       #f
       (can-put? *board-canput-test-1* '(0 . 10) 'b #f))
(test* "have liberty"
       #t
       (can-put? *board-canput-test-1* '(3 . 3) 'b #f))
(test* "have no liberty (opponent color)"
       #f
       (can-put? *board-canput-test-1* '(0 . 0) 'b #f))
(test* "have no liberty (same color)"
       #t
       (can-put? *board-canput-test-1* '(0 . 0) 'w #f))
(test* "have no liberty and can capture"
       #t
       (can-put? *board-canput-test-2* '(4 . 4) 'b #f))
(test* "have no liberty and can capture 2"
       #t
       (can-put? *board-canput-test-2* '(0 . 0) 'w #f))
(test* "last liberty"
       #f
       (can-put? (board-put *board-canput-test-2* '(2 . 0) 'w) '(0 . 0) 'b #f))
(test* "ko"
       #f
       (can-put? *board-canput-test-2* '(0 . 0) 'w '(0 . 0)))

(test-section "put-stone")
(define *board-put-stone-test-1*
  '((e e e e e)
    (e e e e e)
    (e e e e e)
    (e e e e e)
    (e e e e e)))
(define *board-put-stone-test-2* *board-canput-test-2*)
(define *board-put-stone-test-3*
  '((e w e e b)
    (b e w e w)
    (e w b w e)
    (e b e b e)
    (e b w b e)))
(test* "empty board"
       (list '((e e e e e)
               (e e e e e)
               (e e e e e)
               (e b e e e)
               (e e e e e)) 0 #f)
       (receive actual (put-stone *board-put-stone-test-1* '(3 . 1) 'b)
         actual))
(test* "capture single stone"
       (list '((e b w e e)
               (b w e e b)
               (e e e b e)
               (e e b w b)
               (e e b w e)) 1 #f)
       (receive actual (put-stone *board-put-stone-test-2* '(1 . 4) 'b)
         actual))
(test* "capture multiple stones"
       (list '((e b w e e)
               (b w e e e)
               (e e e b w)
               (e e b e b)
               (e e b e b)) 2 #f)
       (receive actual (put-stone *board-put-stone-test-2* '(4 . 4) 'b)
         actual))
(test* "ko"
       (list '((w e w e e)
               (b w e e e)
               (e e e b w)
               (e e b w b)
               (e e b w e)) 1 '(0 . 1))
       (receive actual (put-stone *board-put-stone-test-2* '(0 . 0) 'w)
         actual))
(test* "not ko"
       (list '((e w e e b)
               (b e w e w)
               (e w e w e)
               (e b w b e)
               (e b w b e)) 1 #f)
       (receive actual (put-stone *board-put-stone-test-3* '(3 . 2) 'w)
         actual))

(test-end)

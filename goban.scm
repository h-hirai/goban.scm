#! /usr/bin/env gosh

(use srfi-1)

;; color is symbol 'b (black) or 'w (white) or
;;                 'o (out of board) or 'e (empty)
;; board is list of lists of colors
;; point is cons cell of integer and integer

;; board-ref :: (board point) -> color
(define (board-ref board point)
  (list-ref (list-ref board (car point) '()) (cdr point) 'o))

;; board-put :: (board point color) -> board
(define (board-put board point color) ;; from OSS WEB
  (receive (ls0 ls) (split-at board (car point))
    (receive (cs0 cs) (split-at (car ls) (cdr point))
      (append ls0 (cons (append cs0 (cons color (cdr cs))) (cdr ls))))))

;; opponent-of? :: (color board point) -> bool
(define (opponent-of? color board point)
  (let1 point-color (board-ref board point)
     (or (and (eq? color 'b) (eq? point-color 'w))
         (and (eq? color 'w) (eq? point-color 'b)))))

;; up, down, left, right :: point -> point
(define (up    point) (cons (- (car point) 1) (cdr point)))
(define (down  point) (cons (+ (car point) 1) (cdr point)))
(define (left  point) (cons (car point) (- (cdr point) 1)))
(define (right point) (cons (car point) (+ (cdr point) 1)))
;; around-of :: point -> [point](list)
(define (around-of point)
  (list (up point) (down point) (left point) (right point)))

;; get-chain :: (board point) -> [point](list)
(define (get-chain board point)
  (let1 chain-color (board-ref board point)
    (define (get-points-of-chain point chain)
      (if (or (not (eq? (board-ref board point) chain-color))
              (member point chain))
          chain
          (fold get-points-of-chain (cons point chain)
                (around-of point))))
    (if (or (eq? chain-color 'e)
            (eq? chain-color 'o))
        '()
        (get-points-of-chain point '()))))

;; alive-at? :: (board point) -> bool
(define (alive-at? board point)
  (any (lambda (stone)
         (any (lambda (neighbor)
                (eq? 'e (board-ref board neighbor)))
              (around-of stone)))
       (get-chain board point)))

;; can-put? :: (board point color point) -> bool
(define (can-put? board point color ko-point)
  (and (eq? 'e (board-ref board point))
       (not (equal? point ko-point))
       (let* ((new-board (board-put board point color))
              (alive? (cut alive-at? new-board <>)))
         (or (alive? point)
             (any (every-pred (cut opponent-of? color new-board <>)
                              (complement alive?))
                  (around-of point))))))

;; put-stone :: (board point color) -> [board int ko](values)
;; ko is #f or point
;; this procedure doesn't check validity of point
(define (put-stone board point color)
  (define (remove-stone p b) (board-put b p 'e))
  (let* ((new-board (board-put board point color))
         (alive? (cut alive-at? new-board <>))
         (captured-stones
          (apply lset-union equal?
                 (map (cut get-chain new-board <>)
                      (filter (every-pred (cut opponent-of? color board <>)
                                          (complement alive?))
                              (around-of point)))))
         (ko
          (and (= (length captured-stones) 1)
               (= (length (get-chain new-board point)) 1)
               (not (alive? point))
               (car captured-stones))))
    (values
     (fold remove-stone new-board captured-stones)
     (length captured-stones)
     ko)))

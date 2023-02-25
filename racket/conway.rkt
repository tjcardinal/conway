#lang racket/gui

; Conway logic
; Active points are represented by a set of x y pairs
;  e.g. (set '(1 2) '(3 4))

; Process to calculate next step:
;  Create set of all active points plus all adjacent non-active points
;  Filter for points from this set that:
;   Have 3 alive (i.e. from orignal set) neighbors
;   Have 2 alive neighbors and are alvie

(define (next-step alive-points)
  (for/set ([point-to-check (points-and-neighbors alive-points)]
            #:when (will-be-alive? alive-points point-to-check))
    point-to-check)) 

(define (points-and-neighbors points)
  (set-union points (apply set-union (set-map points neighbors))))

(define (neighbors point)
 (let ([x (car point)]
       [y (cadr point)])
  (for*/set([i (in-range -1 2)]
            [j (in-range -1 2)]
            #:unless (and (equal? i 0) (equal? j 0)))
    (list (+ x i) (+ y j)))))

(define (will-be-alive? alive-points point)
  (let ([neighbors (alive-neighbors-count alive-points point)])
  (cond
    [(= 3 neighbors) #t]
    [(and (= 2 neighbors) (set-member? alive-points point)) #t]
    [else #f])))

(define (alive-neighbors-count points point)
  (set-count (set-intersect points (neighbors point))))

; GUI
(define frame (new frame%
                   [label "Conway's Game of Life"]
                   [width 500]
                   [height 500]))

(send frame show #t)

(define canvas (new canvas% [parent frame]))

(define (draw-points points dc)
  (for ([point points])
    (let ([x (car point)]
          [y (cadr point)]
          [size 15])
      (send dc set-brush "black" 'solid)
      (send dc draw-rectangle (* x size) (* y size) size size))))

; Main loop
(define points (set '(2 2) '(2 3) '(2 4) '(3 3) '(3 4) '(3 5)))

(define timer (new timer%
                   [interval 1000]
                   [notify-callback
                    (lambda ()
                      (send canvas refresh-now (curry draw-points points))
                      (set! points (next-step points)))]))

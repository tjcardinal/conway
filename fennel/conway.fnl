(fn union [a b]
  (collect [k v (pairs a) &into b]
    k
    v))

(fn neighbors [cell]
  (let [{: x : y} cell]
    {(.. (+ x 1) "-" (+ y 1)) {:x (+ x 1) :y (+ y 1)}
     (.. (+ x 1) "-" y) {:x (+ x 1) : y}
     (.. (+ x 1) "-" (- y 1)) {:x (+ x 1) :y (- y 1)}
     (.. x "-" (+ y 1)) {: x :y (+ y 1)}
     (.. x "-" (- y 1)) {: x :y (- y 1)}
     (.. (- x 1) "-" (+ y 1)) {:x (- x 1) :y (+ y 1)}
     (.. (- x 1) "-" y) {:x (- x 1) : y}
     (.. (- x 1) "-" (- y 1)) {:x (- x 1) :y (- y 1)}}))

(fn alive? [cell cells]
  (let [neighbors (neighbors cell)
        count (accumulate [sum 0 k _ (pairs neighbors)]
                (if (not= nil (. cells k)) (+ sum 1) sum))]
    (case count
      3 true
      (where 2 (not= nil (. cells (.. cell.x "-" cell.y)))) true
      _ false)))

(fn next [cells]
  (let [neighbors (accumulate [ns {} _ v (pairs cells)]
                    (union ns (neighbors v)))
        to-check (union cells neighbors)]
    (collect [k v (pairs to-check)]
      (if (alive? v cells) (values k v)))))

(var dtotal 0)

(fn love.update [dt]
  (set dtotal (+ dtotal dt))
  (if (and (< 0.5 dtotal) (not paused))
      (do
        (set dtotal 0)
        (set cells (next cells)))))

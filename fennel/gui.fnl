(fn love.draw []
  (each [_ {: x : y} (pairs cells)]
    (love.graphics.rectangle :fill (* x 10) (* y 10) 10 10)))

(fn love.keypressed [key]
  (case key
    :escape (love.event.quit)
    :space (set paused (not paused))))

(fn love.mousepressed [x y]
  (let [x (math.floor (/ x 10))
        y (math.floor (/ y 10))
        k (.. x "-" y)
        current-alive? (not= nil (. cells k))]
    (case current-alive?
      true (tset cells k nil)
      false (tset cells k {: x : y}))))

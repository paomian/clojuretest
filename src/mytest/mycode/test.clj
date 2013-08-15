(defn empty-board
    [w h]
    (vec (repeat w (repeat h nil))))

(defn populate
    [board living-cells]
    (reduce (fn [board coordinates]
                          (assoc-in board coordinates :on))
                      board
                      living-cells))
(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))
(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dy x) (+ dy y)]))
(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))
(defn indexed-step
  [borad]
  (let [w (count board)
        h (count (first board))]
    (loop [new-borad board x 0 y 0]
      (cond
        (>= x w) new-borad
        (>= y h) (recur new-borad (inc x) 0)
        :else
          (let [new-liveness
                (caes (count-neighbours board [x y])
                      2 (get-in board [x y])
                      3 :on
                      nil)]
            (recur (assoc-in new-borad [x y] new-liveness) s (inc y)))))))
(defn indexed-step2
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce (fn [new-borad x]
              (reduce
                (fn [new-liveness
                       (case (count-neighbours board [x y])
                         2 (get-in board [x y])
                         3 :on
                         nil)]
                  (assoc-in new-borad [x y] new-liveness)))
              new-borad (range h)))
    board (range w)))
(defn indexed-step3 
  [board]
  (let [w (count board)
         h (count (first board))]
    (reduce
      (fn [new-borad [x y]]
        (let [new-liveness
                (case (count-neighbours board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (assoc-in new-borad [x y] new-liveness)))
        board (for [x (range h) y (range w) [x y]]))))
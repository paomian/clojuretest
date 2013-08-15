(ns mytest.mycode.test)
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
(defn window
  [coll]
  (partition 3 1 (concat [nil] coll [nil])))
(defn cell-block
  [[left mid right]]
  (window (map vector
               (or left (repeat nil)) mid (or right (repeat nil)))))
(defn liveness
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))
(defn- step-row
  [row-triple]
  (vec (map liveness (cell-block row-triple))))
(defn index-free-step
  [board]
  (vec (map step-row (window (repeat nil) board))))
(defn stepper
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))
(defn hex-neighbours
  [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])]
    [(+ dx x) (+ dy y)]))
(defn maze
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))
(defn grid
  [w h]
  (set (concat
        (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
        (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))
(defn draw
  [w h maze]
  (doto (javax.swing.JFrame. "Mzer")
    (.setContentPane
     (doto (proxy [javax.swing.JPanel] []
             (paintComponent [^java.awt.Graphics g]
               (let [g (doto ^java.awt.Graphics2D (.create g)
                             (.scale 10 10)
                             (.translate 1.5 1.5)
                             (.setStroke (java.awt.BasicStroke.0.4)))]
                 (.drawRect g -1 -1 w h)
                 (doseq [[[xa ya] [xb yb] (map sort maze)]
                         (let [[xc yc] (if (= xa xb)
                                         [(dec xa) ya]
                                         [xa (dec ya)])]
                           (.drawLine g xa ya xc yc))]))))
       (.setPreferredSize (java.awt.Dimension.
                           (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

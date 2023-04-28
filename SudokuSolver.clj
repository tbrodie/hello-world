(ns sudoku_solver.core)

(import java.io.InputStreamReader)

(defn parse-char [c]
  (if (= c \.)
    nil
    (Character/getNumericValue c)))


(defn input []
  (println "Please enter your puzzle in one line: Ex. 82..9.3.4...etc \n")
  ( name (read-line)))


(defn myboard []
  (->> (partition 9 (seq(input)))
               (mapv #(mapv parse-char %))))

(def board (myboard))


(defn valid-row? [board row val]
  (not-any? (fn [x] (= val x)) (get board row)))

(defn valid-col? [board col val]
  (not-any? (fn [x] (= val x)) (map #(get % col) board)))

(defn valid-box? [board row col val]
  (let [box-size 3
        box-row  (quot row box-size)
        box-col  (quot col box-size)
        row-start (* box-size box-row)
        col-start (* box-size box-col)]
    (not-any? (fn [x] (= val x))
              (for [i (range box-size)
                    j (range box-size)
                    :let [row (+ row-start i)
                          col (+ col-start j)]
                    :when (and (<= 0 row 8)
                               (<= 0 col 8)
                               (not= nil? [row col] [row col]))]
                (get-in board [row col])))))

(defn valid-move? [board row col val]
    (let [is-valid (and (valid-row? board row val)
                      (valid-col? board col val)
                      (valid-box? board row col val))]
    is-valid))

(def coords (doall (for [i (range 9)
                         j (range 9)]
                     [i j])))

(defn find-empty-cells [board]
  (let [empty-cells (->> coords
                         (filter #(nil? (get-in board %))))]
    empty-cells))

(defn find-cells-with-one [board]
  (let [empty-cells (find-empty-cells board)]
    (filter (fn [cell]
              (let [possible-values (filter #(valid-move? board (first cell) (second cell) %) (range 1 10))]
                (= 1 (count possible-values))))
            empty-cells)))


(defn cells-with-sols [board]
  (let [empty-cells (find-empty-cells board)]
    (->> empty-cells
         (filter (fn [cell]
                   (let [possible-values (filter #(valid-move? board (first cell) (second cell) %) (range 1 10))]
                     (= 1 (count possible-values)))))
         (mapv (fn [cell]
                 [:cell cell
                  :values (->> (range 1 10)
                               (filter #(valid-move? board (first cell) (second cell) %))
                               first)])))))


(defn update-board [board [row col] val]
    (assoc-in board [row col] val))

(defn solve [board]
  (if (empty? (cells-with-sols board))
    board
    (let [updated-board (update-board board (first(map second ( cells-with-sols board))) (first(map last ( cells-with-sols board))))]
      (solve updated-board))))


(defn print-board [board]
  (doseq [row (range 9)]
    (when (= (mod row 3) 0) (println "   - - -   - - -   - - - "))
    (doseq [col (range 9)]
      (when (= (mod col 3) 0) (print " |"))
      (print " ")
      (print (get-in board [row col]))
      (when (= col 8) (print " |")))
    (when (< row 9) (println ""))))


(defn -main []
  (println "\nUnsolved puzzle:")
   board
  (print-board board)
  (println "\nSolved puzzle:")
  (print-board (solve board)))


(ns sicp.chap3)

;; 3.1
(defn make-accumulator
  [init-val]
  (let [current-val (atom init-val)]
    (fn [x] (reset! current-val (+ @current-val x)))
    )
  )

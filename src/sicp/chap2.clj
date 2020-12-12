(ns sicp.chap2
  (:require [sicp.chap1])
  )

(sicp.chap1/square 2)

;; 2.1
(defn make-rat
  [n d]
  (let [
        g (sicp.chap1/gcd d n)
        sign (if (< (* n d) 0) -1 1)
        ]
  (conj nil (sicp.chap1/abs (/ d g)) (* sign (/ n g)))
  ))

;; 2.2

(defn make-point [x y] (conj nil y x))
(defn x-point [p] (first p))
(defn y-point [p] (nth p 1))


(defn make-segment [p q] (conj nil q p))
(defn start-segment [seg] (first seg))
(defn end-segment [seg] (nth seg 1))

(defn midpoint-segment
  [seg]
  (let [
        p (start-segment seg)
        q (end-segment seg)
        x-mid (sicp.chap1/average (x-point p) (x-point q))
        y-mid (sicp.chap1/average (y-point p) (y-point q))
        ]
    (make-point x-mid y-mid)
  ))

(defn print-point
  [p]
  newline
  (println (format "(%d, %d)" (x-point p) (y-point p)))
  )

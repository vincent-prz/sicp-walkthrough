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
  (list (* sign (/ n g)) (sicp.chap1/abs (/ d g)))
  ))

;; 2.2

(defn make-point [x y] (list x y))
(defn x-point [p] (first p))
(defn y-point [p] (nth p 1))


(defn make-segment [p q] (list p q))
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

;; 2.3

;; first representation: one segment + one point
;; we assume the point is the point adjacent to the start of the segment
(defn make-rect [seg p] (list seg p))
(defn seg-rect [rect] (first rect))
(defn point-rect [rect] (nth rect 1))

(defn distance
  [p q]
  (let [
        xp (x-point p)
        yp (y-point p)
        xq (x-point q)
        yq (y-point q)
        ]
    (sicp.chap1/sqrt2 (+ (sicp.chap1/square (- xp xq)) (sicp.chap1/square (- xq yq))))
    )
  )

(defn length-seg [seg] (distance (start-segment seg) (end-segment seg)))

(defn dimensions-rect
  [rect]
  (let [
        seg (seg-rect rect)
        point (point-rect rect)
        ]
    (list (length-seg seg) (distance (start-segment seg) point)))
  )

(defn perimeter [rect]
  (rect)
  (let [
        dims (dimensions-rect rect)
        h (first dims)
        w (nth dims 1)
        ]
    (* 2 (+ h w))
    ))

(defn area [rect]
  (rect)
  (let [
        dims (dimensions-rect rect)
        h (first dims)
        w (nth dims 1)
        ]
    (* h w)
    ))

;; abstraction bareers:
;; --- programs that use rectangles ---
;; --- perimeter, area ---
;; --- dimensions-rect ---
;; --- make-rect, seg-rect, point-rect ---
;; --- list, first, nth ---

;; the dimensions-rect bareer allows us to see a rectangle as its diemensions
;; and would allow us to have perimter and area procedures which work independently
;; of the underlying rect implementation
;; (ex of alternative implementation: 3 points)

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

;; 2.4

(defn cdr [z] (z (fn [p q] q)))

;; 2.5
;; it works because of unicity of prime decomposition

(defn make-pair
  [a b]
  (int (* (Math/pow 2 a) (Math/pow 3 b)))
  )

(defn extract-prime-factor
  [n p]
  (defn iter
   [q result]
   (if (= (mod q p) 0)
     (iter (/ q p) (inc result))
     result
     ))
  (iter n 0)
  )

(defn two-pair [p] (extract-prime-factor p 2))
(defn three-pair [p] (extract-prime-factor p 3))

;; 2.6

(def church-zero (fn [f] (fn [x] x)))

(defn church-add1 [n]
  (fn [f] (fn [x] (f (n f) x)))
  )

(def church-one (church-add1 church-zero))
(def church-two (church-add1 church-one))
(defn church-add [m n]
  (fn [f]
    (fn [x] (m f ((n f) x))
      )
    )
  )

;; 2.7

(defn make-interval [a b] (list a b))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (nth x 1))

;; 2.8

(defn sub-interval [x y]
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))
   ))

;; 2.9

;; we can prove:
;; if i = i1 + i2 then w = w1 + w2
;; if i = i1 - i2 then w = w1 + w2
;; counter example for multiplication:
;; [0, 1] * [0, 1] = [0, 1] (w1 = 1, w2 = 1 and w = 1)
;; [1, 2] * [1, 2] = [1, 4] (w1 = 1, w2 = 1 and w = 3)
;; so the width of the multiplication is not a function of widths of the operands
;; dvision being a form of multiplication, the same can be said of division.

;; 2.11

(defn mul-interval
  [p q]
   (let [
         x1 (lower-bound p)
         x2 (upper-bound p)
         y1 (lower-bound q)
         y2 (upper-bound q)
         ]
     (cond
       (and (>= x1 0) (>= x2 0) (>= y1 0) (>= y2 0))
              (make-interval (* x1 y1) (* x2 y2))
       (and (< x1 0) (>= x2 0) (>= y1 0) (>= y2 0))
            (make-interval (* x1 y2) (* x2 y2))
       (and (< x1 0) (< x2 0) (>= y1 0) (>= y2 0))
            (make-interval (* x1 y2) (* x2 y1))
       (and (>= x1 0) (>= x2 0) (< y1 0) (>= y2 0))
            (make-interval (* x2 y1) (* x2 y2))
       (and (< x1 0) (>= x2 0) (< y1 0) (>= y2 0))
            (make-interval
             (min (* x1 y2) (* x2 y1))
             (max (* x1 y1) (* x2 y2))
             )
       (and (< x1 0) (< x2 0) (< y1 0) (>= y2 0))
            (make-interval (* x1 y2) (* x1 y1))
       (and (>= x1 0) (>= x2 0) (< y1 0) (< y2 0))
            (make-interval (* x2 y1) (* x1 y2))
       (and (< x1 0) (>= x2 0) (< y1 0) (< y2 0))
            (make-interval (* x2 y1) (* x1 y1))
       (and (< x1 0) (< x2 0) (< y1 0) (< y2 0))
            (make-interval (* x2 y2) (* x1 y1))
     )
   ))

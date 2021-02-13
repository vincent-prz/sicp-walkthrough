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

;; 2.12

(defn make-center-percent
  [c p]
  (let [w (* p c)]
    (make-interval (- c w) (+ c w))
    )
  )

(defn center
  [i]
  (sicp.chap1/average (lower-bound i) (upper-bound i))
  )

(defn percent
  [i]
  (let [
        c (center i)
        w (- (upper-bound i) c)
        ]
    (/ w c)
    )
  )

;; 2.13

;; we assume what bounds are positive.
;; lower-product = (c1 - w1)(c2 - w2) ~= c1c2 - (w1c2 + w2c1)
;; here we have neglected the term w1w2. Similarly for the upper bound:
;; upper-product = (c1 + w1)(c2 + w2) ~= c1c2 + (w1c2 + w2c1)
;; it results that:
;; center-product = c1c2
;; w-product = w1c2 + w2c1
;; percentage-product = (w1c2 + w2c1) / c1c2 = w1 / c1 + w2 / c2
;;   = percentage-1 + percentage-2

;; 2.17

(defn last-pair
  [l]
  (if (= (count l) 1)
    (first l)
    (last-pair (rest l)))
  )

;; 2.18

(defn reverse-pair
  [l]
  (if (empty? l)
    nil
    (concat (reverse-pair (rest l)) (list (first l)))
    )
  )

;; 2.19


(defn first-denomination
  [coins]
  (first coins)
  )

(defn except-first-denomination
  [coins]
  (rest coins)
  )

(defn no-more?
  [coins]
  (empty? coins)
  )

(defn cc
  [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+
               (cc amount (except-first-denomination coin-values))
               (cc (- amount (first-denomination coin-values)) coin-values)
               )
        )
  )

(def uscoins (list 50 25 10 5 1))
(def ukcoins (list 100 50 20 10 5 2 1 0.5))

;; the algorithm is independent of the order of appearance of the coins

;; 2.20

;; would be much less clunky with a filter
(defn same-parity
  [x & more]
  (defn rec-aux
    [l]
    (cond (empty? l) nil
          (= (mod (first l) 2) (mod x 2)) (conj (rec-aux (rest l)) (first l))
          :else (rec-aux (rest l))
    ))
  (rec-aux more)
  )

;; 2.21

(defn square-list-clunky
  [l]
  (if (empty? l)
    nil
    (conj (square-list-clunky (rest l)) (sicp.chap1/square (first l)))
    )
  )

(defn square-list [l] (map sicp.chap1/square l))

;; 2.22

;; the first answer gives a list in reverse order because the `answer`
;; progressively being built accumulates the elemnts in reverse order

;; the second answer is wrong because the second argument must be
;; a list of elements with the same type as the first argument.
;; here the first argument is a list of int, and the second argument
;; is an int, this is a type mismatch.

;; 2.23

;; tried to reuse map but it didn't work
(defn for-each
  [f l]
  (if (empty? l)
    true
    (let [h (first l) t (rest l)]
      (f h)
      (for-each f t)
      )
  ))

;; 2.27

(defn reverse-deep
  [l]
  (cond
    (not (list? l)) l
    (empty? l) nil
    :else (let [h (first l) t (rest l)]
      (concat (reverse-deep t) (list (reverse-deep h)))
      )
    )
  )

;; 2.28

(defn fringe
  [tree]
  (cond
    (not (list? tree)) (list tree)
    (empty? tree) nil
    :else (let [h (first tree) t (rest tree)]
      (concat (fringe h) (fringe t))
      )
    )
  )

;; 2.29

(defn make-mobile
  [left right]
  (list left right)
  )

(defn make-branch
  [length structure]
  (list length structure)
  )

(defn left-branch [mob] (first mob))
(defn right-branch [mob] (nth mob 1))

(defn branch-length [br] (first br))
(defn branch-structure [br] (nth br 1))

(defn total-weight
  [mob]
  (defn branch-weight
    [br]
    (let [length (branch-length br) structure (branch-structure br)]
      (if (list? structure)
        (* length (total-weight structure))
        (* length structure)
        )
    )
  )
  (+ (branch-weight (left-branch mob)) (branch-weight (right-branch mob)))
  )

;; not cool: duplicating branch-weight subfunction
;; it would be nice to be able to define mutually recursive functions
(defn is-balanced
  [mob]
  (defn branch-weight
    [br]
    (let [length (branch-length br) structure (branch-structure br)]
      (if (list? structure)
        (* length (total-weight structure))
        (* length structure)
        )
      )
    )
  (= (branch-weight (left-branch mob)) (branch-weight (right-branch mob)))
  )

;; d)
;; we need to change the selectors this way:
;; (defn right-branch [mob] (first (nth mob 1)))
;; (defn branch-structure [br] (first (nth br 1)))
;; the other selectors remain the same
;; Question: not sure how the new defiition of branch works if structure is a number ?

;; 2.30
(defn square-tree-clunky
  [tree]
  (cond
    (not (list? tree)) (sicp.chap1/square tree)
    (empty? tree) tree
    :else (conj (square-tree-clunky (rest tree)) (square-tree-clunky (first tree)))
    )
  )

(defn square-tree
  [tree]
  (if
      (not (list? tree)) (sicp.chap1/square tree)
      (map square-tree tree)
      )
  )

;; 2.31
(defn tree-map
  [f tree]
  (if
      (not (list? tree)) (f tree)
      (map (fn [t] (tree-map f t)) tree)
      )
  )

;; 2.32

(defn subsets
  [s]
  (if (empty? s)
    (list (list))
    (let [rst (subsets(rest s))]
      (concat rst
              (map (fn [x] (conj x (first s))) rst)
              )
      )
    )
  )

;; explanation: given a set l:
;; subset(l union {x}) = susbets(l) + {l'union{x}, for l' in susbsets(l)}

;; 2.33

(defn my-map
  [p sequence]
  (reduce
   (fn [accum, current] (concat accum (list (p current))))
   (list)
   sequence)
  )

(defn my-append
  [p1 p2]
  (reduce conj p2 (reverse p1))
  )

(defn my-length
  [sequence]
  (reduce (fn [accum, current] (inc accum)) 0 sequence)
  )

;; 2.34

(defn horner-eval
  [x coeffs]
  (reduce
   (fn [accum, c] (+ c (* x accum)))
   0
   (reverse coeffs)
   )
  )

;; 2.35

(defn count-leaves
  [tree]
  (reduce
   +
   0
   (map
    (fn [subtree]
      (cond
        (not (list? subtree)) 1
        (empty? subtree) 0
        :else (count-leaves subtree)
      ))
    tree
   )
  ))

;; 2.36

(defn accumulate-n
  [op init seqs]
  (if (empty? (first seqs))
    (list)
    (conj
     (accumulate-n op init (map rest seqs))
     (reduce op init (map first seqs))
     )
   )
  )

;; 2.37

(defn dot-product
  [v w]
  (reduce + 0 (map * v w))
  )

(defn matrix-*-vector
  [m v]
  (map (fn [row] (dot-product row v)) m)
  )

(defn conj-end
  [coll elem]
  (concat coll (list elem))
  )

(defn transpose
  [mat]
  (accumulate-n conj-end (list) mat)
  )

(defn matrix-*-matrix
  [m n]
  (let [cols (transpose n)]
    (map (fn [row] (matrix-*-vector cols row)) m))
  )

;; 2.38

;; I have been using `reduce` so far, which is actually the equivalent of `fold-left`
;; let's define `fold-right`

(defn fold-right
  [op init seq]
  (defn iter [result l]
    (if (empty? l)
      result
      (iter (op (first l) result) (rest l))
      )
  )
  (iter init seq)
  )

;; if op is commutative both folds will yield the same results

;; 2.39

(defn reverse-w-fold-left [seq] (reduce conj '() seq))

(defn reverse-w-fold-right
  [seq] (
         fold-right
         (fn [current accum] (concat (list current) accum))
         '()
         seq)
  )

;; 2.40
(defn enumerate-interval
  [n m]
  (if (> n m)
    '()
    (conj (enumerate-interval (inc n) m) n)
    )
  )


(defn flatmap
  [f seq]
  (reduce concat '() (map f seq))
  )

(defn unique-pairs
  [n]
  (flatmap
   (fn [i]
     (map (fn [j] (list j i)) (enumerate-interval 1 (dec i)))
     )
   (enumerate-interval 2 n)
   )
  )

(defn sum-seq [seq] (reduce + seq))

(defn prime-sum-pairs
  [n]
  (filter
   (fn [pair] (sicp.chap1/prime? (sum-seq pair)))
   (unique-pairs n)
   )
  )

;; 2.41
(defn unique-triples
  [n]
  (flatmap
   (fn [k]
     (map (fn [pair] (conj-end pair k)) (unique-pairs (dec k)))
     )
   (enumerate-interval 3 n)
   )
  )

(defn triple-sum
  [n s]
  (filter
   (fn [triple] (= (sum-seq triple) s))
   (unique-triples n)
   )
  )

;; 2.42

(def empty-board '())

(defn get-row [position] (first position))
(defn get-col [position] (nth position 1))

(defn adjoin-position
  [row col board]
  (conj board (list row col))
  )

(defn split-by-predicate
  [pred seq]
  (list (filter pred seq) (filter (fn [x] (not (pred x))) seq))
  )

(defn row-safe?
  [k board]
  "assumption: board is not empty"
  (let [
        split (split-by-predicate (fn [pos] (= (get-col pos) k)) board)
        new-pos (first (first split))
        remaining-pos (nth split 1)
        new-row (get-row new-pos)
        ]
    (empty? (filter (fn [position] (= (get-row position) new-row)) remaining-pos))
    )
  )

(defn same-diagonal?
  [pos1 pos2]
  (let [
        row1 (get-row pos1)
        col1 (get-col pos1)
        row2 (get-row pos2)
        col2 (get-col pos2)
        ]
    (= (sicp.chap1/abs (- row1 row2)) (sicp.chap1/abs (- col1 col2)))
    )
  )

(defn diagonal-safe?
  [k board]
  "assumption: board is not empty"
  (let [
        split (split-by-predicate (fn [pos] (= (get-col pos) k)) board)
        new-pos (first (first split))
        remaining-pos (nth split 1)
        ]
    (empty? (filter (fn [position] (same-diagonal? position new-pos)) remaining-pos))
    )
  )

(defn safe?
  [k board]
  (and (row-safe? k board) (diagonal-safe? k board))
  )

(defn queens
  [board-size]
  (defn queen-cols
    [k]
    (if (= k 0)
      (list empty-board)
      (filter
       (fn [positions] (safe? k positions))
       (flatmap
        (fn [rest-queens]
          (map
           (fn [new-row] (adjoin-position new-row k rest-queens))
           (enumerate-interval 1 board-size)
           )
          )
        (queen-cols (dec k))
        )
       )
      )
    )
  (queen-cols board-size)
  )

;; 2.43
(defn queens-slow
  [board-size]
  (defn queen-cols
    [k]
    (if (= k 0)
      (list empty-board)
      (filter
       (fn [positions] (safe? k positions))
       (flatmap
        (fn [new-row]
          (map
           (fn [rest-queens] (adjoin-position new-row k rest-queens))
           (queen-cols (dec k))
           )
          )
        (enumerate-interval 1 board-size)
        )
       )
      )
    )
  (queen-cols board-size)
  )

;; with this version of the algorithm, we are for each column k, computing
;; board-size times the safe boards for the (k - 1) previous columns, which
;; is a waste.
;; The complexity of this algoritm satisfies the following relationship:
;; T[k] = N*T[k - 1], so the complexity grows exponentially with k.

;; 2.44

(defn below
  [painter]
  "primitive not yet implemented"
  nil
  )

(defn beside
  [painter]
  "primitive not yet implemented"
  nil
  )

(defn up-split
  [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller))
      )
    )
  )

;; 2.45

(defn split
  [outer-op inner-op]
  (defn rec [painter n]
    (if (= n 0)
      painter
      (let [smaller (rec painter (dec n))]
        (outer-op painter (inner-op smaller smaller))
        )
      )
    )
  rec
  )

;; 2.46

(defn make-vect [x y] (list x y))
(defn xcor-vect [v] (first v))
(defn ycor-vect [v] (nth v 1))

(defn add-vect
  [v w]
  (make-vect
   (+ (xcor-vect v) (xcor-vect w))
   (+ (ycor-vect v) (ycor-vect w))
   )
  )

(defn sub-vect
  [v w]
  (make-vect
   (- (xcor-vect v) (xcor-vect w))
   (- (ycor-vect v) (ycor-vect w))
   )
  )

(defn scale-vect
  [s v]
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))
   )
  )

;; 2.47

(defn make-frame
  [origin edge1 edge2]
  (list origin edge1 edge2)
  )

(defn origin
  [frame]
  (first frame)
  )

(defn edge1
  [frame]
  (nth frame 1)
  )

(defn edge2
  [frame]
  (nth frame 2)
  )

;; NOTE: I'm confused with the alternative definition given in the book,
;; shouldn't it be this way ? (cons edge1 edge2) should crash ?
;; if constructor should indeed be this way, selectors are the same.
(defn make-frame-alt
  [origin edge1 edge2]
  (cons origin (cons edge1 (cons edge2 nil)))
  )

;; 2.48

(defn make-segment [v w] (list v w))
(defn start-segment [seg] (first seg))
(defn end-segment [seg] (nth seg 1))

;; 2.49

(defn draw-line
  [vec]
  "primitive not yet implemented"
  nil
  )

(defn frame-coord-map
  [frame]
  (fn [vec]
    (let [x (xcor-vect vec) y (ycor-vect vec)]
      (add-vect
       (origin frame)
       (scale-vect x (edge1 frame))
       (scale-vect y (edge2 frame))
       )
      )
    )
  )

(defn segments->painter
  [segments-list]
  (fn [frame]
    (for-each
     (fn [segment]
       (draw-line (frame-coord-map frame) (start-segment segment))
       (draw-line (frame-coord-map frame) (end-segment segment))
       )
     segments-list
     )
    )
  )

(defn outline-painter []
  (segments->painter
   (let [
         a (make-vect 0 0)
         b (make-vect 1 0)
         c (make-vect 1 1)
         d (make-vect 0 1)
         ]
     (segments->painter
      (list
       (make-segment a b)
       (make-segment b c)
       (make-segment c d)
       (make-segment d a)
       ))
    )
   )
  )

(defn x-painter []
  (segments->painter
   (let [
         a (make-vect 0 0)
         b (make-vect 1 0)
         c (make-vect 1 1)
         d (make-vect 0 1)
         ]
     (segments->painter
      (list
       (make-segment a c)
       (make-segment b d)
       ))
     )
   )
  )

(defn diamond-painter []
  (segments->painter
   (let [
         a (make-vect 1/2 0)
         b (make-vect 1 1/2)
         c (make-vect 1/2 1)
         d (make-vect 0 1/2)
         ]
     (segments->painter
      (list
       (make-segment a b)
       (make-segment b c)
       (make-segment c d)
       (make-segment d a)
       ))
     )
   )
  )

(defn wave []
  "we start from the bottom of the left foot"
  (segments->painter
   (let [
         a (make-vect 2/3 0)
         b (make-vect 2/5 0)
         c (make-vect 1/2 1/3)
         d (make-vect 3/5 0)
         e (make-vect 2/3 0)
         f (make-vect 19/30 1/2)
         g (make-vect 1 2/15)
         h (make-vect 1 4/15)
         i (make-vect 7/9 3/5)
         j (make-vect 19/30 3/5)
         k (make-vect 7/10 5/6)
         l (make-vect 19/30 1)
         m (make-vect 11/30 1)
         n (make-vect 3/10 5/6)
         o (make-vect 11/30 3/5)
         p (make-vect 1/3 3/5)
         q (make-vect 1/5 8/15)
         r (make-vect 0 5/6)
         s (make-vect 0 3/5)
         t (make-vect 1/5 9/20)
         u (make-vect 1/3 8/15)
         v (make-vect 19/30 11/20)
         ]
     (segments->painter
      (list
       (make-segment b c)
       (make-segment c d)
       (make-segment e f)
       (make-segment f g)
       (make-segment h i)
       (make-segment i j)
       (make-segment j k)
       (make-segment k l)
       (make-segment m n)
       (make-segment n o)
       (make-segment o p)
       (make-segment p q)
       (make-segment q r)
       (make-segment s t)
       (make-segment t u)
       (make-segment u v)
       (make-segment v a)
       ))
     )
   )
  )

;; 2.50
(defn transform-painter
  [painter origin corner1 corner2]
  (fn [frame]
    (let [
          m (frame-coord-map frame)
          new-origin (m origin)
          ]
      (painter (
                make-frame
                new-origin
                sub-vect (m corner1) new-origin
                sub-vect (m corner2) new-origin
                )
               )
      )
    )
  )

(defn flip-horiz
  [painter]
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)
   )
  )

(defn rotate90-painter
  [painter]
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 1 1)
   (make-vect 0 0)
   )
  )

(defn rotate180-painter
  [painter]
  (rotate90-painter (rotate90-painter painter))
  )

(defn rotate270-painter
  [painter]
  (rotate90-painter (rotate180-painter painter))
  )

;; 2.51

(defn below
  [painter1 painter2]
  (let [
        paint-bottom
        (transform-painter
         painter1 (make-vect 0 0) (make-vect 1 0) (make-vect 0 0.5))
        paint-up
        (transform-painter
         painter2 (make-vect 0 0.5) (make-vect 1 0.5) (make-vect 0 1))
        ]
    (fn [frame]
      (do (painter1 frame) (painter2 frame))
      )
    )
  )

(defn below-v2
  [painter1 painter2]
  (rotate270-painter
   (beside (rotate90-painter painter1) (rotate90-painter painter2))
   )
  )

;; 2.52: skipping for now

;; 2.53

(defn memq
  [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))
        )
  )

;; 2.54

(defn equal?
  [a b]
  (cond (empty? a) (empty? b)
        (empty? b) (empty? a)
        :else (and (= (first a) (first b)) (equal? (rest a) (rest b)))
        )
  )

;; 2.55

;; (first ''abracadabra) is equivalent to:
;; -> (first (quote (quote abracadabra)))
;; -> (first '(quote abracadabra))
;; and (first 'l) where l is an expression of the form (x y ...) yields x

;; 2.56, 2.57

(defn variable? [e] (symbol? e))

(defn same-variable?
  [e f]
  (and (variable? e) (variable? f) (= e f))
  )

(defn make-sum
  [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (= 0 a) b
        (= 0 b) a
        :else (list '+ a b)
        )
  )

(defn sum?
  [e]
  (= (first e) '+)
  )

(defn addend
  [e]
  (nth e 1)
  )

(defn augend
  [e]
  (cond (= (count e) 3) (nth e 2)
        :else (conj (rest (rest e)) '+)
        )
  )

(defn make-product
  [a b]
  (cond (and (number? a) (number? b)) (* a b)
        (= 0 a) 0
        (= 0 b) 0
        (= 1 a) b
        (= 1 b) a
        :else (list '* a b))
  )

(defn product?
  [e]
  (= (first e) '*)
  )

(defn multiplier
  [e]
  (nth e 1)
  )

(defn multiplicand
  [e]
  (cond (= (count e) 3) (nth e 2)
        :else (conj (rest (rest e)) '*)
        )
  )

(defn make-exponentiation
  [a b]
  (cond (= 0 b) 1
        (= 0 a) 0
        (= 1 b) a
        :else (list '** a b))
  )

(defn exponentiation?
  [e]
  (= (first e) '**)
  )

(defn base
  [e]
  (nth e 1)
  )

(defn exponent
  [e]
  (nth e 2)
  )

(defn deriv
  [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))
        (product? exp) (let [u (multiplier exp) v (multiplicand exp)]
                         (make-sum
                          (make-product (deriv u var) v)
                          (make-product u (deriv v var)))
                         )
        (exponentiation? exp) (let [u (base exp) n (exponent exp)]
                                (make-product
                                 (make-product n (make-exponentiation u (dec n)))
                                 (deriv u var)
                                 )
                                )
        :else (throw (Throwable.
                      (format "Error: unknown expression type -- DERIV %s" exp)))
        )
  )

;; 2.58
(defn make-sum2
  [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (= 0 a) b
        (= 0 b) a
        :else (list a '+ b)
        )
  )

(defn sum2?
  [e]
  (and (list? e) (> (count e) 2) (= (nth e 1) '+))
  )

(defn addend2
  [e]
  (first e)
  )

(defn augend2
  [e]
  (nth e 2)
  )

(defn make-product2
  [a b]
  (cond (and (number? a) (number? b)) (* a b)
        (= 0 a) 0
        (= 0 b) 0
        (= 1 a) b
        (= 1 b) a
        :else (list a '* b))
  )

(defn product2?
  [e]
  (and (list? e) (> (count e) 2) (= (nth e 1) '*))
  )

(defn multiplier2
  [e]
  (first e)
  )

(defn multiplicand2
  [e]
  (nth e 2)
  )


(defn deriv2
  [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum2? exp) (make-sum2 (deriv2 (addend2 exp) var) (deriv2 (augend2 exp) var))
        (product2? exp) (let [u (multiplier2 exp) v (multiplicand2 exp)]
                         (make-sum2
                          (make-product2 (deriv2 u var) v)
                          (make-product2 u (deriv2 v var)))
                         )
        :else (throw (Throwable.
                      (format "Error: unknown expression type -- DERIV2 %s" exp)))
        )
  )

;; 2.59

(defn elem-set?
  [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (elem-set? x (rest set))
        )
  )

(defn adjoin-set
  [x set]
  (if (elem-set? x set)
    set
    (conj set x)
    )
  )

(defn union-set
  [set1 set2]
  (if (empty? set1)
    set2
    (adjoin-set (first set1) (union-set (rest set1) set2))
    )
  )

;; 2.60
;; elem-set? -> same implementation -> O(n). However n will be larger, which can
;;   be an issue if sets contain lots of consecutive repetitions: '(x x x x x x x ...)
;; adjoin-set -> equal to conj -> O(1)
;; intersection-set -> same implementation -> O(n**2). However n will be larger
;; union-set -> same implementation -> but complexity = O(n) instead of O(n**2)
;;   indeed we use n times adjoin-set, which is now O(1)
;; we will prefer this representation if there are no hard memory requirements

;; 2.61
(defn adjoin-set
  [x set]
  (cond (empty? set) (list x)
        (< x (first set)) (conj set x)
        :else (conj (adjoin-set x (rest set)) (first set))
    )
  )

;; 2.62
(defn union-set
  [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        :else (let [
              x1 (first set1)
              r1 (rest set1)
              x2 (first set2)
              r2 (rest set2)
              ]
          (cond (< x1 x2) (conj (union-set r1 set2) x1)
                (> x1 x2) (conj (union-set set1 r2) x2)
                :else (conj (union-set r1 r2) x1)
            )
          )
        )
  )

;; 2.63
(defn make-tree [entry lb rb] (list entry lb rb))

(defn entry [tree] (first tree))
(defn left-branch [tree] (nth tree 1))
(defn right-branch [tree] (nth tree 2))

(defn tree->list1
  [tree]
  (if (empty? tree)
    '()
    (concat
     (tree->list1 (left-branch tree))
     (conj (tree->list1 (right-branch tree)) (entry tree))
     )
    )
  )

(defn tree->list2
  [tree]
  (defn copy-to-list
    [tree result-list]
    (if (empty? tree)
      result-list
      (copy-to-list
       (left-branch tree)
       (conj (copy-to-list (right-branch tree) result-list) (entry tree))
       )
      )
    )
  (copy-to-list tree '())
  )

;; questions:
;; complexity of tree-list1: T(n) = 2 * T(n/ 2) + c => O(n)
;; difference between the two: idk!

;; 2.64

(defn list->tree
  [elements]
  (defn partial-tree
    [elts n]
    (if (= n 0)
      (list '() elts)
      (let [
            left-size (quot (- n 1) 2)
            left-result (partial-tree elts left-size)
            left-tree (first left-result)
            non-left-elts (nth left-result 1)
            new-entry (first non-left-elts)
            right-size (- n (+ left-size 1))
            right-result (partial-tree (rest non-left-elts) right-size)
            right-tree (first right-result)
            remaining-elts (nth right-result 1)
            ]
        (list (make-tree new-entry left-tree right-tree) remaining-elts)
        )
      )
    )
  (first (partial-tree elements (count elements)))
  )

;; a) we proceed recursively as follows:
;; - build the left tree by taking the first half of elements,
;; - take the middle element as the new entry
;; - take the remaining elements and build the right sub tree

;; b) the complexity satsisfies the relationship: T(n) = 2 * T(n / 2) + c
;; so we have a linear complexity


;; 2.65

(defn union-sorted-lists
  [list1 list2]
  (cond (empty? list1) list2
        (empty? list2) list1
        (< (first list1) (first list2))
        (cons (first list1) (union-sorted-lists (rest list1) list2))
        :else (cons (first list2) (union-sorted-lists list1 (rest list2)))
        )
  )

(defn union-set-balanced
  [set1 set2]
  (let [
        list1 (tree->list1 set1)
        list2 (tree->list1 set2)
        full-list (union-sorted-lists list1 list2)
        ]
    (list->tree full-list)
    )
  )

(defn intersection-sorted-lists
  [list1 list2]
  (cond (empty? list1) '()
        (empty? list2) '()
        (= (first list1) (first list2))
        (cons (first list1) (intersection-sorted-lists (rest list1) (rest list2)))
        (< (first list1) (first list2))
        (intersection-sorted-lists (rest list1) list2)
        :else (intersection-sorted-lists list1 (rest list2))
        )
  )

(defn intersection-set-balanced
  [set1 set2]
  (let [
        list1 (tree->list1 set1)
        list2 (tree->list1 set2)
        full-list (intersection-sorted-lists list1 list2)
        ]
    (list->tree full-list)
    )
  )

;; 2.66

;; assumption: a record is represented a pair of (key, value)
(defn lookup
  [given-key set]
  (let [
        entry-record (entry set)
        entry-key (first entry-record)
        ]
    (cond (empty? set) false
          (= given-key entry-key) entry-record
          (< given-key entry-key) (lookup given-key (left-branch set))
          :else (lookup given-key (right-branch set))
          )
    )
  )

;; 2.67

(defn make-leaf
  [symbol weight]
  (list 'leaf symbol weight)
  )

(defn leaf? [obj] (= (first obj) 'leaf))

(defn leaf-symbol [leaf] (nth leaf 1))

(defn leaf-weight [leaf] (nth leaf 2))

(defn symbols
  [tree]
  (if (leaf? tree)
    (list (leaf-symbol tree))
    (nth tree 2)
    )
  )

(defn weight
  [tree]
  (if (leaf? tree)
    (leaf-weight tree)
    (nth tree 3)
    )
  )

(defn left-branch [tree] (first tree))

(defn right-branch [tree] (nth tree 1))

(defn make-code-tree
  [left right]
  (list
   left
   right
   (concat (symbols left) (symbols right))
   (+ (weight left) (weight right))
   )
  )

(defn decode-symbol
  [bits tree]
  (cond (leaf? tree) (list (leaf-symbol tree) bits)
        (empty? bits) (throw (Throwable. "ERROR  NOT ENOUGH BITs"))
        (= (first bits) 0) (decode-symbol (rest bits) (left-branch tree))
        (= (first bits) 1) (decode-symbol (rest bits) (right-branch tree))
        :else (throw (Throwable. (format "BAD BIT: %s" (first bits))))
      )
  )

(defn decode
  [bits tree]
  (if (empty? bits)
    '()
    (let [
          symbol-decoding-result (decode-symbol bits tree)
          s (first symbol-decoding-result)
          remaining-bits (nth symbol-decoding-result 1)
          ]
      (cons s (decode remaining-bits tree))
      )
    )
  )

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; 2.68
(defn encode-symbol
  [symbol tree]
  (cond (leaf? tree) '()
        (elem-set? symbol (symbols (left-branch tree)))
        (cons 0 (encode-symbol symbol (left-branch tree)))
        (elem-set? symbol (symbols (right-branch tree)))
        (cons 1 (encode-symbol symbol (right-branch tree)))
        :else (throw (Throwable. "ERROR- symbol not found in tree"))
    )
  )

(defn encode
  [message tree]
  (if (empty? message)
    '()
    (concat
     (encode-symbol (first message) tree)
     (encode (rest message) tree)
     )
    )
  )

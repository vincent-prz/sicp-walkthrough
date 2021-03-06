(ns sicp.chap1)

(defn gcd
  [a b]
  (if (= b 0)
      a (gcd b (mod a b))))

;; 1.20
;; normal order: 18 calls to mod
;; applicative order: 4 calls to mod
(defn divides? [d n] (= 0 (mod n d)))

(defn square [n] (* n n))

;; 1.23
(defn next-divisor
  [n]
  (if (= n 2) 3 (+ n 2))
  )

(defn find-divisor
  [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        ;; uncomment to get back to state before 1.23
        ;; :else (find-divisor n (+ test-divisor 1))
        :else (find-divisor n (next-divisor test-divisor))
        ))

(defn smallest-divisor
  [n]
  (find-divisor n 2))

(defn prime? [n] (= n (smallest-divisor n)))

(defn expmod
  [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (square (expmod base (/ exp 2) m)) m)
        :else (mod (* base (expmod base (- exp 1) m)) m)
        )
  )

(defn fermat-test
  [n]
  (defn try-it [a] (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1))))
  )

(defn fast-prime?
  [n times]
  (cond (= 0 times) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false
        )
  )

;; 1.22
(defn timed-prime-test
  [n]
  (defn display-prime
    [timestamp]
     (if (fast-prime? n 2)
       (println (format "%d is prime, time elapsed %d ns" n (- (System/nanoTime) timestamp))))
     )
  (display-prime (System/nanoTime))
  )

(defn search-for-primes
  [lo hi]
  (cond (> lo hi) nil
        (even? lo) (search-for-primes (+ lo 1) hi)
        :else (do (timed-prime-test lo) (search-for-primes (+ lo 2) hi))
  ))

;; 1.24: form 1000 to 1_000_000, expected ratio: 3; actual ~ 2

;; 1.26: at each step, expmod is called twice instead of once, negating the divide and conquer strategy

;; 1.27

(defn fermat-test-full
  [n]
  (defn try-it [a] (= (expmod a n n) a))
  (defn aux-rec [a] (if (= n a) true
                        (and (try-it a) (aux-rec (+ a 1)))
                        ))
  (aux-rec 1)
  )

;; 1.28
(defn expmod'
  [base exp m]
  (cond (= exp 0) 1
        (even? exp) (let [x (expmod base (/ exp 2) m) y (mod (square x) m)]
                      (if (and (= y 1) (not= (mod x m) 1) (not= (mod x m) (- m 1))) 0 y))
        :else (mod (* base (expmod base (- exp 1) m)) m)
        )
  )

(defn miller-rabin-test
  [n]
  (defn try-it [a] (= (expmod' a (- n 1) n) 1))
  (try-it (+ 1 (rand-int (- n 1))))
  )

;; 1.29

(defn cube [x] (* x x x))

(defn sum
  [term a next-val b]
  (if (> a b)
    0
    (+ (term a) (sum term (next-val a) next-val b))
    )
  )

(defn simpson
  [f a b n]
  (def h (/ (- b a) n))
  (defn term [k]
    (let [y (f (+ a (* k h)))]
      (cond (= k 0) y
            (= k n) y
            (even? k) (* 2 y)
            :else (* 4 y)
          )))
  (* (/ h 3) (sum term 0 inc n))
  )

;; 1.30


(defn sum-iter
  [term a next-val b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-val a) (+ (term a) result))
    ))
  (iter a 0)
  )

;; 1.31

(defn product
  [factor a next-val b]
  (if (> a b)
    1
    (* (factor a) (product factor (next-val a) next-val b))
    )
  )

(defn factorial [n] (product identity 1 inc n))

(defn pi-product [n]
  (defn pi-factor [k]
    (if (even? k)
      (/ (+ k 2) (+ k 1))
      (/ (+ k 1) (+ k 2))
      )
  )
  (* 4 (product pi-factor 1 inc n))
  )

(defn product-iter
  [factor a next-val b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-val a) (* (factor a) result))
      ))
  (iter a 1)
  )

;; 1.32

(defn accumulate
  [combiner null-value term a next-val b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next-val a) next-val b))
    )
  )

(defn sum'
  [term a next-val b]
  (accumulate + 0 term a next-val b)
  )

(defn product'
  [factor a next-val b]
  (accumulate * 1 factor a next-val b)
  )

(defn accumulate-iter
  [combiner null-value term a next-val b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next-val a) (combiner (term a) result))
    ))
  (iter a null-value)
  )

;; 1.33

(defn filtered-accumulate
  [combiner null-value term a next-val b predicate]
  (if (> a b)
    null-value
    (if (predicate a)
      (combiner (term a) (filtered-accumulate combiner null-value term (next-val a) next-val b predicate))
      (filtered-accumulate combiner null-value term (next-val a) next-val b predicate)
      )
    )
  )

(defn sum-primes
  [a b]
  (filtered-accumulate + 0 identity a inc b prime?)
  )

(defn product-prime-with
  [n]
  (defn prime-with? [i] (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 2 inc n prime-with?)
  )

;; 1.34
;; (f f) evaluates to (2 2), and hences crashes

;; 1.35

(def tolerance 0.00001)

;; FIXME: using Math/abs doesn't work
(defn abs [x] (if (pos? x) x (- x)))

(defn fixed-point
  [f first-guess]
  (defn close-enough? [v1 v2] (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (try-it next-guess)
        )
     ))
  (try-it first-guess)
  )

(def golden-ratio (fixed-point (fn [x] (+ 1 (/ 1 x))) 1))

;; 1.36

(defn fixed-point-with-log
  [f first-guess]
  (defn close-enough? [v1 v2] (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [next-guess (f guess)]
      (println (float next-guess))
      (if (close-enough? guess next-guess)
        next-guess
        (try-it next-guess)
        )
      ))
  (try-it first-guess)
  )

(def sol
  (fixed-point-with-log
   (fn [x] (/ (Math/log 1000) (Math/log x)))
   1.2
   ))

;; TODO: use Math/average
(defn average [x y] (/ (+ x y) 2))

(def sol-with-average-damping
  (fixed-point-with-log
   (fn [x] (average x (/ (Math/log 1000) (Math/log x))))
   1.2
   ))

;; 1.37

(defn cont-frac
  [n d k]
  (defn rec
    [j]
    (if (< j k)
      (/ (n j) (+ (d j) (rec (inc j))))
      (/ (n k) (d k))
      ))
  (rec 1))

(defn cont-frac-iter
  [n d k]
  (defn iter
    [j result]
    (if (> j 0)
      (iter (dec j)
           (/ (n j) (+ (d j) result)))
      result
      ))
  (iter k 0))

(defn find-nb-steps-required
  []
  (defn golden-ratio-approx
    [k]
    (/ 1 (cont-frac (fn [x] 1) (fn [x] 1) k))
    )
  (defn try-it
    [k]
    (if (< (abs (- (golden-ratio-approx k) golden-ratio)) 0.0001)
      k
      (try-it (inc k))
      )
    )
  (try-it 1)
  )

;; 1.38

(defn e-approx
  [k]
  (defn denum
    [i]
    (if (= (mod i 3) 2)
      (* (/ (inc i) 3) 2)
      1))
  (+ (cont-frac-iter (fn [x] 1.0) denum k) 2))

;; 1.39

(defn tan-cf
  [x k]
  (cont-frac
   (fn [j] (if (= j 1) x (- (square x))))
   (fn [j] (- (* 2 j) 1))
   k))

;; 1.40

(defn deriv
  [f]
  (fn [x]
    (let [dx 0.00001]
      (/ (- (f (+ x dx)) (f x)) dx)
      )
    )
 )

(defn newton-transform
  [f]
  (fn [x]
    (- x (/ (f x) ((deriv f) x)))
    )
  )

(defn newtons-method
  [f guess]
  (fixed-point (newton-transform f) guess)
  )

(defn cubic
  [a b c]
  (fn [x]
    (+ (cube x) (* a (square x)) (* b x) c)
    )
  )

;; 1.41

(defn apply-double
  [f]
  (fn [x] (f (f x)))
  )

;; 1.42

(defn compose
  [f g]
  (fn [x] (f (g x)))
  )

;; 1.43

(defn repeated
  [f n]
  (if (= n 0)
    identity
    (compose f (repeated f (dec n)))
    )
  )

;; 1.44

(defn smooth
  [f]
  (fn [x]
    (let [dx 0.00001]
      (/ (+
          (f (- x dx))
          (f x)
          (f (+ x dx)))
         3
         )
      ))
  )

;; 1.45 TODO

;; 1.46
(defn iterative-improve
  [good-enough? improve-guess]
  (fn [initial-guess]
    (defn rec
      [guess]
      (if (good-enough? guess)
        guess
        (rec (improve-guess guess))

      )
    )
    (rec initial-guess)
  ))

(defn sqrt2
  [x]
  ((iterative-improve
    (fn [guess] (< (abs (- (square guess) x)) 0.001))
    (fn [guess] (average guess (/ x guess)))
  ) 1.0))

(defn fixed-point2
  [f first-guess]
  ((iterative-improve
    (fn [guess] (< (abs (- (f guess) guess)) tolerance))
    f
    ) first-guess))

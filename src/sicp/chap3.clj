(ns sicp.chap3)

;; 3.1
(defn make-accumulator
  [init-val]
  (let [current-val (atom init-val)]
    (fn [x] (reset! current-val (+ @current-val x)))
    )
  )

;; 3.2
(defn make-monitored
  [f]
  (let [counter (atom 0)]
    (fn
      [x]
      (cond (= x 'how-many-calls?) @counter
            (= x 'reset-count) (do (reset! counter 0) nil)
            :else (do (reset! counter (inc @counter)) (f x))
       ))
    )
  )

;; 3.3
(defn make-account
  [balance password]
  (let [current-balance (atom balance)]
    (defn withdraw
      [amount]
      (if (< @current-balance amount)
        "Insufficient funds"
        (reset! current-balance (- @current-balance amount))
        )
      )
    (defn deposit
      [amount]
      (reset! current-balance (+ @current-balance amount))
      )
    (defn check-password [p]
      (if (not (= p password)) (throw (Throwable. "Incorrect password"))
          ))
    (defn dispatch [m p]
      (check-password p)
      (cond (= m 'withdraw) withdraw
            (= m 'deposit) deposit
            :else (throw (Throwable. (format "Unknown request %s" m)))
            )
      )
    dispatch
    ))

;; 3.5

(defn monte-carlo
  [nb-trials experiment]
  (defn iter
    [trials-remaining trials-passed]
    (cond (= 0 trials-remaining) (/ trials-passed nb-trials)
          (experiment) (iter (dec trials-remaining) (inc trials-passed))
          :else (iter (dec trials-remaining) trials-passed)
      )
    )
  (iter nb-trials 0)
  )

(defn random-in-range
  [low high]
  (let [range (- high low)]
    (+ low (rand range))
    )
  )

(defn estimate-integral
  [predicate x1 x2 y1 y2 nb-trials]
  (defn integral-experiment
    []
    (let [x (random-in-range x1 x2) y (random-in-range y1 y2)]
      (predicate x y)
      )
    )
  (monte-carlo nb-trials integral-experiment)
  )

(defn in-unit-circle?
  [x y]
  (< (+ (* x x) (* y y)) 1.0)
  )

(def pi-estimate
  (* 4.0 (estimate-integral in-unit-circle? -1 1 -1 1 2000))
  )

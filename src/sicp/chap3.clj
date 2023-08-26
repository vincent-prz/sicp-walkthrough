(ns sicp.chap3)

;; 3.1
(defn make-accumulator
  [init-val]
  (let [current-val (atom init-val)]
    (fn [x] (do
      (reset! current-val (+ @current-val x));
      @current-val
      ))
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

;; 3.3, 3.4
(defn make-account
  [balance password]
  (let [current-balance (atom balance)
        nb-tries (atom 0)]
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
    (defn call-the-cops [] (throw (Throwable. "Calling the cops")))
    (defn check-password [p]
      (if (not (= p password)) (do
         (reset! nb-tries (inc @nb-tries))
          (if (>= @nb-tries 7) (call-the-cops) (throw (Throwable. "Incorrect password")))
          )
          (reset! nb-tries 0)
        ))
    (defn dispatch [p m]
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

;; 3.6

(defn make-rand [initial-seed]
  (defn rand-helper [seed]
    (repeatedly
      (let [gen (java.util.Random. seed)]
        (fn [] (.nextInt gen)))))
  (let [index (atom 0) random-values (atom (rand-helper initial-seed))]
    (defn generate []
      (let [result (nth @random-values @index)]
        (reset! index (inc @index))
        result
      ))
    (defn reset [seed]
      (reset! index 0)
      (reset! random-values (rand-helper seed))
      nil
      )
    (defn dispatch [m]
      (cond (= m 'generate) (generate)
            (= m 'reset) reset
            :else (throw (Throwable. (format "Unknown request %s" m)))
      )
    )
  dispatch
  )
)

;; 3.7

(defn make-account-v2
  [balance password]
  (let [current-balance (atom balance)
        nb-tries (atom 0)]
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
    (defn call-the-cops [] (throw (Throwable. "Calling the cops")))
    (defn join
      [joint-password] (fn [p m] (dispatch joint-password p m)))
    (defn dispatch [password p m]
      (defn check-password [p]
        (if (not (= p password)) (do
           (reset! nb-tries (inc @nb-tries))
            (if (>= @nb-tries 7) (call-the-cops) (throw (Throwable. "Incorrect password")))
            )
            (reset! nb-tries 0)
          ))
      (check-password p)
      (cond (= m 'withdraw) withdraw
            (= m 'deposit) deposit
            (= m 'join) join
            :else (throw (Throwable. (format "Unknown request %s" m)))
            )
      )
    )
    (fn [p m] (dispatch password p m))
  )

(defn make-joint
  [acc password new-password]
  ((acc password 'join) new-password)
  )


;; 3.8

(defn make-return-first-arg
  []
  (let [store (atom nil)]
    (fn [arg]
      (if (= @store nil) (reset! store arg))
    @store)
    ))

(def return-first-arg (make-return-first-arg))

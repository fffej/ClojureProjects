;;; Implementation of a simplified General Problem Solver
;;; http://en.wikipedia.org/wiki/General_Problem_Solver
;;; Based on code in "Paradigms of Artificial Intelligence Programming"

;; TODO There is a problem preserving order.

(ns uk.co.fatvat.gps2
  (:use [clojure.contrib.set])
  (:use [clojure.set]))

(defstruct operation :action :preconditions :add-list :del-list)

(def *ops* (ref nil))

(def *dbg-ids* (ref #{:gps}))

(defn dbg 
  [id format-string & args]
  "Print debugging info if (DEBUG id) has been specified"
  (when (contains? id @*dbg-ids*)
    (println (format format-string args))))

(defn debug
  [& ids]
  "Start dbg output on the given ids"
  (dosync
   (alter *dbg-ids* (fn [x] (set (union x ids))))))

(defn undebug
  [& ids]
  "Stop dbg output on the given ids"
  (dosync
   (alter *dbg-ids* (fn [x] (difference x ids)))))

(defn dbg-indent
  [id indent format-string & args]
  "Print indented debugging info if (DEBUG ID) has been specified"
  (when (@*dbg-ids* id)
    (println (format (str (apply str (repeat indent \space)) format-string) args))))

(defn contains-value?
  [coll val]
  (not (nil? (some (partial = val) coll))))

(defn executing?
  [x]
  "Is x of the form: (executing ...)?"
  (and (seq? x) (= 'executing (first x))))

(defn convert-op
  [op]
  "Make op conform the the (EXECUTING op) convention"
  (if-not (some executing? (:add-list op))
    (struct operation 
	    (:action op) 
	    (:preconditions op) 
	    (set (conj (:add-list op) (list 'executing (:action op))))
	    (:del-list op))
    op))

(defn make-op
  [action preconditions add-list del-list]
  (convert-op (struct operation action preconditions add-list del-list)))

(defn appropriate?
  [goal operation]
  "An op is appropriate to a goal if it is in its add list"
  (contains-value? (:add-list operation) goal))

(declare achieve-all)

(defn apply-op
  [state goal op goal-stack]
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (count goal-stack) "Consider: %s" (:action op))
  (let [new-state (achieve-all state (:preconditions op) (cons goal goal-stack))]
    (when-not (nil? state)
      (dbg-indent :gps (count goal-stack) "Action: %s" (:action op))
      (concat (remove (fn [x] (= x (:del-list op))) new-state)
	      (:add-list op)))))

(defn achieve
  [state goal goal-stack]
  "A goal is achieved if it already holds,
   or if there is an appropriate op for it that is applicable"
  (dbg-indent :gps (count goal-stack) "Goal: %s" goal)

  (cond
    (state goal) state
    (contains-value? goal-stack goal) nil
    :else (set (some (fn [op] (apply-op state goal op goal-stack))
		     (filter (fn [x] (appropriate? goal x)) @*ops*)))))

(defn achieve-all 
  [state goals goal-stack]
  "Achieve each goal, and make sure they still hold at the end."
  (let [current-state (atom state)]
    (if (and (every? (fn [g] (swap! current-state 
				    (fn [s] (achieve s g goal-stack)))) goals)
	     (subset? goals @current-state))
      @current-state)))


(defn GPS
  [state goals ops]
  "General Problem Solver: from state, achieve using ops"
  (dosync
   (ref-set *ops* ops))
  (remove (comp not sequential?) (achieve-all (set (cons (list 'start) state)) goals [])))

(def school-ops
    [(make-op 'drive-son-to-school
         #{'son-at-home 'car-works}
         #{'son-at-school}
         #{'son-at-home})

    (make-op 'shop-installs-battery
         #{'car-needs-battery 'shop-knows-problem 'shop-has-money}
         #{'car-works}
	 #{})

    (make-op 'tell-shop-problem
          #{'in-communication-with-shop}
          #{'shop-knows-problem}
	  '{})

    (make-op 'telephone-shop
          #{'know-phone-number}
          #{'in-communication-with-shop}
	  #{})

    (make-op 'look-up-number
         #{'have-phone-book}
         #{'know-phone-number}
	 #{})

    (make-op 'give-shop-money
         #{'have-money}
         #{'shop-has-money}
         #{'have-money})])

(defn school-example
  []
  (GPS #{'son-at-home 'car-needs-battery 'have-money 'have-phone-book}
       #{'son-at-school}
       school-ops))

(def banana-ops
     [
      (make-op 'climb-on-chair
	       #{'chair-at-middle-room 'at-middle-room 'on-floor}
	       #{'at-bananas 'on-chair}
	       #{'at-middle-room 'on-floor})
      (make-op 'push-chair-from-door-to-middle-room
	       #{'chair-at-door 'at-door}
	       #{'chair-at-middle-room 'at-middle-room}
	       #{'chair-at-door 'at-door})
      (make-op 'walk-from-door-to-middle-room
	       #{'at-door 'on-floor}
	       #{'at-middle-room}
	       #{'at-door})
      (make-op 'grasp-bananas
	       #{'at-bananas 'empty-handed}
	       #{'has-bananas}
	       #{'empty-handed})
      (make-op 'drop-ball
	       #{'has-ball}
	       #{'empty-handed}
	       #{'has-ball})
      (make-op 'eat-bananas
	       #{'has-bananas}
	       #{'empty-handed 'not-hungry}
	       #{'has-bananas 'hungry})])

(defn money-and-bananas
  []
  (GPS #{'at-door 'on-floor 'has-ball 'hungry 'chair-at-door}
       #{'not-hungry}
       banana-ops))
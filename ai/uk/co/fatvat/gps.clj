;;; Implementation of a simplified General Problem Solver
;;; http://en.wikipedia.org/wiki/General_Problem_Solver
;;; Based on code in "Paradigms of Artificial Intelligence Programming"
(ns uk.co.fatvat.gps
  (:use [clojure.set]))

(defstruct operation :action :preconditions :add-list :delete-list)

(defn make-op
  [action preconditions add-list delete-list]
  (struct operation action preconditions add-list delete-list))

(def *state* (ref nil))

(def *ops* (ref nil))

(defn contains-value?
  [coll val]
  (not (nil? (some (partial = val) coll))))

(defn appropriate?
  [goal operation]
  "An op is appropriate to a goal if it is in its add list"
  (contains-value? (:add-list operation) goal))

(defn apply-op
 [op]
 "Print a message and update state if op is applicable"
 (when (every? achieve (:preconditions op))
   (println "Executing: " (:action op))
   (dosync
    (alter *state* 
	   (fn [s]
	     (union 
	      (difference s (:delete-list op))
	      (:add-list op)))))))

(defn achieve
  [goal]
  "A goal is achieved if it already holds.  Or if there is an appropriate
   operation for it that is applicable"
  (or (contains-value? @*state* goal)
      (some apply-op (filter (fn [x] (appropriate? goal x))  @*ops*))))

(defn gps
  [state goals ops]
  "General Problem Solver: Achieve all goals using the operations available."
  (when (every? achieve goals) 'solved))

(def example-operations
     [(make-op 'message-friends
	       #{'have-facebook}
	       #{'friends-emailed}
	       #{'at-home})

      (make-op 'arrange-party
	       #{'friends-emailed}
	       #{'party-arranged}
	       #{})

      (make-op 'get-on-bus
	       #{'party-arranged}
	       #{'at-club}
	       #{})
      
      (make-op 'drink
	       #{'have-drink}
	       #{'had-drink}
	       #{'have-drink})

      (make-op 'dance
	       #{'had-drink}
	       #{'dancing}
	       #{})

      (make-op 'give-bar-money
	       #{'have-money 'at-club}
	       #{'bar-has-money 'have-drink}
	       #{'have-money})])

(defn example []
  (dosync
   (ref-set *state* #{'at-home 'have-money 'have-facebook}) 
   (ref-set *ops* example-operations))
  (gps @*state*
       #{'dancing 'at-club}
       example-operations))

;;; Eliza example from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.eliza
  (:use [clojure.contrib.test-is]))

(defn position
  "First index within the sequence satisfying test"
  ([test coll]
     (position test coll 0))
  ([test coll start]
     (position test (nthnext coll start) start start))
  ([test coll start current-pos]
     (cond
       (empty? coll) nil
       (test (first coll)) current-pos
       :else (recur test (rest coll) start (inc current-pos)))))

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn segment-pattern?
  "Is this a segment matching pattern?"
  [pattern]
  (and (sequential? pattern)      
       (sequential? (first pattern))
       (= (ffirst pattern) '?*)))

;; Sentinel value for fail
(def fail {:fail :fail})

;; Indicates pat-match success, with no variables
(def no-bindings {})

(defn match-variable
  "Does var match input? Uses (or updates) and return bindings."
  [var input bindings]
  (let [binding-value (bindings var)]
    (cond
      (nil? binding-value) (assoc bindings var input)
      (= input binding-value) bindings
      :else fail)))

(declare pat-match)

(defn segment-match
  "Match the segment pattern ((?* var) pat) against input"
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern)) ;; TODO first again?
	   pat (rest pattern)]
       (if (empty? pat)
	 (match-variable var input bindings)
	 ;; We assume that pat starts with a constant
	 ;; In other words, a pattern can't have 2 consecutive vars
	 (let [pos (position (partial = (first pat)) input start)]
	   (if (nil? pos)
	     fail
	     (let [b2 (pat-match pat (map identity (subvec (vec input) pos))
				 (match-variable var (map identity (subvec (vec input) 0 pos)) bindings))]
	       ;; If this match failed, try another longer one
	       (if (= b2 fail)
		 (segment-match pattern input bindings (inc pos))
		 b2))))))))

(defn pat-match 
  "Match pattern against input in the contet of the bindings"
  ([pattern input] 
     (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond
       (= bindings fail) fail
       (variable? pattern) (match-variable pattern input bindings)
       (= pattern input) bindings
       (segment-pattern? pattern) (segment-match pattern input bindings)
       (and (sequential? pattern) 
	    (sequential? input)) (pat-match 
				  (rest pattern) (rest input)
				  (pat-match 
				   (first pattern) 
				   (first input) 
				   bindings))
	  :else fail)))

(deftest test-pattern-matching
  (is (= fail (pat-match '(i need a ?X) '(i really need a vacation))))
  (is (= no-bindings (pat-match '(this is easy) '(this is easy))))
  (is (= fail (pat-match '(?X is ?X) '((2 + 2 is 4)))))
  (is (= '{?X (2 + 2)} (pat-match '(?X is ?X) '((2 + 2) is (2 + 2)))))
  (is (= '{?P (Mr Hulot and I) ?X (a vacation)} (pat-match '((?* ?P) need (?* ?X)) 
							   '(Mr Hulot and I need a vacation))))
  (is (= '{?X (1 2 a b)} (pat-match '((?* ?X) a b (?* ?X)) '(1 2 a b a b 1 2 a b)))))

;; Because we don't have dotted pairs, this test will fail.  This will be addressed
;; in improved versions
;;  (is (= '{(?X a long vacation) (?P I)} (pat-match '(?P need . ?X) '(i need a long vacation)))))


(def *eliza-rules*
 '((((?* ?x) hello (?* ?y))      
    (How do you do.  Please state your problem.))
   (((?* ?x) computer (?* ?y))
    (Do computers worry you?) (What do you think about machines?)
    (Why do you mention computers?)
    (What do you think machines have to do with your problem?))
   (((?* ?x) name (?* ?y))
    (I am not interested in names))
   (((?* ?x) sorry (?* ?y))
    (Please don't apologize) (Apologies are not necessary)
    (What feelings do you have when you apologize))
   (((?* ?x) I remember (?* ?y)) 
    (Do you often think of ?y)
    (Does thinking of ?y bring anything else to mind?)
    (What else do you remember) (Why do you recall ?y right now?)
    (What in the present situation reminds you of ?y)
    (What is the connection between me and ?y))
   (((?* ?x) do you remember (?* ?y))
    (Did you think I would forget ?y ?)
    (Why do you think I should recall ?y now)
    (What about ?y) (You mentioned ?y))
   (((?* ?x) if (?* ?y)) 
    (Do you really think its likely that ?y) (Do you wish that ?y)
    (What do you think about ?y) (Really-- if ?y))
   (((?* ?x) I dreamt (?* ?y))
    (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
    (Have you dreamt ?y before?))
   (((?* ?x) dream about (?* ?y))
    (How do you feel about ?y in reality?))
   (((?* ?x) dream (?* ?y))    
    (What does this dream suggest to you?) (Do you dream often?)
    (What persons appear in your dreams?)
    (Don't you believe that dream has to do with your problem?))
   (((?* ?x) my mother (?* ?y))
    (Who else in your family ?y) (Tell me more about your family))
   (((?* ?x) my father (?* ?y))
    (Your father) (Does he influence you strongly?) 
    (What else comes to mind when you think of your father?))

   (((?* ?x) I want (?* ?y))     
    (What would it mean if you got ?y)
    (Why do you want ?y) (Suppose you got ?y soon))
   (((?* ?x) I am glad (?* ?y))
    (How have I helped you to be ?y) (What makes you happy just now)
    (Can you explain why you are suddenly ?y))
   (((?* ?x) I am sad (?* ?y))
    (I am sorry to hear you are depressed)
    (I'm sure its not pleasant to be sad))
   (((?* ?x) are like (?* ?y))   
    (What resemblance do you see between ?x and ?y))
   (((?* ?x) is like (?* ?y))    
    (In what way is it that ?x is like ?y)
    (What resemblance do you see?)
    (Could there really be some connection?) (How?))
   (((?* ?x) alike (?* ?y))      
    (In what way?) (What similarities are there?))
   (((?* ?x) same (?* ?y))       
    (What other connections do you see?))

   (((?* ?x) I was (?* ?y))       
    (Were you really?) (Perhaps I already knew you were ?y)
    (Why do you tell me you were ?y now?))
   (((?* ?x) was I (?* ?y))
    (What if you were ?y ?) (Do you thin you were ?y)
    (What would it mean if you were ?y))
   (((?* ?x) I am (?* ?y))       
    (In what way are you ?y) (Do you want to be ?y ?))
   (((?* ?x) am I (?* ?y))
    (Do you believe you are ?y) (Would you want to be ?y)
    (You wish I would tell you you are ?y)
    (What would it mean if you were ?y))
   (((?* ?x) am (?* ?y))
    (Why do you say "AM?") (I don't understand that))
   (((?* ?x) are you (?* ?y))
    (Why are you interested in whether I am ?y or not?)
    (Would you prefer if I weren't ?y)
    (Perhaps I am ?y in your fantasies))
   (((?* ?x) you are (?* ?y))   
    (What makes you think I am ?y ?))
   (((?* ?x) because (?* ?y))
    (Is that the real reason?) (What other reasons might there be?)
    (Does that reason seem to explain anything else?))
   (((?* ?x) were you (?* ?y))
    (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
   (((?* ?x) I can't (?* ?y))    
    (Maybe you could ?y now) (What if you could ?y ?))
   (((?* ?x) I feel (?* ?y))     
    (Do you often feel ?y ?))
   (((?* ?x) I felt (?* ?y))     
    (What other feelings do you have?))
   (((?* ?x) I (?* ?y) you (?* ?z))   
    (Perhaps in your fantasy we ?y each other))
   (((?* ?x) why don't you (?* ?y))
    (Should you ?y yourself?)
    (Do you believe I don't ?y) (Perhaps I will ?y in good time))
   (((?* ?x) yes (?* ?y))
    (You seem quite positive) (You are sure) (I understand))
   (((?* ?x) no (?* ?y))
    (Why not?) (You are being a bit negative)
    (Are you saying "NO" just to be negative?))
   (((?* ?x) someone (?* ?y))
    (Can you be more specific?))
   (((?* ?x) everyone (?* ?y))
    (surely not everyone) (Can you think of anyone in particular?)
    (Who for example?) (You are thinking of a special person))
   (((?* ?x) always (?* ?y))
    (Can you think of a specific example) (When?)
    (What incident are you thinking of?) (Really-- always))
   (((?* ?x) what (?* ?y))
    (Why do you ask?) (Does that question interest you?)
    (What is it you really want to know?) (What do you think?)
    (What comes to your mind when you ask that?))
   (((?* ?x) perhaps (?* ?y))    
    (You do not seem quite certain))
   (((?* ?x) are (?* ?y))
    (Did you think they might not be ?y)
    (Possibly they are ?y))
   (((?* ?x))               
    (Very interesting) (I am not sure I understand you fully)
    (What does that suggest to you?) (Please continue) (Go on) 
    (Do you feel strongly about discussing such things?))))
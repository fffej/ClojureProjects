;;; Eliza example from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.eliza)

;; Pattern matching bits and bobs
(defn variable? [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn segment-pattern? 
  "Is this a segment matching pattern: ((?* var) . pat)"
  [p]
  (and (seq? p)
       (.startsWith (first p) "?*")))

(defn atom? [x]
  (not (seq? x)))

(defn pat-match-v1
  "Does pattern match input?  Any variable can match anything."
  [pattern input]
  (if (variable? pattern)
    true
    (if (or (atom? pattern) (atom? input))
        (= pattern input)
	(and (pat-match-v1 (first pattern) (first input))
	     (pat-match-v1 (rest pattern) (rest input))))))

;; We differ from PAIP in just using a map which simplifies things
;; somewhat.  We use the empty map to signify a match that has no
;; bindings
(def no-bindings {})

(defn match-variable
  "Does VAR match input?  Uses (or updates) and returns bindings."
  [var input bindings]
  (let [binding (bindings var)]
    (cond
     (nil? binding) (assoc bindings var input)
     (= input binding) bindings
     :else nil)))

(defn pat-match-v2
  "Match pattern against input in the context of the bindings"
  ([pattern input] 
    (pat-match-v2 pattern input no-bindings))
  ([pattern input bindings]
    (cond
     (nil? bindings) nil
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (and (seq? pattern) (seq? input))
       (pat-match-v2 
	(rest pattern) 
	(rest input)
	(pat-match-v2 (first pattern) (first input) bindings))
     :else nil)))

;; Segment pattern matching
(defn segment-pattern? 
  "Is this a segment matching pattern: ((?* var) . pattern)"
  [pattern]
  (and (seq? pattern) (seq? (first pattern)) (.startsWith (str (ffirst pattern)) "?*")))

(defn position [elt vec start]
  (if (>= start (count vec))
    nil
    (if (= (nth vec start) elt)
      start
      (recur elt vec (inc start)))))

(declare pat-match)

(defn sub-sequence 
  ([lst start] 
     (sub-sequence lst start (count lst)))
  ([lst start end]
  ((fn [lst x y accum]
     (if (>= x y)
       (reverse accum)
       (recur (rest lst) (inc x) y (conj accum (first lst))))) (nthnext lst start) 0 (- end start) '())))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern)) pat (rest pattern)]
       (if (nil? pat)
	   (match-variable var input bindings)
	   (let [pos (position (first pat) input start)]
	     (if pos
	       (let [b2 (pat-match pat (sub-sequence input pos) (match-variable var (sub-sequence input 0 pos) bindings))]
		 (if (nil? b2)
		   (segment-match pattern input bindings (inc pos))
		   b2))
	       nil))))))

(defn pat-match
  "Match pattern against input in the context of the bindings"
  ([pattern input]
     (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond
      (nil? bindings) nil
      (variable? pattern) (match-variable pattern input bindings)
      (= pattern input) bindings
      (segment-pattern? pattern) (segment-match pattern input bindings) ;;; To be determined
      (and (seq? pattern) (seq? input))
        (pat-match (rest pattern) (rest input) (pat-match (first pattern) (first input) bindings))
      :else nil)))

(defn switch-viewpoint [words]
  (replace '{I,You You,I me,you am,are} words))

(defn rule-pattern [rule]
  (first rule))

(defn rule-responses [rule]
  (fnext rule))

(defn random-elt [choices]
  "Choose an element at random froma seq"
  (nth choices (rand-int (count choices)))) 

(defn use-eliza-rules [input rules]
  "Find some rule with which to transform the input"
  (some (fn[rule] 
	  (let [result (pat-match (rule-pattern rule) input)]
	    (when-not (nil? result)
	      (replace (into {} (switch-viewpoint result)) (random-elt (rule-responses rule)))))) rules))

;;; Dump in a bunch of rules
(def *eliza-rules*
     '[[((?* ?x) hello (?* ?y)) 
       [(How do you do.  Please state your problem.)]]

       [((?* ?x) computer (?* ?y))
       [(Do computers worry you?) (What do you think about machines?) (Why do you mention computers?) (What do you think machines have to do with your problem?)]]

       [((?* ?x) name (?* ?y))
       [(I am not interested in names)]]

       [((?* ?x) sorry (?* ?y))
       [(Please don't apologize) (Apologies are not necessary) (What feelings do you have when you apologize)]]

       [((?* ?x) I remember (?* ?y))
       [(Do you often think of ?y) (Does thinking of ?y bring anything else to mind?) (What else do you remember) (Why do you recall ?y right now?) (What in the present situation reminds you of ?y) (What is the connection between me and ?y)]]


       [((?* ?x) do you remember (?* ?y))
       [(Did you think I would forget ?y ?) (Why do you think I should recall ?y now) (What about ?y) (You mentioned ?y)]]

       [((?* ?x) if (?* ?y))
       [(Do you really think its likely that ?y) (Do you wish that ?y) (What do you think about ?y) (Really-- if ?y)]]


       [((?* ?x) I dreamt (?* ?y))
       [(Really-- ?y) (Have you ever fantasized ?y while you were awake?) (Have you dreamt ?y before?)]]
       
       [((?* ?x) dream about (?* ?y))
       [(How do you feel about ?y in reality?)]]


       [((?* ?x) dream (?* ?y))
       [(What does this dream suggest to you?) (Do you dream often?) (What persons appear in your dreams?) (Don't you believe that dream has to do with your problem?)]]
       
       [((?* ?x) my mother (?* ?y))
       [(Who else in your family ?y) (Tell me more about your family)]]

       [((?* ?x) my father (?* ?y))
       [(Your father) (Does he influence you strongly?)  (What else comes to mind when you think of your father?)]]

       [((?* ?x) I want (?* ?y))     
       [(What would it mean if you got ?y) (Why do you want ?y) (Suppose you got ?y soon)]]
       
       [((?* ?x) I am glad (?* ?y))
       [(How have I helped you to be ?y) (What makes you happy just now) (Can you explain why you are suddenly ?y)]]

       [((?* ?x) I am sad (?* ?y))
       [(I am sorry to hear you are depressed) (I'm sure its not pleasant to be sad)]]

       [((?* ?x) are like (?* ?y))   
       [(What resemblance do you see between ?x and ?y)]]

       [((?* ?x) is like (?* ?y))    
       [(In what way is it that ?x is like ?y) (What resemblance do you see?) (Could there really be some connection?) (How?)]]

       [((?* ?x) alike (?* ?y))      
       [(In what way?) (What similarities are there?)]]
       
       [((?* ?x) same (?* ?y))
       [(What other connections do you see?)]]

       [((?* ?x) I was (?* ?y))       
       [(Were you really?) (Perhaps I already knew you were ?y) (Why do you tell me you were ?y now?)]]
       
       [((?* ?x) was I (?* ?y))
       [(What if you were ?y ?) (Do you thin you were ?y) (What would it mean if you were ?y)]]

       [((?* ?x) I am (?* ?y))       
       [(In what way are you ?y) (Do you want to be ?y ?)]]

       [((?* ?x) am I (?* ?y))
       [(Do you believe you are ?y) (Would you want to be ?y) (You wish I would tell you you are ?y) (What would it mean if you were ?y)]]

       [((?* ?x) am (?* ?y))
       [(Why do you say "AM?") (I don't understand that)]]

       [((?* ?x) are you (?* ?y))
       [(Why are you interested in whether I am ?y or not?) (Would you prefer if I weren't ?y) (Perhaps I am ?y in your fantasies)]]

       [((?* ?x) you are (?* ?y))
       [(What makes you think I am ?y ?)]]

       [((?* ?x) because (?* ?y))
       [(Is that the real reason?) (What other reasons might there be?) (Does that reason seem to explain anything else?)]]

       [((?* ?x) were you (?* ?y))
       [(Perhaps I was ?y) (What do you think?) (What if I had been ?y)]]

       [((?* ?x) I can't (?* ?y))
       [(Maybe you could ?y now) (What if you could ?y ?)]]

       [((?* ?x) I feel (?* ?y))
       [(Do you often feel ?y ?)]]
       
       [((?* ?x) I felt (?* ?y))
       [(What other feelings do you have?)]]

       [((?* ?x) I (?* ?y) you (?* ?z))   
       [(Perhaps in your fantasy we ?y each other)]]

       [((?* ?x) why don't you (?* ?y))
       [(Should you ?y yourself?) (Do you believe I don't ?y) (Perhaps I will ?y in good time)]]

       [((?* ?x) yes (?* ?y))
       [(You seem quite positive) (You are sure) (I understand)]]

       [((?* ?x) no (?* ?y))
       [(Why not?) (You are being a bit negative) (Are you saying "NO" just to be negative?)]]

       [((?* ?x) someone (?* ?y))
       [(Can you be more specific?)]]

       [((?* ?x) everyone (?* ?y)),
       [(surely not everyone) (Can you think of anyone in particular?) (Who for example?) (You are thinking of a special person)]]

       [((?* ?x) always (?* ?y))
       [(Can you think of a specific example) (When?) (What incident are you thinking of?) (Really-- always)]]

       [((?* ?x) what (?* ?y)),
       [(Why do you ask?) (Does that question interest you?) (What is it you really want to know?) (What do you think?) (What comes to your mind when you ask that?)]]
       [((?* ?x) perhaps (?* ?y))   
       [(You do not seem quite certain)]]

       [((?* ?x) are (?* ?y))
       [(Did you think they might not be ?y) (Possibly they are ?y)]]

       [((?* ?x))               
       [(Very interesting) (I am not sure I understand you fully) (What does that suggest to you?) (Please continue) (Go on) (Do you feel strongly about discussing such things?)]]])

;; Finish some typing!

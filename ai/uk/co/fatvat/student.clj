;;; jeff.foster@acm.org
(ns uk.co.fatvat.student
  (:use clojure.contrib.trace)
  (:use clojure.walk)
  (:use clojure.contrib.def)
  (:use clojure.test)
  (:use uk.co.fatvat.debug)
  (:use uk.co.fatvat.patmatch))

(defstruct exp :op :lhs :rhs)

(defn exp?
  [e]
  (and
   (not= nil (:op e))
   (not= nil (:lhs e))
   (not= nil (:rhs e))))

(defn mk-exp
  "Create an expression with the given lhs, rhs and operation"
  [lhs op rhs]
  (struct exp op lhs rhs))

(defn mk-exp-infix
  "Create an expression from the infix args"
  [e]
  (if (sequential? e)
    (struct exp (first e) (mk-exp-infix (nth e 1)) (mk-exp-infix (nth e 2)))
    e))

(defn eval-exp
  "Gargh, why did I choose this representation?"
  [e]
  (if (exp? e)
    ((:op e) (eval-exp (:lhs e)) (eval-exp (:rhs e)))
    e))

(defn exp-args
  "The arguments of an expression"
  [exp]
  [(:lhs exp) (:rhs exp)])

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

;; TODO "." isn't a special character, but how should , be handled?
;; `{ ~(list 'if '?x* (symbol ",") 'then '?y*) 1}
(defvar *basic-student-rules*
   `[
     ~['(?x* .) '?x]
     ~['(?x* . ?y*) '(?x ?y)]
     ~[(list 'if '?x* (symbol ",") 'then '?y*)  '(?x ?y)]
     ~['(if ?x* then ?y*)      '(?x ?y)]
     ~[(list 'if '?x* (symbol ",") '?y*)       '(?x ?y)]
     ~['(?x* and ?y*)      '(?x ?y)]
     ~['(find ?x* and ?y*)     '((= to-find-1 ?x) (= to-find-2 ?y))]
     ~['(find ?x*)             '(= to-find ?x)]
     ~['(?x* equals ?y*)       '(= ?x ?y)]
     ~['(?x* same as ?y*)      '(= ?x ?y)]
     ~['(?x* = ?y*)            '(= ?x ?y)]
     ~['(?x* is equal to ?y*)  '(= ?x ?y)]
     ~['(?x* is ?y*)           '(= ?x ?y)]
     ~['(?x* - ?y*)            '(- ?x ?y)]
     ~['(?x* minus ?y*)        '(- ?x ?y)]
     ~['(difference between ?x* and ?y*)  '(- ?y ?x)]
     ~['(difference ?x* and ?y*)          '(- ?y ?x)]
     ~['(?x* + ?y*)            '(+ ?x ?y)]
     ~['(?x* plus ?y*)         '(+ ?x ?y)]
     ~['(sum ?x* and ?y*)      '(+ ?x ?y)]
     ~['(product ?x* and ?y*)  '(* ?x ?y)]
     ~['(?x* * ?y*)            '(* ?x ?y)]
     ~['(?x* times ?y*)        '(* ?x ?y)]
     ~['(?x* / ?y*)            '(/ ?x ?y)]
     ~['(?x* per ?y*)          '(/ ?x ?y)]
     ~['(?x* divided by ?y*)   '(/ ?x ?y)]
     ~['(half ?x*)             '(/ ?x 2)]
     ~['(one half ?x*)         '(/ ?x 2)]
     ~['(twice ?x*)            '(* 2 ?x)]
     ~['(square ?x*)           '(* ?x ?x)]
     ~['(?x* % less than ?y*)  '(* ?y (/ (- 100 ?x) 100))]
     ~['(?x* % more than ?y*)  '(* ?y (/ (+ 100 ?x) 100))]
     ~['(?x* % ?y*)            '(* (/ ?x 100) ?y)]]
  "The list of student rules (pre-expansion)")

(defvar operators-and-inverses
  '{+ -, - +, * /, / *, = =} 
  "Operators and inverses")

(defn inverse-op
  [op]
  (operators-and-inverses op))

(defn- map-key
  "Expand the rules"
  [f [k v]]
  [(f k) v])

(defvar *student-rules* (map (partial map-key expand-pat-match-abbrev) *basic-student-rules*)
  "Student rules (post-expansion)")

(defn create-list-of-equations
  "Separate out the equations embedded in nested parens"
  [exp]
  (cond
    (empty? exp) nil
    (not (sequential? (first exp))) (list exp)
    :else (concat 
           (create-list-of-equations (first exp))
           (create-list-of-equations (rest exp)))))

(defn noise-word?
  "Is this a low-content word that can be ignored?"
  [word]
  ('#{a an the this number of $} word))

(defn make-variable
  "Create a variable name based on the given list of words from 
   which noise words have already been removed"
  [words]
  (first words))

(declare translate-to-expression)

(defn translate-pair 
  "Translate the value part of the pair into an equation or expression"
  [pair]
  (cons
   (rest pair) ;; TODO binding-var?
   (translate-to-expression (rest pair))))

(defn translate-to-expression
  "Translate an English phrase into an equation or expression"
  [words]
  (or
   (rule-based-translator words 
                          *student-rules* 
                          :action (fn [bindings response]
                                    (postwalk-replace 
                                     (into {} 
                                           (map (fn [[var binding]] 
                                                  [var (translate-to-expression binding)]) 
                                                bindings))
                                     response)))
   (make-variable words)))

(defn commutative?
  [op]
  ('#{+ * =} op))

(defn binary-exp?
  [x]
  (and (exp? x) (= 2 (count (exp-args x)))))

(defn prefix->infix
  "Translate prefix to infix expressions."
  [exp]
  (if (sequential? exp)
    (map (comp first prefix->infix)
         (if (binary-exp? exp)
           (list (:lhs exp) (:op exp) (:rhs exp))
           exp))
    exp))

(defn print-equations
  "Print a list of equations."
  [header equations]
  (println header equations))
;  (println (format "%s %s" header (map (comp first prefix->infix) equations))))

(defn unknown?
  [exp]
  (symbol? exp))

;; TODO I get the feeling I should use clojure.walk here somewhere
(defn in-exp
  "True if x appears anywhere in exp"
  [x exp]
  (or (= x exp)
      (and (exp? exp)
           (or (in-exp x (:lhs exp))
               (in-exp x (:rhs exp))))))

(defn no-unknown
  "Returns true if there are no unknowns in exp"
  [exp]
  (cond
    (unknown? exp) false
    (not (exp? exp)) true
    (no-unknown (:lhs exp)) (no-unknown (:rhs exp))))

(deftest test-no-unknown
  (is (= true (no-unknown (mk-exp-infix '(+ 1 2)))))
  (is (= false (no-unknown (mk-exp-infix '(+ 1 x)))))
  (is (= false (no-unknown (mk-exp-infix '(+ 1 (* 2 y))))))
  (is (= true (no-unknown (mk-exp-infix '(+ (+ 1 2) (+ 3 4)))))))

(defn one-unknown
  "Returns the single unknown in exp, if there is exactly one." 
  [exp]
  (cond
    (unknown? exp) exp
    (not (exp? exp)) nil
    (no-unknown (:lhs exp)) (one-unknown (:rhs exp))
    (no-unknown (:rhs exp)) (one-unknown (:lhs exp))))
         
(defn solve-arithmetic
  "Do arithmetic for the right-hand side"
  [equation]
  (mk-exp (:lhs equation) '= (eval-exp (:rhs equation))))

(defn isolate 
  "Isolate the lone x in e on the left-hand side of e"
  [e x]
  (dbg :student (format "e=%s x=%s" e x))
  (cond
    ;; X = A ==> X = n
    (= (:lhs e) x) e
    
    ;; A = f(X) ==> f(X) = A
    (in-exp x (:rhs e)) (isolate (mk-exp (:rhs e) '= (:lhs e)) x)

    ;; f(X) * A = B ==> f(X) = B / A
    (in-exp x (:lhs (:lhs e))) (isolate (mk-exp (:lhs (:lhs e)) 
                                                '=
                                                (mk-exp (:rhs e)
                                                       (inverse-op (:op (:lhs e)))
                                                       (:rhs (:lhs e))))
                                        x)

    ;; A * f(X) = B ==> f(x) = B / A
    (commutative? (:op (:lhs e))) (isolate (mk-exp (:rhs (:lhs e)) 
                                                   '=
                                                   (mk-exp (:rhs e)
                                                           (inverse-op (:op (:lhs e)))
                                                           (:lhs (:lhs e))))
                                           x)

    ;; A / f(X) = B ==> f(x) = A / B
    :else (isolate (mk-exp (:rhs (:lhs e)) 
                           '=
                           (mk-exp (:lhs (:lhs e))
                                   (:op (:lhs e))
                                   (:rhs e)))
                   x)))
                                                  

(defn solve
  "Solve a system of equations by constraint propagation"
  [equations known]
  (dbg :student (format "SOLVE %s %s" equations known))
  (or
   (some (fn [equation]
           (let [x (one-unknown equation)]
             (when x
               (let [answer (solve-arithmetic (isolate equation x))]
                 (solve (postwalk-replace {(:lhs answer) (:rhs answer)}
                                          (remove (partial = equation) equations))
                        (cons answer known))))))
         equations)
   known))

(defn solve-equations
  "Print the equations and their solution"
  [equations]
  (print-equations "The equations to be solved are:" equations))
;  (print-equations "The solution is:" (solve (map mk-exp-infix equations) nil)))

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (solve-equations
   (create-list-of-equations
    (translate-to-expression (remove noise-word? words)))))


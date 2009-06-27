(ns uk.co.fatvat.icfp
  (:import [java.lang.reflect Array])
  (:import [java.io FileInputStream File]))

;;; Location of relevant files
(def bin1 "/home/jfoster/clojureprojects/icfp/uk/co/fatvat/bin1.obf")

;;; Virtual machine specification
(defstruct virtualmachine :mem :counter :inport :outport :status)

(defn numeric-op
  "D-type General numeric op"
  [vm args f]
  (dosync
   (let [m (:mem vm)]
     (ref-set (m @(:counter vm)) (f @(m (first args)) @(m (second args)))))))  

(defn trace
  ([vm op]
     (println @(:counter vm) op))
  ([vm op rest]
     (println @(:counter vm) op rest)))

(defn phi
  "D-type"
  [vm [x y]]
  (let [m (:mem vm)]
    (trace vm 'Phi (format "%s ? %s : %s --> ??" @(:status vm) @(m x) @(m y)))
    (dosync
     (ref-set (m @(:counter vm))
	      (if @(:status vm)
		@(m x)
		@(m y))))))

(defn get-val
  [vm x]
  @((:mem vm) x))

(defn print-args
  [vm op x y]
  (format "%s %s // %s %s %s" x y (get-val vm x) op (get-val vm y)))
    
(defn add
  "D-type Add instruction"
  [vm [x y]]
  (trace vm 'Add (print-args vm '+ x y))
  (numeric-op vm [x y] +))

(defn sub
  "D-type Sub instruction"
  [vm [x y]]
  (trace vm 'Sub (print-args vm '- x y))
  (numeric-op vm [x y] -))

(defn mult
  "D-type Multiply instruction"
  [vm [x y]]
  (trace vm 'Mult (print-args vm '* x y))
  (numeric-op vm [x y] *))

(defn div
  "D-type Divide"
  [vm args]
  (trace vm 'Div)
  (numeric-op vm args (fn [x y] (if (zero? y) 0 (/ x y)))))

(defn noop
  "S-type Noop instruction"
  [vm args]
  (trace vm 'Noop)
  vm)

(defn copy
  "S-Type: Copy instruction"
  [vm [x]]
  (trace vm 'Copy (format "%s // %s" x @((:mem vm) x)))
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) @((:mem vm) x))))

(defn sqrt
  "S-Type: Square root instruction: undefined for negative values"
  [vm args]
  (trace vm 'Sqrt)
  (assert (not (neg? @((:mem vm) (first args)))))
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) (Math/sqrt @((:mem vm) (first args))))))

(defn input
  "S-Type: Set the memory from the inport"
  [vm args]
  (trace vm 'Input)
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) @((:inport vm) (first args)))))

(defn output
  "Output instruction: Set the memory on the outport"
  [vm args]
  (trace vm 'Output)
  (dosync
   (ref-set ((:outport vm) (first args))  @((:mem vm) (second args)))))

(defn cmpz
  "Comparison function"
  [vm args]
  (trace vm 'Cmpz)
  (let [cmp (first args)
	val @((:mem vm) (second args))
	status (cond 
		 (= cmp 'LTZ) (< val 0)
		 (= cmp 'LEZ) (<= val 0)
		 (= cmp 'EQZ) (zero? val)
		 (= cmp 'GEZ) (> val 0)
		 (= cmp 'GTZ) (>= val 0)
		 :else (assert false))]
    (dosync
     (ref-set (:status vm) status))))

(def d-type-instructions {1 add, 2 sub, 3 mult, 4 div, 5 output, 6 phi})
(def s-type-instructions {0 noop, 1 cmpz, 2 sqrt, 3 copy, 4 input})
(def comparison {0 'LTZ, 1 'LEZ, 2 'EQZ, 3 'GEZ, 4 'GTZ})

;;; Reading in the file from disk and bit manipulation fu
(defn get-bytes
  "Read the bytes for the given file, stored as a sequence of bytes"
  [filename]
  (let [file (File. filename)]
    (assert (.exists file))
    (with-open [x (FileInputStream. file)]
      (doall
       (into [] (take-while (partial not= -1) (repeatedly #(.read x))))))))

(defn to-double
  "Convert the given series of 8 bytes into an IEEE 754 number"
  [bytes]
  (Double/longBitsToDouble 
   (long
    (reduce bit-or 
	    (map (fn [x shift] (bit-shift-left (bit-and (int x) 0xFF) shift))
		 bytes 
		 (range 0 64 8))))))

(defn to-int
  "Convert the given series of bytes into an integer"
  [bytes]
  (int (reduce bit-or
	       (map (fn [x shift] (bit-shift-left (bit-and (int x) 0xFF) shift))
		    bytes
		    (range 0 32 8)))))


(defn- d-args
  "Convert a D-type instruction into a vector of op-code + args"
  [ins]
  (let [x (to-int ins)]
    [(bit-shift-right (bit-and x 0xFFFC000) 14) (bit-and x 0x00003FFF)]))

(defn- s-args
  "Convert an S-type instruction into a vector of op-code and args"
  [op ins]
  (if (= cmpz op)
    [(comparison (bit-shift-right (bit-and (to-int ins) 0x700000) 21)) (bit-and (to-int ins) 0x00003FFF)]
    [(bit-and (to-int ins) 0x00003FFF)
     (bit-shift-right (bit-and (last (butlast ins)) 0xF0) 4)]))

(defn get-op
  "Decode the 4 bytes as an op code, complete with arguments"
  [ins]
  (let [d-opcode (bit-shift-right (bit-and 0xF0 (last ins)) 4)
	s-opcode (bit-and 0x0F (last ins))]
    (if (zero? d-opcode)
      (let [sins (s-type-instructions s-opcode)]
	[sins (s-args sins ins)])
      [(d-type-instructions d-opcode) (d-args ins)])))

(defn get-instruction-data
  "Decode the data at the given address (in 96 bit multiplies)"
  [image address]
  (if (even? (/ address 12))
    [(get-op (subvec image (+ address 8) (+ address 8 4)))
     (to-double (subvec image address (+ address 8)))]
    [(get-op (subvec image address (+ address 4)))
     (to-double (subvec image (+ address 4) (+ address 12)))]))
	  
(defn read-data
  "Read in the data from the image and return a series of decoded instructions"
  [image]
  (map (fn [x] (get-instruction-data image x)) (range 0 (count image) 12)))

;;; Physics functions
(def G 6.6428e-11)

(def mass-earth 6e24)

(def radius-earth 6.357e6)

(defn distance
  "Distance between two bodies"
  [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2) dy (- y1 y2)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

;;; Virtual machine executing instructions
(defn vector-refs
  "Create a vector of references, initialized to zero"
  [n]
  (into [] (take n (repeatedly #(ref 0)))))

(defn init-vm
  [data]
  (let [memory (vector-refs (count data))]
    (dosync
     (doall
      (map (fn [r v] (ref-set r v)) memory data)))
    (struct virtualmachine memory (ref 0) (vector-refs 16384) (vector-refs 16384) (ref false))))

(defn hohmann-trace
  [vm]
  (let [x (:outport vm)
	pc @(:counter vm)
	score @(x 0)
	fuel-remaining @(x 1)
	sx-relative @(x 2)
	sy-relative @(x 3)
	target-radius @(x 4)]
    nil))
    ;(println (format "%s: %s,%s,%s,%s,%s" pc score fuel-remaining sx-relative sy-relative target-radius))))
    
(defn hohmann-input 
  [c vm]
  (println "Running with config: " c)
  (dosync
   (ref-set ((:inport vm) 0x3E80) c)))

(defn run-machine
  "Run the virtual machine with the decoded instructions"
  [instructions tracer init-input]
  (let [vm (init-vm (map second instructions))]
    (init-input vm)
    (doseq [instruction instructions]     
      (tracer vm)
       (let [[op args] (first instruction)]
	 (apply op (list vm args))
	 (dosync
	  (alter (:counter vm) inc)))))
    nil)
	 
(ns uk.co.fatvat.icfp
  (:import [java.lang.reflect Array])
  (:import [java.io FileInputStream File]))

;;; Location of relevant files
(def bin1 "/home/jfoster/clojureprojects/icfp/uk/co/fatvat/bin1.obf")

;;; Boring static definitions
(def d-type-instructions {1 'Add, 2 'Sub, 3 'Mult, 4 'Div, 5 'Output, 6 'Phi})
(def s-type-instructions {0 'Noop, 1 'Cmpz, 2 'Sqrt, 3 'Copy, 4 'Input})
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
  (if (= 'Cmpz op)
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
  [image pc]
  (map (fn [x] (get-instruction-data image x)) (range 0 (count image) 12)))

;;; Physics functions
(def G 6.6428e-11)

(defn distance
  "Distance between two bodies"
  [[x1 y1] [x2 y2]]
  (let [dx (- x1 x2) dy (- y1 y2)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

;;; Virtual machine executing instructions

(defstruct virtualmachine :mem :counter :inport :outport :status)

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

(defn numeric-op
  "General numeric op"
  [vm args f]
  (dosync
   (let [m (:mem vm)]
     (ref-set (m @(:counter vm)) (f @(m (first args)) @(m (second args)))))))  

(defn phi
  [vm args]
  (let [m (:mem vm)]
    (dosync
     (ref-set (m @(:counter vm))
	      (if @(:status vm)
		@(m (first args))
		@(m (second args)))))))
    
(defn add
  "Add instruction"
  [vm args]
  (numeric-op vm args +))

(defn sub
  "Sub instruction"
  [vm args]
  (println args)
  (numeric-op vm args -))

(defn mult
  "Multiply instruction"
  [vm args]
  (numeric-op vm args *))

(defn div
  "Divide"
  [vm args]
  (numeric-op vm args (fn [x y] (if (zero? y) 0 (/ x y)))))

(defn noop
  "Noop instruction"
  [vm args]
  vm)

(defn copy
  "Copy instruction"
  [vm args]
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) @((:mem vm) (first args)))))

(defn sqrt
  "Square root instruction: undefined for negative values"
  [vm args]
  (assert (not (neg? @((:mem vm) (first args)))))
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) (Math/sqrt @((:mem vm) (first args))))))

(defn input
  "Input instruction: Set the memory on the inport"
  [vm args]
  (dosync
   (ref-set ((:mem vm) @(:counter vm)) @((:inport vm) @(:counter vm)))))

(defn output
  "Output instruction: Set the memory on the outport"
  [vm args]
  (let [m (:mem vm)
	o ((:outport vm) (first args))]
    (dosync
     (ref-set o @(m (second args))))))

(defn cmpz
  [vm args]
  (let [cmp (first args)
	val @((:mem vm) (second args))
	status
	(cond 
	  (= cmp 'LTZ) (< val 0)
	  (= cmp 'LEZ) (<= val 0)
	  (= cmp 'EQZ) (zero? val)
	  (= cmp 'GEZ) (> val 0)
	  (= cmp 'GTZ) (>= val 0)
	  :else (assert false))]
    (dosync
     (ref-set (:status vm) status))))

	  
;; TODO turn the symbols into the functions.
(defn run-machine
  "Run the virtual machine with the decoded instructions"
  [instructions]
  (let [vm (init-vm (map second instructions))]
    (doseq [instruction instructions]
      (let [[op args] (first instruction) data (second instruction)]
	(cond 
	  (= op 'Noop) (noop vm args)
	  (= op 'Cmpz) (cmpz vm args) 
	  (= op 'Sqrt) (sqrt vm args) 
	  (= op 'Copy) (copy vm args)
	  (= op 'Input) (input vm args)
	  (= op 'Add) (add vm args)
	  (= op 'Sub) (sub vm args)
	  (= op 'Mult) (mult vm args) 
	  (= op 'Div) (div vm args)
	  (= op 'Output) (output vm args)
	  (= op 'Phi) (phi vm args)
	  :else (assert false)))
      (dosync
       (alter (:counter vm) inc)))
    vm))
      
    

  

	   
	
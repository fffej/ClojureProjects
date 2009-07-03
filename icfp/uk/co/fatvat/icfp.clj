;; jeff.foster@acm.org
(ns uk.co.fatvat.icfp
  (:use clojure.contrib.trace)
  (:import [java.lang.reflect Array])
  (:import [java.io FileInputStream File]))

;;; Location of relevant files
(def bin1 "/home/jfoster/clojureprojects/icfp/uk/co/fatvat/bin1.obf")

;;; Virtual machine specification
(defstruct virtualmachine :mem :counter :inport :outport :status :firstrun :user)

(defn increment-counter
  [vm]
  (assoc vm :counter (inc (:counter vm))))

(defn memory-put
  "Update the memory specified by the key with the address to the given value"
  [vm key addr val]
  (assoc vm key (assoc (key vm) addr val)))

(defn memory-read
  "Return the value in key at address"
  [vm key addr]
  ((key vm) addr))

(defn mem-read
  [vm addr]
  ((:mem vm) addr))

(defn numeric-op
  "D-type General numeric op"
  [vm [x y] f]
  (memory-put vm :mem (:counter vm) (f (mem-read vm x) (mem-read vm y))))

(defn phi
  "D-type"
  [vm [x y]]
  (let [m (:mem vm)]
    (trace 'Phi (format "%s ? %s : %s --> %s" (:status vm) (m x) (m y) (if (:status vm) (m x) (m y))))
    (memory-put vm :mem (:counter vm)
	     (if (:status vm) (m x) (m y)))))

(defn print-args
  [vm op x y]
  (format "%s %s // %s %s %s" x y (mem-read vm x) op (mem-read vm y)))
    
(defn add
  "D-type Add instruction"
  [vm [x y]]
  (trace 'Add (print-args vm '+ x y))
  (numeric-op vm [x y] +))

(defn sub
  "D-type Sub instruction"
  [vm [x y]]
  (trace 'Sub (print-args vm '- x y))
  (numeric-op vm [x y] -))

(defn mult
  "D-type Multiply instruction"
  [vm [x y]]
  (trace 'Mult (print-args vm '* x y))
  (numeric-op vm [x y] *))

(defn div
  "D-type Divide"
  [vm args]
  (trace 'Div)
  (numeric-op vm args (fn [x y] (if (zero? y) 0 (/ x y)))))

(defn noop
  "S-type Noop instruction"
  [vm args]
  (trace 'Noop)
  vm)

(defn copy
  "S-Type: Copy instruction"
  [vm [x]]
  (trace 'Copy (format "%s // %s" x (mem-read vm x)))
  (memory-put vm :mem (:counter vm) (mem-read vm x)))

(defn sqrt
  "S-Type: Square root instruction: undefined for negative values"
  [vm [x]]
  (trace 'Sqrt)
  (assert (not (neg? (mem-read vm x))))
  (memory-put vm :mem (:counter vm) (Math/sqrt (mem-read vm x))))

(defn input
  "S-Type: Set the memory from the inport"
  [vm [x]]
  (trace 'Input)
  (memory-put vm :mem (:counter vm) (memory-read vm :inport x)))

(defn output
  "Output instruction: Set the memory on the outport"
  [vm [x y]]
  (trace 'Output (format "%s %s // %s" x y (mem-read vm y)))
  (memory-put vm :outport x (mem-read vm y)))

(defn cmpz
  "Comparison function"
  [vm [cmp y]]
  (let [val (mem-read vm y)
	status (cond ;; TODO replace this with functions so it becomes (apply cmp val)
		 (= cmp 'LTZ) (< val 0)
		 (= cmp 'LEZ) (<= val 0)
		 (= cmp 'EQZ) (zero? val)
		 (= cmp 'GEZ) (> val 0)
		 (= cmp 'GTZ) (>= val 0)
		 :else (assert false))]
    (trace 'Cmpz (format "%s %s --> %s" cmp y status))
    (assoc vm :status status)))

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
    [(bit-and (to-int ins) 0x00003FFF) (bit-shift-right (bit-and (last (butlast ins)) 0xF0) 4)]))

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
    [(get-op (subvec image (+ address 8) (+ address 12)))
     (to-double (subvec image address (+ address 8)))]
    [(get-op (subvec image address (+ address 4)))
     (to-double (subvec image (+ address 4) (+ address 12)))]))
	  
(defn read-data
  "Read in the data from the image and return a series of decoded instructions"
  [image]
  (map (fn [x] (get-instruction-data image x)) (range 0 (count image) 12)))

;;; Virtual machine executing instructions
(defn init-vm
  "Create a new VM with the initial data segment"
  [data]
  (struct virtualmachine (vec data) 0 (vec (repeat 16384 0)) (vec (repeat 16384 0)) false true []))

(defn hohmann-score
  "Return information about the outport sensors for the Hohmann example"
  [vm]
  (println (:counter vm))
  (let [x (:outport vm)
	pc (:counter vm)
	score (x 0)
	fuel-remaining (x 1)
	sx-relative (x 2)
	sy-relative (x 3)
	target-radius (x 4)]
    [pc score fuel-remaining sx-relative sy-relative target-radius]))
    
(defn hohmann-updater 
  "Given the state of the virtual machine, determine what to boost and return
   the new state of the virtual machine"
  [vm]
  (memory-put vm :inport 0x3E80 1001))

(defn create-vm
  [instructions]
  (init-vm (map second instructions)))

(def bin1 (read-data (get-bytes bin1)))

(defn run-machine
  "Run the virtual machine with the decoded instructions."
  [vm ops update-input]
  (reduce 
   (fn [v [op args]] 
     (increment-counter (op v args)))
   (update-input vm)
   ops))

; [0 0.0 10000.0 -6556995.342902722 7814.932738513376 4.2164E7]
(defn run []
  (let [x (create-vm bin1)
	ops (map first bin1)]
    (time (hohmann-score (last (take 1000 (repeatedly #(run-machine x ops hohmann-updater))))))))

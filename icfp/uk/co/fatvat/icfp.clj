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

(defstruct virtualmachine :mem :inport :outport :status)

(defn vector-refs
  "Create a vector of references, initialized to zero"
  [n]
  (into [] (take n (repeatedly #(ref 0)))))

(defn init-vm
  []
  (struct virtualmachine (vector-refs 16384) (vector-refs 16384) (vector-refs 16384) false))

(defn run-machine
  "Run the virtual machine with the decoded instructions"
  [instructions]
  (let [vm (init-vm)]
    (doseq [instruction instructions]
      (let [[op args] (first instruction) data (second instruction)]
	(cond 
	  (= op 'Noop) (println "noop" args)
	  (= op 'Cmpz) (println "cmpz" args)
	  (= op 'Sqrt)  (println "sqrt" args)
	  (= op 'Copy) (println "copy" args)
	  (= op 'Input) (println "input" args)
	  (= op 'Add) (println "add" args)
	  (= op 'Sub) (println "sub" args)
	  (= op 'Mult) (println "mult" args)
	  (= op 'Div) (println "div" args)
	  (= op 'Output) (println "output" args)
	  (= op 'Phi) (println "phi" args)
	  :else (assert false))))))
      
    

  

	   
	
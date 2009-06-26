(ns uk.co.fatvat.icfp
  (:import [java.lang.reflect Array])
  (:import [java.io FileInputStream File]))

;;; Location of relevant files
(def bin1 "/home/jfoster/clojureprojects/icfp/uk/co/fatvat/bin1.obf")

;;; Boring static definitions
(def d-type-instructions {1 'Add, 2 'Sub, 3 'Mult, 4 'Div, 5 'Output, 6 'Phi})
(def s-type-instructions {0 'Noop, 1 'Cmpz, 2 'Sqrt, 3 'Copy, 4 'Input})
(def comparison {0 'LTZ, 1 'LEZ, 2 'EQZ, 3 'GEZ, 4 'GTZ})
(def header 0xCAFEBABE)
(def team-id 125)

(def op-and 1879048192)

;;; Define what the virtual memory looks like
(defstruct vm :data-memory :instruction-memory :programcounter :status)

(defn get-bytes
  "Read the bytes for the given file, stored as a sequence of bytes"
  [filename]
  (let [file (File. filename)]
    (assert (.exists file))
    (with-open [x (FileInputStream. file)]
      (doall
       (into [] (take-while (partial not= -1) (repeatedly #(.read x))))))))

(defn get-op-code
  [bytes]
  (println "bytes" bytes)
  (bit-shift-right (bit-and (byte (first bytes)) 0xF0) 4))

(defn to-double
  [bytes]
  (Double/longBitsToDouble 
   (long
    (reduce bit-or 
	    (map (fn [x shift] (bit-shift-left (bit-and (int x) 0xFF) shift))
		 bytes 
		 (range 0 64 8))))))

(defn to-int
  [bytes]
  (int (reduce bit-or
	       (map (fn [x shift] (bit-shift-left (bit-and (int x) 0xFF) shift))
		    bytes
		    (range 0 32 8)))))

(defn get-op-code
  [ins]
  (let [d-opcode (bit-shift-right (bit-and 0xF0 (last ins)) 4)
	s-opcode (bit-and 0x0F (last ins))]
    (if (zero? d-opcode)
      (s-type-instructions s-opcode)
      (d-type-instructions d-opcode))))

(defn decode-instruction
  [ins]
  (let [opcode (get-op-code ins)]
    [opcode]))

(defn get-instruction-data
  [image address]
  (if (even? (/ address 12))
    [(decode-instruction (subvec image (+ address 8) (+ address 8 4)))
     (to-double (subvec image address (+ address 8)))]

    [(decode-instruction (subvec image address (+ address 4)))
     (to-double (subvec image (+ address 4) (+ address 12)))]))
	  
(defn read-data
  [image pc]
  (println "Program counter: " (/ pc 12))
  (if (>= pc 144) ;(count image))
    nil
    (let [ins (get-instruction-data image pc)]
      (println "Instruction: =" ins)
      (recur image (+ pc 12)))))
  
	   
	
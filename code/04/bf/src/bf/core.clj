(ns bf.core
  (:gen-class))

(defn new-machine [instructions]
  {:instr-pointer 0
   :cell-pointer 0
   :cells [0]
   :errored false
   :instructions (into [] instructions)})

(defn finished? [{errored? :errored, instr-pointer :instr-pointer, instructions :instructions :as machine}]
  (or errored? (>= instr-pointer (count instructions))))

(defn get-current-instruction [{instr-pointer :instr-pointer, instructions :instructions}]
  (get instructions instr-pointer))

(defn get-current-cell [{cells :cells, cell-pointer :cell-pointer}]
  (get cells cell-pointer))

(defn set-current-cell [{cells :cells, cell-pointer :cell-pointer :as machine} new-value]
  (let [new-cells (assoc cells cell-pointer new-value)]
    (assoc machine :cells new-cells)))

(defn inc-current-cell [machine]
  (let [current-cell (get-current-cell machine)]
    (set-current-cell machine
                      (if (>= current-cell 255)
                        0
                        (inc current-cell)))))

(defn dec-current-cell [machine]
  (let [current-cell (get-current-cell machine)]
    (set-current-cell machine
                      (if (<= current-cell 0)
                        255
                        (dec current-cell)))))

(defn move-left [{cell-pointer :cell-pointer :as machine}]
  (if (<= cell-pointer 0)
    (assoc machine :errored true)
    (assoc machine :cell-pointer (dec cell-pointer))))

(defn move-right [{cells :cells, cell-pointer :cell-pointer :as machine}]
  (let [new-cell-pointer (inc cell-pointer)
        new-machine (assoc machine :cell-pointer new-cell-pointer)]
    (if (>= new-cell-pointer (count cells))
      (assoc new-machine :cells (conj cells 0))
      new-machine)))

(defn next-instruction [{instr-pointer :instr-pointer :as machine}]
  (assoc machine :instr-pointer (inc instr-pointer)))

(defn previous-instruction [{instr-pointer :instr-pointer :as machine}]
  (if (<= instr-pointer 0)
    (assoc machine :errored true)
    (assoc machine :instr-pointer (dec instr-pointer))))

(defn end-while
  ([machine] (end-while (previous-instruction machine) 0))
  ([{errored? :errored :as machine} depth]
   (cond
     errored? machine
     (= (get-current-instruction machine) \]) (recur (previous-instruction machine) (inc depth))
     (= (get-current-instruction machine) \[) (if (= 0 depth) machine (recur (previous-instruction machine) (dec depth)))
     true (recur (previous-instruction machine) depth))))

(defn scan-to-close
  ([machine] (scan-to-close (next-instruction machine) 0))
  ([{errored? :errored :as machine} depth]
   (cond
     errored? machine
     (finished? machine) (assoc machine :errored true)
     (= (get-current-instruction machine) \[) (recur (next-instruction machine) (inc depth))
     (= (get-current-instruction machine) \]) (if (= 0 depth) machine (recur (next-instruction machine) (dec depth)))
     true (recur (next-instruction machine) depth))))

(defn start-while [machine]
  (if (= 0 (get-current-cell machine))
    (next-instruction (scan-to-close machine))
    (next-instruction machine)))

(defn print-current-cell [machine]
  (let [cell (get-current-cell machine)]
    (do
      (.write *out* cell)
      (when (= 10 cell) (flush))
      machine)))

(defn read-current-cell [machine]
  (let [input-byte (.read *in*)]
    (if (= input-byte -1)
      machine
      (set-current-cell machine (.read *in*)))))

(defn step [machine]
  (case (get-current-instruction machine)
    \< (next-instruction (move-left machine))
    \> (next-instruction (move-right machine))
    \+ (next-instruction (inc-current-cell machine))
    \- (next-instruction (dec-current-cell machine))
    \] (end-while machine)
    \[ (start-while machine)
    \. (next-instruction (print-current-cell machine))
    \, (next-instruction (read-current-cell machine))
    (next-instruction machine) ; Anything else is ignored
    ))

(defn run [machine]
  (if (finished? machine)
    machine
    (recur (step machine))))

(defn -main
  "Run a brainfuck program"
  [& args]
  (println (run (new-machine (slurp (first args))))))

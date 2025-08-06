(ns delta-il-to-ld-converter.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; ==========================================
;; Data Structures
;; ==========================================

(defrecord Instruction [opcode operand line-number])
(defrecord LadderElement [type operand row column connections])
(defrecord LadderRung [elements output])

;; ==========================================
;; IL Instruction Parser
;; ==========================================

(def instruction-types
  #{:LD :LDI :AND :ANI :OR :ORI :OUT :SET :RST
    :LDP :LDF :ANDP :ANDF :ORP :ORF :ANB :ORB
    :MPS :MRD :MPP :MC :MCR :END :NOP})

(defn parse-line
  "Parse a single IL instruction line"
  [line line-number]
  (let [trimmed (str/trim line)
        parts (str/split trimmed #"\s+")]
    (when (and (seq parts) (not (str/starts-with? trimmed ";")))
      (let [opcode (keyword (str/upper-case (first parts)))
            operand (str/join " " (rest parts))]
        (when (instruction-types opcode)
          (->Instruction opcode operand line-number))))))

(defn parse-il
  "Parse IL code into instruction sequence"
  [il-code]
  (->> (str/split-lines il-code)
       (map-indexed (fn [idx line] (parse-line line (inc idx))))
       (filter some?)
       vec))

;; ==========================================
;; Stack-based IL Interpreter
;; ==========================================

(defrecord ILState [stack rungs current-rung branch-stack])

(defn create-contact
  "Create a ladder contact element"
  [type operand normally-closed?]
  {:type (if normally-closed? :nc-contact :no-contact)
   :operand operand
   :connections {:left nil :right nil}})

(defn create-coil
  "Create a ladder coil element"
  [operand type]
  {:type type
   :operand operand
   :connections {:left nil}})

;; ==========================================
;; IL to Ladder Logic Conversion
;; ==========================================

(defmulti process-instruction
  "Process individual IL instructions"
  (fn [state instruction] (:opcode instruction)))

(defmethod process-instruction :LD
  [state {:keys [operand]}]
  (-> state
      (update :stack conj {:type :series
                           :elements [(create-contact :input operand false)]
                           :start-new-rung true})))

(defmethod process-instruction :LDI
  [state {:keys [operand]}]
  (-> state
      (update :stack conj {:type :series
                           :elements [(create-contact :input operand true)]
                           :start-new-rung true})))

(defmethod process-instruction :AND
  [state {:keys [operand]}]
  (let [top (peek (:stack state))
        contact (create-contact :input operand false)]
    (if top
      (-> state
          (update :stack pop)
          (update :stack conj 
                  (if (= (:type top) :series)
                    (update top :elements conj contact)
                    {:type :series
                     :elements [top contact]})))
      state)))

(defmethod process-instruction :ANI
  [state {:keys [operand]}]
  (let [top (peek (:stack state))
        contact (create-contact :input operand true)]
    (if top
      (-> state
          (update :stack pop)
          (update :stack conj 
                  (if (= (:type top) :series)
                    (update top :elements conj contact)
                    {:type :series
                     :elements [top contact]})))
      state)))

(defmethod process-instruction :OR
  [state {:keys [operand]}]
  (let [top (peek (:stack state))
        contact (create-contact :input operand false)
        new-branch {:type :series :elements [contact]}]
    (if top
      (-> state
          (update :stack pop)
          (update :stack conj 
                  (if (= (:type top) :parallel)
                    (update top :branches conj new-branch)
                    {:type :parallel
                     :branches [top new-branch]})))
      state)))

(defmethod process-instruction :ORI
  [state {:keys [operand]}]
  (let [top (peek (:stack state))
        contact (create-contact :input operand true)
        new-branch {:type :series :elements [contact]}]
    (if top
      (-> state
          (update :stack pop)
          (update :stack conj 
                  (if (= (:type top) :parallel)
                    (update top :branches conj new-branch)
                    {:type :parallel
                     :branches [top new-branch]})))
      state)))

(defmethod process-instruction :OUT
  [state {:keys [operand]}]
  (let [top (peek (:stack state))]
    (if top
      (-> state
          (update :stack pop)
          (update :rungs conj {:logic top
                               :output (create-coil operand :normal-coil)}))
      state)))

(defmethod process-instruction :SET
  [state {:keys [operand]}]
  (let [top (peek (:stack state))]
    (if top
      (-> state
          (update :stack pop)
          (update :rungs conj {:logic top
                               :output (create-coil operand :set-coil)}))
      state)))

(defmethod process-instruction :RST
  [state {:keys [operand]}]
  (let [top (peek (:stack state))]
    (if top
      (-> state
          (update :stack pop)
          (update :rungs conj {:logic top
                               :output (create-coil operand :reset-coil)}))
      state)))

(defmethod process-instruction :ANB
  [state _]
  (let [stack (:stack state)]
    (if (>= (count stack) 2)
      (let [block2 (peek stack)
            stack' (pop stack)
            block1 (peek stack')
            stack'' (pop stack')
            combined {:type :series
                     :elements (vec (concat 
                                   (if (= (:type block1) :series) (:elements block1) [block1])
                                   (if (= (:type block2) :series) (:elements block2) [block2])))}]
        (assoc state :stack (conj stack'' combined)))
      state)))

(defmethod process-instruction :ORB
  [state _]
  (let [stack (:stack state)]
    (if (>= (count stack) 2)
      (let [block2 (peek stack)
            stack' (pop stack)
            block1 (peek stack')
            stack'' (pop stack')
            combined {:type :parallel
                     :branches [block1 block2]}]
        (assoc state :stack (conj stack'' combined)))
      state)))

(defmethod process-instruction :MPS
  [state _]
  (let [top (peek (:stack state))]
    (if top
      (update state :branch-stack conj top)
      state)))

(defmethod process-instruction :MRD
  [state _]
  (let [branch-top (peek (:branch-stack state))]
    (if branch-top
      (update state :stack conj branch-top)
      state)))

(defmethod process-instruction :MPP
  [state _]
  (let [branch-top (peek (:branch-stack state))]
    (if branch-top
      (-> state
          (update :branch-stack pop)
          (update :stack conj branch-top))
      state)))

(defmethod process-instruction :default
  [state _]
  state)

;; ==========================================
;; Ladder Diagram Renderer
;; ==========================================

(defn render-element
  "Render a ladder element as ASCII art"
  [element]
  (case (:type element)
    :no-contact (str "-[" (:operand element) "]-")
    :nc-contact (str "-[/" (:operand element) "]-")
    :normal-coil (str "-(" (:operand element) ")")
    :set-coil (str "-(S " (:operand element) ")")
    :reset-coil (str "-(R " (:operand element) ")")
    ""))

(defn render-series
  "Render series connected elements"
  [elements]
  (str/join "-" (map render-element elements)))

(declare render-logic)

(defn render-parallel
  "Render parallel branches"
  [branches]
  (let [rendered-branches (map render-logic branches)]
    (str "++" 
         (str/join (str "\n     +") 
                   (map #(str "-" % "-") rendered-branches))
         "\n     ++")))

(defn render-logic
  "Render logic structure recursively"
  [logic]
  (case (:type logic)
    :series (render-series (:elements logic))
    :parallel (render-parallel (:branches logic))
    (render-element logic)))

(defn render-rung
  "Render a complete ladder rung"
  [rung index]
  (let [logic-str (if (:logic rung)
                    (render-logic (:logic rung))
                    "")
        output-str (if (:output rung)
                    (render-element (:output rung))
                    "")
        lines (str/split-lines (str logic-str))]
    (if (= (count lines) 1)
      (str (format "%03d: |" index) logic-str output-str "|\n")
      (str (format "%03d: |" index) (first lines) output-str "|\n"
           (str/join "\n" (map #(str "     |" % "|") (rest lines)))
           "\n"))))

(defn render-ladder
  "Render complete ladder diagram"
  [rungs]
  (let [header "=== LADDER DIAGRAM ===\n"
        power-rails "     +----[POWER RAIL]----+\n"
        rung-strs (map-indexed #(render-rung %2 (inc %1)) rungs)
        footer "     +----[END RAIL]------+"]
    (str header
         power-rails
         (str/join "\n" rung-strs)
         footer)))

;; ==========================================
;; Main Conversion Function
;; ==========================================

(defn convert-il-to-ld
  "Convert IL code to Ladder Diagram"
  [il-code]
  (let [instructions (parse-il il-code)
        initial-state (->ILState [] [] nil [])
        final-state (reduce process-instruction initial-state instructions)
        ladder-diagram (render-ladder (:rungs final-state))]
    {:instructions instructions
     :rungs (:rungs final-state)
     :diagram ladder-diagram
     :warnings (when (seq (:stack final-state))
                ["Warning: Unprocessed stack elements remain"])}))

;; ==========================================
;; Validation Functions
;; ==========================================

(defn validate-il-code
  "Validate IL code for common errors"
  [il-code]
  (let [instructions (parse-il il-code)
        errors []]
    (cond
      (empty? instructions)
      (conj errors "Error: No valid instructions found")
      
      (not-any? #(#{:OUT :SET :RST} (:opcode %)) instructions)
      (conj errors "Warning: No output instructions found")
      
      :else errors)))

;; ==========================================
;; Example Usage
;; ==========================================

(defn example-simple []
  (let [il-code "LD X0
OUT Y0"]
    (println "Simple Example:")
    (println "IL Code:")
    (println il-code)
    (println "\nLadder Diagram:")
    (println (:diagram (convert-il-to-ld il-code)))))

(defn example-series []
  (let [il-code "LD X0
AND X1
AND X2
OUT Y0"]
    (println "\nSeries Connection Example:")
    (println "IL Code:")
    (println il-code)
    (println "\nLadder Diagram:")
    (println (:diagram (convert-il-to-ld il-code)))))

(defn example-parallel []
  (let [il-code "LD X0
OR X1
OUT Y0"]
    (println "\nParallel Connection Example:")
    (println "IL Code:")
    (println il-code)
    (println "\nLadder Diagram:")
    (println (:diagram (convert-il-to-ld il-code)))))

(defn example-complex []
  (let [il-code "LD X0
AND X1
OR X2
OUT Y0
LD X3
ANI X4
OUT Y1
LDI X5
AND X6
SET Y2
LD X7
RST Y2"]
    (println "\nComplex Example:")
    (println "IL Code:")
    (println il-code)
    (println "\nLadder Diagram:")
    (let [result (convert-il-to-ld il-code)]
      (println (:diagram result))
      (when (:warnings result)
        (println "\nWarnings:")
        (doseq [w (:warnings result)]
          (println " -" w))))))

(defn run-all-examples []
  (example-simple)
  (println "\n" (str/join "" (repeat 40 "=")) "\n")
  (example-series)
  (println "\n" (str/join "" (repeat 40 "=")) "\n")
  (example-parallel)
  (println "\n" (str/join "" (repeat 40 "=")) "\n")
  (example-complex))

;; Advanced features for complex ladder logic

(defn optimize-rungs
  "Optimize ladder rungs by combining common elements"
  [rungs]
  ;; Implementation for optimization logic
  rungs)

(defn analyze-cross-references
  "Analyze cross-references between rungs"
  [rungs]
  (let [outputs (set (map #(get-in % [:output :operand]) rungs))
        inputs (mapcat (fn [rung]
                        (tree-seq map? vals (:logic rung)))
                      rungs)
        input-operands (set (keep :operand inputs))]
    {:outputs outputs
     :inputs input-operands
     :cross-refs (set/intersection outputs input-operands)}))

;; ==========================================
;; File I/O Functions
;; ==========================================

(defn read-il-file
  "Read IL code from a file"
  [filepath]
  (slurp filepath))

(defn write-ladder-file
  "Write ladder diagram to a file"
  [filepath ladder-text]
  (spit filepath ladder-text))

(defn convert-file
  "Convert IL file to ladder diagram file"
  [input-file output-file]
  (let [il-code (read-il-file input-file)
        result (convert-il-to-ld il-code)]
    (write-ladder-file output-file (:diagram result))
    (println "Conversion complete!")
    (println "Input:" input-file)
    (println "Output:" output-file)
    (when (:warnings result)
      (println "Warnings:" (:warnings result)))
    result))

;; ==========================================
;; Main entry point
;; ==========================================

(defn -main
  "Main entry point for command line usage"
  [& args]
  (cond
    (= (first args) "example")
    (run-all-examples)
    
    (= (count args) 2)
    (convert-file (first args) (second args))
    
    :else
    (do
      (println "Usage:")
      (println "  Run examples: lein run example")
      (println "  Convert file: lein run <input.il> <output.ld>")
      (println "\nRunning default examples...")
      (run-all-examples))))
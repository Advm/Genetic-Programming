(ns push307.core
  (:gen-class)
  (:require [clojure.java.jdbc :as j]))

(comment
  "NAME: Adam Valencia
   The instructions for setting up the dependencies neccesary to link your postgreSQL database to clojure
   can be found at this link ( https://www.youtube.com/watch?v=yjS0jHr_-2g&t=773s ). This will show you
   how to update your dependencies, create the link between your database and namespace, and finally create
   sample queryies to the database. The video starts at 4:15 and ends at 13:00. To actaully create the database,
   I followed the tutorial found here ( https://www.youtube.com/watch?v=yjS0jHr_-2g&t=773s ). Once I had
   the server running, I then downloaded a SQL client for mac called Postice found here 
   ( https://eggerapps.at/postico/ ). Of coure, if you have a GUI for SQL then you can use that. I used Postico
   to create the database I was going to use and then copied over the lines from an excel spreadsheet. This method
   was actually recomended in the Postico docs to import large amounts of data. You can find the rows of
   data I used here ( https://docs.google.com/spreadsheets/d/1hgJX5yagQAw6EhwkK49rsZGaKUP_ZJF1ojlZvYdEagw/edit?usp=sharing ).
   Feel free to message me on discord if anything doesn't work or if you have other questinos!")

(def gpdb {:dbtype "postgresql"
           :dbname "postgres"
           :host "localhost"})


(j/query gpdb ["SELECT * FROM gpdb WHERE age > 42"])
(def row (first (j/query gpdb ["SELECT * FROM gpdb LIMIT 1"])))
row
(def database-attributes
  (into [] (map name (keys (first (j/query gpdb ["SELECT * FROM gpdb LIMIT 1"]))))))
database-attributes

(def database-attribute-types
  (into {} (map #(assoc nil % (str (type %2))) (keys row) (vals row))))

(def operators
  ["="
   "<"
   ">"
   "<="
   ">="])
(nth operators 3)

;;;;;;;;;;
;; Examples
; An example Push state
(def example-push-state
  {:exec '(4 integer_+ integer_-)
   :integer '(3 6 3 9 8 7 6 1 2)
   :string '("abc" "def")
   :boolean '(true false false true)
   :where '()
   :input {:in1 4 :in2 6}})

; An example Plushy genome
(def example-plushy-genome
  '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close))

; An example Push program
; This is the program tha would result from the above Plushy genome
(def example-push-program
  '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-)))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:genome '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close)
   :program '(3 5 integer_* exec_dup ("hello" 4 "world" integer_-))
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

;Included Integers 1-9 for terminal nodes

(def instructions
  (list
  ;;  'integer_+
  ;;  'integer_-
  ;;  'integer_*
  ;;  'integer_%
   'exec_dup
   'close
   'integer_stack_depth
   'integer_swap
   'integer_rot
   'integer_dup
   'integer_ERC
   'string_length
   'string_stack_depth
   'boolean_from_integer
   'boolean_rand
   'condition_from_stack
  ;;  'condition_from_index
  ;;  'condition_distinct_from_index
   'AND
   'OR))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {'exec_dup 1})

;;;;;;;;;
;; Stack Utilities
(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :where '()})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (conj state {stack (conj (stack state) item)}))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (conj state {stack (rest (stack state))}))

(defn pop-items-from-stack
  "Removes the top n items from the stack"
  [state stack n]
  (loop
   [start state
    count 0]
    (if (= count n)
      start
      (recur (pop-stack start stack) (inc count)))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (if (empty? (stack state))
    true
    false))

(defn stack-depth
  "Returns the depth of a given stack given a state"
  [state stack]
  (count (stack state)))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty-stack? state stack)
    :no-stack-item
    (first (stack state))))

(defn shove-stack
  "Shoves item into a state in state at index i and returns the resulting state"
  [state stack item i]
  (assoc state stack
         (concat
          (take i (stack state))
          (list item)
          (take-last (- (stack-depth state stack) i) (stack state)))))

(defn yank-stack
  "Pulls an item DEEP from the stack and pushes it onto the top of the given stack"
  [state stack i]
  (if (< i (stack-depth state stack))
    (let [item (nth (stack state) i)]
      (assoc state stack
             (concat
              (list item)
              (take i (stack state))
              (take-last (- (stack-depth state stack) (inc i)) (stack state)))))
    state))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map with keys {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

(defn query-for-random-row
  "A Utility function that returns a random row from the postgreSQL database
   NOTE: 50 is the list of rows in the database. If time allows will replace with dynamic values"
  [num-rows]
  (take num-rows (j/query gpdb ["SELECT * FROM gpdb OFFSET floor(random() * 50) LIMIT ?;" num-rows])))

;;;;;;;;;
;; Integer Stack Instructions
(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the 
  second, the result pushed to the stack should be (second - first)."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (let [num (second (:integer state))
        denom (first (:integer state))]
    (cond
      ;; Checking for nil here since nil was being pushed onto integer stack. If no integers are presented we cannot error check for zero denominator
      ;; thus nil? was utilized
      (nil? num) (pop-stack state :exec)
      (nil? denom) (pop-stack state :exec)
      (= 0 denom) (push-to-stack (pop-stack (pop-stack state :integer) :integer) :integer num)
      :else (make-push-instruction state quot [:integer :integer] :integer))))

(defn integer_stack_depth
  "Pushes the length of the integer stack onto the integer stack and returns the
   reuslting state"
  [state]
  (push-to-stack state :integer (stack-depth state :integer)))

(defn integer_swap
  "Swaps the top two items on the integer stack"
  [state]
  (if (>= (stack-depth state :integer) 2)
    (let [new-state (pop-stack state :integer)]
      (shove-stack new-state :integer (first (:integer state)) 1))
    state))

(defn integer_rot
  "Rotates the top three items on the integer stack"
  [state]
  (yank-stack state :integer 2))

(defn integer_dup
  [state]
  (if (empty-stack? state :integer)
    state
    (push-to-stack state :integer (first (:integer state)))))

(defn integer_ERC
  "Pushes a random integer ERC onto the integer stack of a given state"
  [state]
  (let [r (rand-nth [10 100 1000 10000 100000])]
    (push-to-stack state :integer (rand-nth (range r)))))

;;;;;;;;;
;; String Stack Instructions
(defn string_length
  "Pushes the length of the top item on the string stack onto the integer stack"
  [state]
  (push-to-stack state :integer (count (first (:string state)))))

(defn string_stack_depth
  "Pushes the depth of the string stack onto the integer stack"
  [state]
  (push-to-stack state :integer (count (:string state))))

(defn exec_dup
  [state]
  (if (empty-stack? state :exec)
    state
    (push-to-stack state :exec (first (:exec state)))))

;;;;;;;;;
;; Boolean Stack Instructions
(defn boolean_from_integer
  "Pushes False onto the boolean stack if top integer is 0, true otheriswise"
  [state]
  (if (= 0 (peek-stack state :integer))
    (push-to-stack state :boolean false)
    (push-to-stack state :boolean false)))

(defn boolean_rand
  "Pushes a random boolean value onto the boolean stack"
  [state]
  (push-to-stack state :boolean (rand-nth [true false])))

;;;;;;;;;
;; Where Stack Instructions
database-attributes
(defn condition_from_stack
  "Condition clause makrers. Pops two integers off the integer stack to index an attribute and operator from the table.
   Depending on the attribute type, pushes a where clause to the :where stack that matches the datatype."
  [state]
  (if (< (stack-depth state :integer) 3)
    state
    (let [attribute_idx (mod (peek-stack state :integer) (count database-attributes))
          operator (operators (mod (peek-stack (pop-stack state :integer) :integer) (count operators)))
          condition_type ((nth (keys database-attribute-types) attribute_idx) database-attribute-types)
          new-state (pop-items-from-stack state :integer 2)]
      (cond
        (= "class java.lang.String" condition_type) (if (empty-stack? new-state :string)
                                                      state
                                                      (push-to-stack (pop-stack new-state :string) :where (str "(" "'" (peek-stack new-state :string) "'"
                                                                                                               operator
                                                                                                               (database-attributes attribute_idx) ")")))
        (= "class java.lang.Integer" condition_type) (push-to-stack (pop-stack new-state :integer) :where (str "(" (peek-stack new-state :integer)
                                                                                                               operator
                                                                                                               (database-attributes attribute_idx) ")"))
        (= "class java.lang.Boolean" condition_type) (if (empty-stack? new-state :boolean)
                                                       state
                                                       (push-to-stack (pop-stack new-state :boolean) :where (str "(" (peek-stack new-state :boolean)
                                                                                                                 operator
                                                                                                                 (database-attributes attribute_idx) ")")))
        :else (state)))))

(defn get-attribute-from-type
  "Returns a list of the attributes of the table that correspond to the given datatype"
  [type]
  (filter (comp #{type} database-attribute-types) (keys database-attribute-types)))

database-attributes
(defn condition_from_index
  "Same as condition from stack instead the value to compare is indexed from the table using an integer popped off the
   integer stack."
  [state]
  (if (< (stack-depth state :integer) 3)
    state
    (let [attribute_idx (mod (peek-stack state :integer) (count database-attributes))
          attribute_type ((nth (keys database-attribute-types) attribute_idx) database-attribute-types)
          attributes (get-attribute-from-type attribute_type)
          operator (operators (mod (peek-stack (pop-stack state :integer) :integer) (count operators)))
          rand-row (first (query-for-random-row 1))
          attribute-values-map (select-keys rand-row attributes)
          attribute-values-map-idx (mod (peek-stack (pop-items-from-stack state :integer 2) :integer) (count attribute-values-map))
          attribute-value (attribute-values-map (nth (keys attribute-values-map) attribute-values-map-idx))
          new-state (pop-items-from-stack state :integer 3)]
      (if (string? attribute-value)
        (push-to-stack new-state :where (str "(" (name (rand-nth attributes)) operator "'" attribute-value "'" ")"))
        (push-to-stack new-state :where (str "(" (name (rand-nth attributes)) operator attribute-value ")"))))))

(defn condition_distinct_from_index
  "Same as condition from stack instead the value to compare is indexed from a set of values from the table 
   that matches the attributes datatype."
  [state]
  (if (< (stack-depth state :integer) 3)
    state
    (let [attribute_idx (mod (peek-stack state :integer) (count database-attributes))
          attribute_type ((nth (keys database-attribute-types) attribute_idx) database-attribute-types)
          attributes (get-attribute-from-type attribute_type)
          attribute (name (rand-nth attributes))
          operator (operators (mod (peek-stack (pop-stack state :integer) :integer) (count operators)))
          rand-row (first (query-for-random-row 1))
          attribute-values-map (select-keys rand-row attributes)
          attribute-values-map-idx (mod (peek-stack (pop-items-from-stack state :integer 2) :integer) (count attribute-values-map))
          attribute-value (nth (into [] (set (flatten (map vals (j/query gpdb [(str "SELECT " attribute " FROM gpdb")]))))) attribute-values-map-idx)
          new-state (pop-items-from-stack state :integer 3)]
      (if (string? attribute-value)
        (push-to-stack new-state :where (str "(" attribute operator "'" attribute-value "'" ")"))
        (push-to-stack new-state :where (str "(" attribute operator attribute-value ")"))))))

(defn AND
  "ANDs two where clauses and pushes the contents back onto the stack"
  [state]
  (if (< (stack-depth state :where) 2)
    state
    (push-to-stack (pop-items-from-stack state :where 2) :where (str (apply str (butlast (peek-stack state :where)))
                                                                     " AND "
                                                                     (apply str (rest (peek-stack (pop-stack state :where) :where)))))))

(defn OR
  "ORs two where clauses and pushes the contents back onto the stack"
  [state]
  (if (< (stack-depth state :where) 2)
    state
    (push-to-stack (pop-items-from-stack state :where 2) :where (str (apply str (butlast (peek-stack state :where)))
                                                                     " OR "
                                                                     (apply str (rest (peek-stack (pop-stack state :where) :where)))))))

;;;;;;;;;
;; Interpreter
(defn unwrap-list
  "Given a nested-list, unwraps the lists onto a given state's exec stack and
   return the new state"
  [push-state list]
  (assoc push-state :exec (concat list (:exec push-state))))

(defn push-literal-to-stack
  "Given a literal pushes it onto the appropriate stack"
  [push-state next-instruction]
  (cond
    ;; More Literals could be used yet we are only dealing with integers. If more datatypes/stacks become available,
    ;; this list can easily be expanded
    (int? next-instruction) (push-to-stack push-state :integer next-instruction)
    (string? next-instruction) (push-to-stack push-state :string next-instruction)
    :else push-state))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Or, if the next element is a nested list, needs to unwrap that list onto
  the exec stack.
  Returns the new Push state."
  [push-state]
  (let [next-instruction (first (:exec push-state))]
    (cond
      (list? next-instruction) (unwrap-list (pop-stack push-state :exec) next-instruction)
      (fn? (eval next-instruction)) ((eval next-instruction) (pop-stack push-state :exec))
      :else (push-literal-to-stack (pop-stack push-state :exec) next-instruction))))

; This is the program tha would result from the above Plushy genome
(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit.
   Max Number of steps are 150"
  [program start-state]
  (loop
   [limit 0
    state (unwrap-list start-state program)]
    (cond
      (= limit 150) "Termination INITIATIED"
      (empty-stack? state :exec) state
      :else (recur (inc limit) (interpret-one-step state)))))

;;;;;;;;;
;; Translation from Plushy genomes to Push programs
(defn translate-plushy-to-push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opened-blocks
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opened-blocks %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else

;;;;;;;;;
;; GP
; An example Plushy genome
(def example-plushy-genome
  '(3 5 integer_* exec_dup "hello" 4 "world" integer_- close))

(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
  ;; Used (inc) to reduce the chance of a program having 0 instructions
  (repeatedly (inc (rand-int max-initial-plushy-size)) #(rand-nth instructions)))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  ;; We talked about this in class and I am so happy that I found a way to do tournament selection in one line
  (apply (partial min-key :total-error) (take 10 (shuffle population))))

(defn pareto-selection
  "Selects an individual from the population using a pareto-based selection method. Returned
  individual will be a parent in the next generation. Can use a fixed pareto size."
  [population]
  (let [paretoFront (list (first population))
        rem (rest population)]
    (loop
     [pareto paretoFront
      remainder rem]
      (if (empty? remainder)
        (nth pareto (quot (count pareto) 2))
        (if (< (count (:program (last paretoFront))) (count (:program (first remainder))))
          (recur pareto (rest remainder))
          (recur (concat pareto (first remainder)) (rest remainder)))))))


;;Returns all the "incorrect" invdividuals
;;This query returns all the values that are indicators of people at high risk for Heart attacks.
(defn target-function
  "Queries the database given the SQL statement and returns a list of maps of all the individuals who fit that SQL query"
  [query]
  (j/query gpdb [(str "SELECT * FROM gpdb WHERE " query)]))

(def query-answer "(tobacco = TRUE AND bp > 130 AND income < 40000 AND age >= 55) OR (bp > 150 AND income < 30000 AND age >= 60) OR (bp >= 180) OR (bp >= 160 AND age >= 65) OR (tobacco = TRUE AND bp > 160) OR (age >= 50 AND income < 20000 AND tobacco = TRUE)")
(def query-incorrect (j/query gpdb [(str "SELECT * FROM gpdb EXCEPT SELECT * FROM gpdb WHERE" query-answer)]))

(def incorrect-query (map #(assoc % :category 0) query-incorrect))
(count incorrect-query)
(def correct-query (map #(assoc % :category 1) (target-function query-answer)))
(count correct-query)

;populations contains an indviidual 
(defn lexicase-selection
  "Lexicase Parent Selection for an individual"
  [population]
  (loop
   [testCases (shuffle (concat incorrect-query correct-query))
    eligible population]
    (cond
      (= 1 (count eligible)) (first eligible)
      (empty? testCases) (if (empty? eligible) (rand-nth population) (rand-nth eligible))
      :else (let [testCase (first testCases)
                  survivors (filter (fn [x] (let [result (:results x)
                                                  category (:category testCase)]
                                              (cond
                                                ;;if nil? returns true then that means we could not find our testCase in the query results
                                                ;;if some? returns true, then that means that we found our testcase in the query results
                                                (and (some? (some #{testCase} result)) (= 0 category)) false
                                                (and (nil? (some #{testCase} result)) (= 0 category)) true
                                                (and (nil? (some #{testCase} result)) (= 1 category)) false
                                                (and (some? (some #{testCase} result)) (= 1 category)) true
                                              :else true)))
                                    eligible)]
              (recur (rest testCases) survivors)))))

(some #{10} '(1 2 3 4 5))
(dissoc {:a 2 :b 3 :c 3} :b)
(defn crossover
  "Crosses over two Plushy genomes (note: not individuals) using uniform crossover.
  Returns child Plushy genome."
  [prog-a prog-b]
  (map #(rand-nth [%1 %2]) prog-a prog-b))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the Plushy genomes) with some probability. Returns child Plushy genome."
  [prog instructions]
  (let [len (count prog)
        r (rand-nth [0 1])]
    (if (= r 0)
      prog
      (loop
       ;; A very aggresive way of doing this. This does not account for the possibility of something can not be added but I don't think that is the case.
       ;; I create a new list and append the new instruction before or after the current index of the program
       [p (into [] prog)
        p-uniform-addition []
        instruction (rand-nth instructions)
        i 0]
    ;; This is to make sure we only iterate as many items are in the list
        (if (= i len)
          p-uniform-addition
          (recur
           (rest p)
           (let [probability (rand-nth [0 1])]
             (cond
               (= probability 0) (conj p-uniform-addition instruction (first p))
               (= probability 1) (conj p-uniform-addition (first p) instruction)))
           (rand-nth instructions)
           (inc i)))))))

(defn uniform-deletion
  "Randomly deletes instructions from Plushy genomes at some rate. Returns
   child Plushy genome."
  [prog]
  ;; Cheeky
  (random-sample 0.8 prog))

(defn create-child-invididual
  "Given a plushy genome returns a new child individual"
  [program]
  ;; Did not include errors and error since this is accounted for in the reggression-error-function
  {:genome program
   :program (translate-plushy-to-push program)})

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions]
  (let [parent1 (tournament-selection population)
        ;;parent2 (tournament-selection population)
        ;parent3 (pareto-selection (sort-by :total-error population))
        ;;parent1 (lexicase-selection (shuffle population))
        parent2 (lexicase-selection (shuffle population))
        p (rand)]
    (cond
      (< p 0.5) (create-child-invididual (crossover (:genome parent1) (:genome parent2)))
      (< p 0.75) (create-child-invididual (uniform-addition (:genome parent1) instructions))
      (<= p 1.0) (create-child-invididual (uniform-deletion (:genome parent1))))))

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info"
  [population generation]
 (let [bestProgram (apply (partial min-key :total-error) population)]
    (println "-------------------------------------------------------")
    (println "               Report for Generation" (str generation))
    (println "-------------------------------------------------------")
    (println "Best Program:" (:program bestProgram))
    (println "Best Program Size:" (count (:program bestProgram)))
    (println "Best Total Error:" (:total-error bestProgram))
    (println "Best WHERE Clause: " (:clause bestProgram))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-plushy-size (max size of randomly generated Plushy genomes)"
  [{:keys [population-size max-generations error-function instructions max-initial-plushy-size]
    :as argmap}]
  (loop
   [gen 0
    ;; I map the error-function on the new populatino to make finding the best indivudal easier and less error prone.
    population (map (eval error-function) (map create-child-invididual (repeatedly population-size #(make-random-plushy-genome instructions max-initial-plushy-size))))]
    (report population gen)
    (cond
      (= 0 (:total-error (apply (partial min-key :total-error) population))) :SUCCESS
      (= gen max-generations) nil
      :else (recur (inc gen) (map error-function (repeatedly population-size #(select-and-vary population instructions)))))))

(defn precision-calculation
  "Returns the fitness score of the two queries"
  [query1]
  (let [positives (target-function query-answer)
        false_positives (count (filter (fn [x] (nil? (some #{x} positives))) query1))
        false_negatives (count (filter (fn [x] (nil? (some #{x} query1))) positives))]
    (+ false_positives false_negatives)))

(defn calculate-loss
  "Given a queries, returns the difference in correct queries"
  [q1]
  (let [gpQ (str "SELECT * FROM gpdb WHERE " q1)
        gp_Query (j/query gpdb [gpQ])]
    (if (= 0 (count gp_Query))
      10000      ;;Done to promote queries that actually return results as they will converge at 34 since an empty query is better than an actual query
      (precision-calculation gp_Query))))


(defn regression-error-function
  "Takes an individual and evaluates it on some test cases.
  This will need to translate each individual's Plushy genome into a Push
  program before executing the Push program (see translate-plushy-to-push).
  For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors. You may also want to set
  :program to be the Push program translated from the Plushy genome, though
  this isn't mandatory.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (let [program (interpret-push-program (:program individual) empty-push-state)]
    (if (empty-stack? program :where)
      ;; Supply a very high error to discourage this individual
      ;; Attach a "false" clase as an indication that this invidual does not create a meaningful querty
      (assoc individual :total-error 10000 :clause "false" :results '())
      (assoc individual :clause (peek-stack program :where)
             :total-error (calculate-loss (peek-stack program :where))
             :results (target-function (peek-stack program :where))))))

(comment (push-gp {:instructions instructions
          :error-function regression-error-function
          :max-generations 800
          :population-size 100
          :max-initial-plushy-size 50}))

(push-gp {:instructions instructions
          :error-function regression-error-function
          :max-generations 800
          :population-size 100
          :max-initial-plushy-size 50})

;; (clojure.stacktrace/e)

(comment "Currently, the fitness converges at 28 since there is a very high probability that this will return the most results. I need
          to promote quieres that look at more factors but in doing so, it increases the error. I will implement, in the testing, a function
          that temporarily ignores fitness to produce more intensive queries or somehow favors more complex where clauses.")
;;;;;;;;;;
;; The main function call
;; You can call this in a REPL, or alternatively from the command line
;; by running:
;;   lein run

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (binding [*ns* (the-ns 'push307.core)]
    ; The above line is necessary to allow `lein run` to work
    (push-gp {:instructions instructions
              :error-function regression-error-function
              :max-generations 800
              :population-size 200
              :max-initial-plushy-size 50})))
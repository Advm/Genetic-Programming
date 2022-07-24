(ns homework1.core)

;; Question 1
(def times-list
  '("03:05:32"
    "06:25:34"
    "06:35:28"
    "09:55:00"
    "15:22:01"
    "18:07:11"
    "20:12:18"
    "20:21:21"
    "22:00:34"
    "23:00:28"
    "00:05:40"
    "00:14:50"
    "02:54:51"
    "06:44:20"))

(defn convert-time-to-seconds
  "Given a string of the format HH:MM:SS returns the time in seconds"
  [time]
  (let [hours (subs time 0 2)
        mins (subs time 3 5)
        secs (subs time 6 8)]
    (+ (* (Integer/parseInt hours) 60 60) (* (Integer/parseInt mins) 60) (Integer/parseInt secs))))

(defn convert-seconds-to-time
  "Given number of seconds returns a string in the format HH:MM:SS"
  [seconds]
  (let [hours (quot seconds (* 60 60))
        mins (quot (- seconds (* hours 60 60)) 60)
        secs (- seconds (* 60 mins) (* hours 60 60))]
  
    ;I used str since format doesn't return for some reason. Therefore I wrapped it in str to have a return value of string type
    (str (format "%02d:%02d:%02d" hours mins secs))))

(def seconds-in-day (* 60 60 24))

(defn calculate-gap
  "Given two times in seconds calculates the difference between them assuming time2 succceeds time1"
  [time1 time2]
  (let [result (- time2 time1)]
    (if (< result 0)
      ;Adjusts time for the multiple days in a list.
      (- (+ time2 seconds-in-day) time1)
      result)))

(defn find-biggest-gap-time
  "Given a list of seconds calculates the biggest gap in consecutive times"
  [times-list-seconds]
  (loop
   [biggest 0
    remainder times-list-seconds]
    (if (empty? (rest remainder))
      (convert-seconds-to-time biggest)
      (recur (if (> (calculate-gap (first remainder) (first (rest remainder))) biggest)
               (calculate-gap (first remainder) (first (rest remainder)))
               biggest)
             (rest remainder)))))

(defn longest-time-gap
  "Given a list of times return the longest gap between two times"
  [times-list]
  (let [times-list-seconds (map convert-time-to-seconds times-list)]
    (find-biggest-gap-time times-list-seconds)))

(longest-time-gap times-list)

;; Question 2
(def scrabble-values
  {\a 1
   \b 3
   \c 3
   \d 2
   \e 1
   \f 4
   \g 2
   \h 4
   \i 1
   \j 8
   \k 5
   \l 1
   \m 3
   \n 1
   \o 1
   \p 3
   \q 10
   \r 1
   \s 1
   \t 1
   \u 1
   \v 4
   \w 4
   \x 8
   \y 4
   \z 10})

(def lower-case-letters
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defn score-letter
  "Returns the scrabble score for a letter."
  [letter]
  (get scrabble-values letter))

(defn rand-scrabble-word
  "Generates a random 7 letter scrabble word"
  []
  (apply str (take 7 (repeatedly #(rand-nth lower-case-letters)))))

(defn calculate-word-score
  "Given a scrabble word generates a map of the score and word"
  [word]
  {:word word :score (apply + (map score-letter word))})

(calculate-word-score (rand-scrabble-word))

(defn scrabble-population
  "Returns a list of 1000 scrabble word consiting of the word and score"
  []
  (take 1000 (repeatedly #(calculate-word-score (rand-scrabble-word)))))

(defn best-scrabble-words
  "Given a population of words returns a list of scrablle words with scores over 40"
  []
  (filter #(> (get % :score) 40) (scrabble-population)))

(best-scrabble-words)

;; ;; Question 3
(def string-population
  '("." " "  "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defn rand-word
  "Generates a random string of length L where L is the length of the word given"
  [l]
  (apply str (take l (repeatedly #(rand-nth string-population)))))

(rand-word 3)

(defn find-difference
  "Returns an integer that corresponds to the different number of characters of two strings of length L"
  [s1 s2]
  (loop [c1 s1
         c2 s2
         difference 0]
  (cond
    (empty? c1) difference
    :else (recur (rest c1) (rest c2) 
                   (if (not= (first c1) (first c2))
                     (inc difference)
                     difference)))))

(defn mutate-word
  "Given a string changes one char in the string and returns the result string."
  [word]
  (let [idx (rand-int (count word))
        first (subs word 0 idx)
        replacement (rand-nth string-population)
        end (subs word (+ idx 1))]
    (str first replacement end)))


(defn stochastic-hill-climber
  "Hill Climber for mutating a random string into a correct string."
  [iterations target-string current-string prev-diff]
  (loop [iteration iterations 
         target target-string 
         current current-string
         prev prev-diff]
  (let [mutated-string (mutate-word current)
        mutated-diff (find-difference target mutated-string)]
  (if (= mutated-diff 0)
    iteration
    (recur (inc iteration) target (if (<= prev mutated-diff) current mutated-string) (if (<= prev mutated-diff) prev mutated-diff))))))

;;These were created for testing purposes
(def target-word "Genetic Programming rules. Hillclimbing drools Adam is King")
target-word

(def random-string (rand-word (count target-word)))
random-string

(find-difference target-word random-string)

(stochastic-hill-climber 0 target-word random-string (find-difference target-word random-string))

(defn word-list
  "Generates a lazy sequence of random words of increaing length"
  []
  (map #(rand-word %) (range 1 90)))

(word-list)

(defn timed-stochastic-hill-climber
  "Times how long stochastic-hill-climber runs on words of length 0-100"
  [word-list]
  (loop [words word-list]
    (if (> (count words) 0)
    (let [word (first words) random-word
          (rand-word (count word))]
      ;Times how long it takes to find the target string of length L
      (time (stochastic-hill-climber 0 word random-word (find-difference word random-word)))))
    (if (empty? words)
      "Done"
      (recur (rest words)))))

(timed-stochastic-hill-climber (word-list))

(comment "stochastic-hill-climber returns the number of iterations it took to find the target words. An edit I could do would be to return information about the time
          it took to do so but I took a different approach. At first I generated a lazy sequence of words but my implementation was (I think) efficient so I found it
          better to just create a range. After awhile I noticed that after iteration 90 words were taking over 200ms to calculate. What I then did was create a list of
          runs that detailed the first word that took over 200ms to run and the iteration that this took place. After 100 runs I then averaged the numver of iterations
         that it took longer tha  200ms to calculate. Therefore, on average my algorithm can calculate a string that is 73 characters in length using SHC in under 200ms.")
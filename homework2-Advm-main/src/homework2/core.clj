(ns homework2.core)
(comment 
;; ;; Question 1
(def random-rolls
  "Declares an infinite sequence of dice rolls"
  (repeatedly #(rand-nth (range 1 7))))

(take 4 random-rolls)

;TODO: Maybe Reduce runtime to O(n) by using memoization (maybe)
(defn check-sequence
  "Checks 5 elements of a sequence and returns if 5 6s are sequential"
  [elements]
  (loop
   [current elements]
    (if (empty? current)
      true
      (if (not= 6 (first current))
        false
        (recur (rest current))))))

(defn index-of-five-6s
  "Given an infinite sequence of dice rolls return the first index of 5 sequential 6s"
  [dice-rolls]
  (loop
   [idx 0
    rolls dice-rolls]
    (if (check-sequence (take 5 rolls))
      idx
      (recur (inc idx) (rest rolls)))))

(index-of-five-6s random-rolls)

;; ;; Question 2  
(defn long-divide
  "Divides a number using long division method and returns a vector of the from
   [result remainder]"
  [num denom]
  [(quot (* num 10) denom) (mod (* num 10) denom)])

(long-divide 2 7)

(defn ratio->infinite-seq
  "Returns an infinite sequence of a ratio"
  [ratio]
  (let [num (if (= ratio 0) 0 (numerator ratio))
        denom (if (= ratio 0) 1 (denominator ratio))
        result (long-divide num denom)]
    (lazy-seq (cons (int (result 0))
                    (ratio->infinite-seq (/ (int (result 1)) denom))))))


(take 50 (ratio->infinite-seq 3/8))
(take 50 (ratio->infinite-seq 2/7))

;; Question 3
(def cityMap [[0 111 431 291 512 215 924 145]
              [111 0 378 451 10000000 49 862 377]
              [431 378 0 789 10000000 647 67 172]
              [291 451 789 0 10000000 415 424 119]
              [512 10000000 10000000 10000000 0 10000000 392 10000000] ;;Really high values to have City 4 have only one neighboring city to hopefully push the salesman to force visit a city
              [215 49 647 415 10000000 0 596 706]
              [924 862 67 424 392 596 0 806]
              [145 377 172 119 10000000 706 806 0]])

(defn swap-cities
  [cities]
  (let [a (rand-nth (range (count cities))) b (rand-nth (range (count cities)))]
    (assoc cities a (cities b) b (cities a))))

(swap-cities [0 3 1 2])

(defn random-route
  "Generates a random salesman traversal"
  [cities]
  (shuffle (range (count cities))))

(random-route cityMap)

(defn distance-between-cities
  "Given two cities returns the length between them"
  [walk]
  ((cityMap (first walk)) (last walk)))

(distance-between-cities [0 3])

(defn route-length
  "Given a list of cities determines the length of this route"
  [route]
  (let [dist (distance-between-cities [(last route) (first route)])
        cities (mapv #(vector %1 %2) (butlast route) (rest route))]
    (+ dist (reduce + (map #(distance-between-cities %) cities)))))

(defn ts-stochastic-hill-climber
  "A stochastic hill climbing method for The Travelling Salesman problem given a max iteration limit, a predisgnated route, and a list of cities
   as an adjacency matrix."
  [iterations current-route prev-distance]
  (loop [iteration 0
         current current-route
         prev prev-distance]
    (let [mutated-route (swap-cities current)
          mutated-distance (route-length mutated-route)]
      (if (= iteration iterations)
        [current prev]
        (recur (inc iteration) (if (<= prev mutated-distance) current mutated-route)
               (if (<= prev mutated-distance) prev mutated-distance))))))

;;1000 iterations is 1837
;;1500 iterations is 1837
;;5000 iterations is 1837
(def iteration0 (random-route cityMap))
(count (filter #(<= (% 1) 1837)(repeatedly 10000 #(ts-stochastic-hill-climber 100 iteration0 (route-length iteration0)))))

(comment "
              The problem that I used for my Hill Climbing Implementation is the traveling salesman problem. Given an adjancecy matrix
          populated with the cities and their distances to another city, my ts-stochastic-hill-climber function should return
          the route that has a good solution but not optimal.
              The first factor I considered is how many times that my hill-climber ran. I ran trials with 50, 75, 90, 100, 1000,1500, and 5000 
          iterations. What I was noticing that anything over 500 was a bit excessive. Based on my algorithm, 1837[miles] was the \"best\"
          solution. I then modified the amount of iterations to see when 1837 was approaching more often than not occured at around 100 iterations.
              The next factor I considered is the starting route. Given that I declare iteation0 then run the algorithm mulitple times this could
          affect the results. After a couple tests, 1837 approached irregardless of the starting string so this did not affect the result.
              The last factor I considered is the transition function. I was originally permutating the string but this literally is not the 
          point of hill-cilmbing. Given your advice, I swapped two cities and use that as the transition function only moving to neighbors whose
          mutated route resulted in a shorter route. I added my matrix online and a website (https://www.easycalculation.com/operations-research/traveling-salesman-problem.php)
          said that an optimal route had a distance of 1325 but their calculation did not account that the last city would visit the first city again
          so taken that into account my solution was more optimal. Yet, I digress, I believe my hill-climbing solution provides fairly good results."))

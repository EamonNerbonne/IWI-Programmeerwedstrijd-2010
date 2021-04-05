(ns fatslim2
  (:import (java.util Scanner)))

(import 'java.util.Scanner)
(def sc (java.util.Scanner. *in*))
(defn read-int [] (.nextInt sc))
(defn read-str [] (.next sc))

;;; one run

(def start-time (* 4 60 60))
(def train-time (* 8 60 60))

(defn simulate-people-through-n-gates
  "Given a +people+ seq of [arrival-time time-at-gate] pairs, and +n+ the number of gates available, calculates a seq of time-past-gate values."
  [people n] 
  (loop [people-before-gates people
         people-behind-gates '()
         gates  (repeat n 0)]
    (if (empty? people-before-gates)
      (sort people-behind-gates)
      (let [[arrival-time time-at-gate] (first people-before-gates)
            first-available-gate        (first gates)
            time-past-gate (+ (max first-available-gate arrival-time)
                              time-at-gate)]
        (recur (rest people-before-gates)
               (conj people-behind-gates time-past-gate)
               (sort (conj (rest gates) time-past-gate)))))))

;; returns the number of people who miss their train
(defn process [num-gates num-machines people]
  (count
   ;; filter to find only people who will miss their train
   (filter #(> (+ start-time %) train-time)
           ;; simulate people past the gates
           (simulate-people-through-n-gates 
            (map (fn [a] [a 1])     ;  give everyone a delay of 1 for the gates
                 (sort (concat 
                        ;; simulate people past the ticket machines
                        (simulate-people-through-n-gates (filter (fn [[_ delay]] (not (= delay 0)))
                                                                 people) 
                                                         num-machines)
                        ;; people who already have a ticket
                        (map first (filter (fn [[_ delay]] (= delay 0)) 
                                           people)))))
            num-gates))))

;;; startup, read input

(defn -main [& args]
  (let [runs (read-int)]
    (dotimes [r runs]
      (let [num-gates (read-int)
            num-machines (read-int)
            num-people (read-int)
            people (map (fn [_] [(read-int) (read-int)])
                        (repeat num-people nil))]
        (println (process num-gates num-machines people))))))

(-main)

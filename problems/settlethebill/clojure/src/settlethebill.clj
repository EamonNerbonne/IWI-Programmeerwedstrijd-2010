(ns settlethebill
  "A Clojure implementation of the SettleTheBill problem"
  (:gen-class)
  (:import (java.util Scanner)))

(import 'java.util.Scanner)
(def sc (Scanner. *in*))
(defn read-int [] (.nextInt sc))
(defn read-str [] (.next sc))

(defn read-run []
  (let [n    (read-int)
        rels (take n (repeat nil))]
    (map (fn [_] {:from   (read-int)
                  :to     (read-int)
                  :amount (read-int)})
         [n rels])))

(defn components [n rels]
  (loop [comps (range n)
         rels  rels]
    (recur ())))

(defn run-run [[n rels]]
  ;; TODO echte uitwerking :)
  (println (- n 1)))

; Read in stuff and go compute the mofo
(defn -main [& args]
  (let [runs (read-int)]
    (dotimes [r runs]
      (let [data (read-run)]
        (run-run data)))))

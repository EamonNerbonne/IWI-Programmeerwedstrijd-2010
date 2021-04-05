(ns fatslim
  "A Clojure implementation of the FatSlim problem"
  (:gen-class)
  (:import (java.util Scanner)))

(import 'java.util.Scanner)
(def sc (Scanner. *in*))
(defn read-int [] (.nextInt sc))
(defn read-str [] (.next sc))

(defn people-to-events [people]
  (map (fn [p] (assoc p :kind :arrival)) 
       people))

(defn cost [w s wide-gate-cost slim-gate-cost]
  (+ (* wide-gate-cost w)
     (* slim-gate-cost s)))

; w: wide gates, wf: wide gates free, ws: wide gates in use by slims
; s: slim gates, sf: slim gates free
(defn process-event [s e wide-gate-cost slim-gate-cost wide-costs-more-than-twice-slim]
  (loop [[w wf ws s sf] s
         [{k :kind, t :time, fat :fat, d :duration, ow :occupy-wide} & events :as events-all] e]
    (if (> (count events-all) 0)
      (cond
        (= k :arrival) (let [event-wide {:kind :depart :time (+ t d) :occupy-wide 'wide-in-wide}
                             event-wiim {:kind :depart :time (+ t d) :occupy-wide 'slim-in-wide}
                             event-slim {:kind :depart :time (+ t d) :occupy-wide 'slim-in-slim}
                             events-w  (sort-by (fn [{t :time}] t) (conj events event-wide))
                             events-ws (sort-by (fn [{t :time}] t) (conj events event-wiim))
                             events-s  (sort-by (fn [{t :time}] t) (conj events event-slim))]
                         (if fat
                           (cond
                             (> wf 0)                                                      (recur [w       (dec wf) ws       s       sf]       events-w)
                             (and (> ws 0) (> sf 0) wide-costs-more-than-twice-slim)       (recur [w       wf       (dec ws) (inc s) sf]       events-w)
                             (and (> ws 0) (> sf 0))                                       (recur [(inc w) wf       ws       (dec s) (dec sf)] events-w)
                             (> ws 0)                                                      (recur [w       wf       (dec ws) (inc s) sf]       events-w)
                             (> sf 0)                                                      (recur [(inc w) wf       ws       (dec s) (dec sf)] events-w)
                             true                                                          (recur [(inc w) wf       ws       s       sf]       events-w))
                           (cond 
                             (> sf 0)                                                      (recur [w       wf       ws       s       (dec sf)] events-s)
                             (> wf 0)                                                      (recur [w       (dec wf) (inc ws) s       sf]       events-ws)
                             true                                                          (recur [w       wf       ws       (inc s) sf]       events-s))
                           ))
        (= k :depart) (cond 
                        (= ow 'wide-in-wide) (recur [w (inc wf) ws       s sf]       events)
                        (= ow 'slim-in-wide) (recur [w (inc wf) (dec ws) s sf]       events)
                        (= ow 'slim-in-slim) (recur [w wf       ws       s (inc sf)] events)))
      [w wf ws s sf])))

(defn read-people [] "Read c people from stdin"
  (let [c (read-int)
        p (take c (repeat nil))]
    (map (fn [_] {:time (read-int)
                  :duration (read-int)
                  :fat (let [f (read-str)]
                         (if (= f "L")
                           false
                           true))})
         p)))

; Read in stuff and go compute the mofo
(defn -main [& args]
  (let [runs (read-int)]
    (dotimes [r runs]
      (let [slim-gate-cost (read-int)
            wide-gate-cost (read-int)
            wide-costs-more-than-twice-slim   (> wide-gate-cost (* 2 slim-gate-cost))
            events (people-to-events (read-people))

            [w wf ws s sf] (process-event [0 0 0 0 0] 
                                          events
                                          wide-gate-cost
                                          slim-gate-cost
                                          wide-costs-more-than-twice-slim)]
        (println (cost w s wide-gate-cost slim-gate-cost))))))

(-main)

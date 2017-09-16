(ns double-booked.core
  (:require [clj-time.core :as t]
            [clojure.pprint :refer [pprint]]))

(def default-event-num 10)

(defn generate-events!
  "Generate a sequence of n events.
   default-event-num is used if no parameter is supplied."
  ([]
   (generate-events! default-event-num))
  ([n]
   (println (str "Generating " n " events."))
   (map (fn [id]
          (let [week-in-seconds 604800
                period-offset  #(do
                                  (t/days (inc (rand-int 60)))
                                  (t/seconds (inc (rand-int week-in-seconds))))
                start          (t/plus (t/now)
                                       (period-offset))
                end            (t/plus start
                                       (period-offset))]
            {:id    id
             :start start
             :end   end}))
        (range n))))

(defn events-overlap?
  "Given two events of different IDs,
   check if the intervals of those events overlap."
  [e1 e2]
  (when-not (= (:id e1) (:id e2))
    (t/overlaps? (t/interval (:start e1) (:end e1))
                 (t/interval (:start e2) (:end e2)))))

(defn- get-overlaps-for-event
  "Given a sequence of events and an event to check against,
   returns a vector of pairs of matching events, sorted by event :id."
  [evts e1]
  (reduce
   (fn [acc e2]
     (if (events-overlap? e1 e2)
       (conj acc (sort-by :id [e1 e2]))
       acc))
   [] evts))

(defn- find-overlapping-events
  "Given a sequence of events, find all overlapping pairs.
   Returns a set of unique, overlapping pairs of events."
  [events]
  (->> events
       (reduce
        (fn [acc e]
          (let [overlaps (get-overlaps-for-event events e)]
            (into acc overlaps)))
        [])
       set))

(defn- print-results
  "Given a number of events and sequence of overlapping event pairs,
   prints a map containing a summary of overlap results."
  [event-count pairs]
  (pprint
   {:matching-pairs   pairs
    :number-of-events event-count
    :number-of-pairs  (count pairs)}))

(defn -main [& args]
  (if-let [param (first args)]
    (try
      (let [n     (Integer. param)
            pairs (find-overlapping-events (generate-events! n))]
        (print-results n pairs)
        pairs)
      (catch Exception e
        (println (str "Please provide a number of events as an integer. Exception: " (.getMessage e)))))
    (let [events (generate-events!)
          pairs  (find-overlapping-events events)]
      (print-results (count events) pairs)
      pairs)))

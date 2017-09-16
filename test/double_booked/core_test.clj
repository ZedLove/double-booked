(ns double-booked.core-test
  (:require [clj-time.core :as t]
            [clojure.test :as test :refer [deftest is testing]]
            [double-booked.core :refer :all]))

(deftest generate-events!-test
  (testing "Generates the right number of events"
    (is (let [n 5]
          (= n (count (generate-events! n)))))
    (is (= default-event-num (count (generate-events!)))))
  (testing "Generates valid events"
    (is (let [{:keys [id start end]} (first (generate-events! 1))]
          (and id start end)))))

(deftest events-overlap?-test
  (testing "Doesn't check events of the same ID"
    (is (let [event {:id 1}]
          (= nil (events-overlap? event event)))))
  (testing "Correctly finds overlap in events with different IDs"
    (is (let [identical-date {:start (t/now) :end (t/plus (t/now) (t/days 1))}
              e1 (merge identical-date {:id 1})
              e2 (merge identical-date {:id 2})]
          (= true (events-overlap? e1 e2)))))
  (testing "Correctly doesn't find overlap in events with different IDs"
    (is (let [e1 {:id 1 :start (t/now) :end (t/plus (t/now) (t/days 1))}
              e2 {:id 2 :start (t/plus (:end e1) (t/days 1)) :end (t/plus (t/now) (t/days 10))}]
          (= false (events-overlap? e1 e2))))))

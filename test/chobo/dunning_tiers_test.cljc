(ns chobo.dunning-tiers-test
  "Dunning escalation tiers: reminder → urgent → final → collection."
  (:require [clojure.test :refer [deftest is testing]]
            [chobo.invoice :as inv]))

;; The days-overdue v1 stub extracts day-of-month from ISO date strings.
;; Tests use dates within the same month to keep the stub accurate.

(deftest dunning-tier-reminder-test
  ;; due 2026-06-27, now 2026-06-30 → 3 days overdue → reminder (≥1)
  (let [inv (-> (inv/invoice "gftd") (inv/add-line (inv/line "Sub" 3000))
                (inv/mark-issued) (assoc :due-at "2026-06-27"))]
    (is (= :reminder (:tier (inv/dunning-tier inv "2026-06-30"))))))

(deftest dunning-tier-urgent-test
  ;; due 2026-06-20, now 2026-06-30 → 10 days overdue → urgent (≥7)
  (let [inv (-> (inv/invoice "gftd") (inv/add-line (inv/line "Sub" 3000))
                (inv/mark-issued) (assoc :due-at "2026-06-20"))]
    (is (= :urgent (:tier (inv/dunning-tier inv "2026-06-30"))))))

(deftest dunning-tier-not-overdue-test
  (let [inv (-> (inv/invoice "gftd") (inv/add-line (inv/line "X" 100)) (inv/mark-issued)
                (assoc :due-at "2026-12-31"))]
    (is (nil? (inv/dunning-tier inv "2026-06-30")))))

(deftest dunning-tier-activity-test
  (let [inv (-> (inv/invoice "gftd") (inv/add-line (inv/line "Sub" 3000))
                (inv/mark-issued) (assoc :due-at "2026-06-20"))
        tier (inv/dunning-tier inv "2026-06-30")
        a (inv/dunning-tier-activity inv tier {:tenant "gftd"})]
    (is (= :billing (:lane a)))
    (is (= :dunning-escalation (:kind a)))
    (is (= "gftd" (:tenant a)))
    (is (= :urgent (get-in a [:props :tier])))))

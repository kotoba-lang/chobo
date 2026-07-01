(ns chobo.invoice-tax-test
  "Invoice tax breakdown: line-with-tax, apply-tax, tax-summary."
  (:require [clojure.test :refer [deftest is testing]]
            [chobo.invoice :as inv]))

(deftest line-with-tax-test
  (let [l (inv/line "Pro subscription" 2900 "JPY")
        taxed (inv/line-with-tax l 0.08)]
    (is (= 232 (:tax taxed)))                  ; 2900 * 0.08 = 232.0 → long 232
    (is (= 3132 (:total-with-tax taxed)))))    ; 2900 + 232

(deftest apply-tax-test
  (let [inv' (-> (inv/invoice "gftd")
                 (inv/add-line (inv/line "Pro" 2900))
                 (inv/add-line (inv/line "Overage" 100))
                 (inv/apply-tax 0.08))]
    (is (= 2 (count (:lines inv'))))
    (is (= 240 (:tax (:tax-summary inv'))))               ; (2900+100)*0.08 = 240
    (is (= 3240 (:total (:tax-summary inv'))))            ; 3000 + 240
    (is (= 0.08 (:rate (:tax-summary inv'))))))

(deftest tax-summary-fallback-test
  (let [inv' (-> (inv/invoice "gftd") (inv/add-line (inv/line "X" 100)))]
    ;; no apply-tax → fallback to plain totals
    (is (= 0 (:tax (inv/tax-summary inv'))))
    (is (= 100 (:total (inv/tax-summary inv'))))))

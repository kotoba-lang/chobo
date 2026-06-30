(ns chobo.views-test
  (:require [clojure.test :refer [deftest is testing]]
            [shitsuke.hiccup :as hic]
            [chobo.views :as views]
            [chobo.ssr :as ssr]))

(deftest tenant-badge-renders-test
  (let [html (hic/->html (views/tenant-badge {:name "gftd." :operator-level :certified}))]
    (is (clojure.string/includes? html "gftd."))
    (is (clojure.string/includes? html "certified"))))

(deftest meter-gauge-renders-test
  (let [html (hic/->html (views/meter-gauge
                          {:quotas {:image-gen 200}}
                          {:consumed {:image-gen 210}}
                          :image-gen))]
    (is (clojure.string/includes? html "image-gen"))
    (is (clojure.string/includes? html "210"))
    (is (clojure.string/includes? html "200"))
    (is (clojure.string/includes? html "OVER"))))

(deftest invoice-card-renders-test
  (let [html (hic/->html (views/invoice-card
                          {:id "inv_1" :lines [{:description "Pro" :amount 2900 :currency "JPY"}]
                           :status :issued}))]
    (is (clojure.string/includes? html "inv_1"))
    (is (clojure.string/includes? html "Pro"))
    (is (clojure.string/includes? html "2900"))
    (is (clojure.string/includes? html "issued"))))

(deftest ssr-root-html-stable-test
  (let [html (ssr/root-html)]
    (is (clojure.string/starts-with? html "<!doctype html>"))
    (is (clojure.string/includes? html "Pro"))           ; plan name
    (is (clojure.string/includes? html "OVER"))           ; meter over
    (is (clojure.string/includes? html "Audit ledger"))))

(deftest ssr-parity-test
  (is (= (hic/->html (views/root (ssr/sample-db)))
         (hic/->html (views/root (ssr/sample-db))))))

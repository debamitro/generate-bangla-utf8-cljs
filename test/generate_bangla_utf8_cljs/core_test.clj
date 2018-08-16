(ns generate-bangla-utf8-cljs.core-test
  (:require [clojure.test :refer :all]
            [generate-bangla-utf8-cljs.converter :as converter]))

(deftest a-test
  (testing "Try a phrase"
    (= (converter/to-bangla-utf8 "ro^de rAngA i^Ter pA^jA") "রোঁদে রাঙা ইঁটের পাঁজা")))
(deftest a-test2
  (testing "Try a phrase"
   (= (converter/to-bangla-utf8 "hA^u mA^u khA^u") "হাঁউ মাঁউ খাঁউ")))

(deftest b-test
  (testing "Try a word"
    (= (converter/to-bangla-utf8 "nidArun garam") "নিদারুন গরম")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

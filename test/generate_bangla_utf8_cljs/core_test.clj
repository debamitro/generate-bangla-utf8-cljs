(ns generate-bangla-utf8-cljs.core-test
  (:require [clojure.test :refer :all]
            [generate-bangla-utf8-cljs.converter :as converter]))

(deftest a-test
  (testing "FIXME, I fail."
    (= (converter/to-bangla-utf8 "ro^de rAngA i^Ter pA^jA") "রোঁদে রাঙা ইঁটের পাঁজা")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

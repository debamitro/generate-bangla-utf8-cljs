(defproject generate-bangla-utf8-cljs "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 ]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :main ^:skip-aot generate-bangla-utf8-cljs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

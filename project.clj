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
  :cljsbuild {
              :builds [
                       {:id "default"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "target/out.js"
                                   :main generate-bangla-utf8-cljs.core
                                   :pretty-print true
                                   :optimizations :advanced
                                   }
                        }
                       {:id "node"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "target/out_node.js"
                                   :main generate-bangla-utf8-cljs.core
                                   :target :nodejs
                                   :pretty-print true
                                   :optimizations :advanced
                                   }
                        }
                       ]
              }
  :main ^:skip-aot generate-bangla-utf8-cljs.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

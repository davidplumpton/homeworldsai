(defproject homeworldsai "0.1.0-SNAPSHOT"
  :description "An AI for the Looney Pyramids game of Homeworlds"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot homeworldsai.core
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.3.0"]]
  :profiles {:uberjar {:aot :all}})

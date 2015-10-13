(defproject table-to-b "0.1.0-SNAPSHOT"
  :description "Ein Programm zur Erstellung einer B-Maschine aus einem Spreadsheet"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [de.hhu.stups/bparser "2.4.40"]
                 [dk.ative/docjure "1.9.0"]
                 [stencil "0.5.0"]
                 [midje "1.7.0"]]
  :plugins [[lein-cloverage "1.0.6"]]
  :main ^:skip-aot table-to-b.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

(defproject table-to-b "0.1.0-SNAPSHOT"
  :description "Ein Programm zur Erstellung einer B-Maschine aus einem Spreadsheet"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                          [dk.ative/docjure "1.9.0"]]
  :main ^:skip-aot table-to-b.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

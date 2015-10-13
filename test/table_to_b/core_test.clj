(ns table-to-b.core-test
  (:require [clojure.test :refer :all]
            [table-to-b.core :refer :all]
            [midje.sweet :as mj]
            [dk.ative.docjure.spreadsheet :as dj]))

(def mwb  (dj/create-workbook "mockworkbook" [["Z11" "Z21"] ["Z12" 15]["Z13" 16]]))

(def sheet (dj/select-sheet "mockworkbook" mwb))

(def rows (dj/row-seq sheet))

(def titles (map dj/read-cell (first rows)))

(deftest get-datatype-test
  (is (= (get-datatype 0) "INTEGER")))

(mj/fact "Testet Zuordnung der Typnummer zur Bezeichnung (get-datatype)"
  (get-datatype 0) => "INTEGER"
  (get-datatype 1) => "STRING"
  (get-datatype 2) => "Formula"
  (get-datatype 3) => "Blank"
  (get-datatype 4) => "BOOLEAN"
  (get-datatype 5) => "Error")

(mj/fact "testet row-as-tuple"
  (row-as-tuple (first rows) 2) => "\"Z11\", \"Z21\""
  (row-as-tuple (nth rows 1) 2) => "\"Z12\", 15"
  (row-as-tuple (nth rows 2) 3) => "\"Z13\", 16, \"\"")

(mj/fact "testet get-type"
  (get-type (first (first rows))) => 1
  (get-type (first (rest (first (rest rows))))) => 0)

(mj/fact "testet element"
  (element (first (first rows))) => "\"Z11\""
  (map element (first (rest rows))) => ["\"Z12\"" 15])

(mj/fact "testet types-of-nth-column"
  (types-of-nth-column 1 sheet) => [1 0 0])

(mj/fact "testet haupttyp"
  (haupttyp [1 0 0]) => "STRING"
  (haupttyp [0 0 0]) => "INTEGER"
  (haupttyp [1 1 1]) => "STRING"
  (haupttyp [2 2 2]) =>"Formula"
  (haupttyp [3 3]) => "Blank"
  (haupttyp [4 4 4 4]) => "BOOLEAN"
  (haupttyp [5 5]) => "Error")

(mj/fact "testet haupttypen"
  (haupttypen titles (rest rows)) => ["STRING" "INTEGER"])

(mj/fact "testet zuordnung"
  (zuordnung ["N1" "N2" "N3"] ["W1" "W2" "W3"]) => "N1:W1,N2:W2,N3:W3")

(mj/fact "testet ermittel_titel"
  (ermittel_titel '("Spalte 1" "Spalte_2")) => '("Spalte_1" "Spalte_2"))

(mj/fact "testet tupletype"
  (tupletype titles (rest rows)) => "STRING*INTEGER")

(mj/fact "testet tuples"
  (tuples (rest rows) (count titles)) => [{:tuple "\"Z12\", 15"} {:tuple "\"Z13\", 16"}])

(mj/fact "testet tuplefunctions"
  (tuplefunctions titles (rest rows)) => [{:title "Z11" :id 0} {:title "Z21" :id 1}])

(mj/fact "testet rec"
  (rec (rest rows) titles) => [{:titles_Werte "Z11:\"Z12\",Z21:15"} {:titles_Werte "Z11:\"Z13\",Z21:16"}])

(mj/fact "testet titles_haupttypen"
  (titles_haupttypen titles (rest rows)) => "Z11:STRING,Z21:INTEGER")

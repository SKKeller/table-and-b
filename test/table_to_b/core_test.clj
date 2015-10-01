(ns table-to-b.core-test
  (:require [clojure.test :refer :all]
            [table-to-b.core :refer :all]
            [midje.sweet :as mj]
            [dk.ative.docjure.spreadsheet :as dj]))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(def mwb  (dj/create-workbook "mockworkbook" [["Z11" "Z21"] ["Z12" 15]["Z13" 16]]))

(def sheet (dj/select-sheet "mockworkbook" mwb))

(def rows (dj/row-seq sheet))

(deftest get-datatype-test
  (is (= (get-datatype 0) "INTEGER")))

(mj/fact "Testet Zuordnung der Typnummer zur Bezeichnung"
  (get-datatype 0) => "INTEGER"
  (get-datatype 1) => "STRING"
  (get-datatype 2) => "Formula"
  (get-datatype 3) => "Blank"
  (get-datatype 4) => "BOOLEAN"
  (get-datatype 5) => "Error")

(mj/fact
  (row-as-tuple [1 "\"test\"" 3]) => "(1, \"test\", 3)\n")

(mj/fact "testet get-type"
  (get-type (first (first rows))) => 1
  (get-type (first (rest (first (rest rows))))) => 0)

(mj/fact "testet element"
  (element (first (first rows))) => "\"Z11\""
  (map element (first (rest rows))) => ["\"Z12\"" 15])

(mj/fact "testet row-as-tuple"
  (row-zu-vector (first (rest rows))) => ["\"Z12\"", 15])

(mj/fact "testet row-as-tuple"
  (row-as-tuple ["\"Z12\"", 15]) => "(\"Z12\", 15)\n")

(mj/fact "testet row-as-tuples"
  (rows-as-tuples rows) => "(\"Z11\", \"Z21\")\n(\"Z12\", 15)\n(\"Z13\", 16)\n")

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

(mj/fact "testet anzahl-Reihen"
  (anzahl-Reihen sheet) => 3)

(mj/fact "testet anzahl-Spalten"
  (anzahl-Spalten (first rows)) => 2)

(mj/fact "testet haupttypen"
  (haupttypen sheet) => ["STRING" "INTEGER"])

(mj/fact "testet erstelle-Zuordnung"
  (erstelle-zuordnung ["N1" "N2" "N3"] ["W1" "W2" "W3"]) => "N1:W1,N2:W2,N3:W3")

(mj/fact "testet record-data"
  (record-data (rest rows) (first rows)) => "rec(Z11:\"Z12\",Z21:15)\nrec(Z11:\"Z13\",Z21:16)\n")

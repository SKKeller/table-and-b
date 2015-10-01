(ns table-to-b.core
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [clojure.java.io :as io]
            [stencil.core :as sc]
            [clojure.string :as st]
            [clostache.parser :as cs])
  (:import de.be4.classicalb.core.parser.BParser
           de.be4.classicalb.core.parser.exceptions.BException)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn test1 "funktioniert" []
  (sc/render-string "tests:, {{#repo}} {{name}}: {{/repo}}."
             {:repo [{:name "t1"} {:name "t2"} {:name "t3"}]}))

(defn test2 "wirft fehlermeldung, da es das template nicht findet" []
  (sc/render-file "templates/test2.mustache" {:name "Welt"}))

(defn test3 "" []
  (cs/render-resource "templates/test2.mustache" {:name "Michael"}))

(def workbook
  "läd spreadsheet.xlsx"
  (dj/load-workbook "spreadsheet.xlsx"))

(def s
  "tabelle Price List"
  (dj/select-sheet "Price List" workbook))

(def datatype
  "Zuordnung Datentypnr -> Bezeichnung"
  {:0 "INTEGER" :1 "STRING" :2 "Formula" :3 "Blank" :4 "BOOLEAN" :5 "Error"})

(def tupleanfang
  "String für den Anfang der Tupleversion"
  (str "MACHINE ExcelTable_TupleVersion" \newline "DEFINITIONS TUPLETYPE == ("))

(def recanfang
  "String für den Anfang der Recordversion"
  (str "MACHINE ExcelTable_RecordVersion"))

(defn get-datatype
  "ordnet der Typnummer den Datentyp zu"
  [typenr]
  (get datatype ((comp keyword str) typenr)))

(defn get-type
  "Gibt den Typ der übergeben Zelle als Zahl zurück, Zuordnung siehe 'datatype'"
  [cell]
  (when cell
    (.getCellType cell)))

(defn element
  "Gibt das übergebene Element mit Anführungszeichen zurück, wenn es vom Typ String ist
  sonst ohne"
  [el]
  (if (= (get-type el) 1)
    (str \" el \")
  (int (dj/read-cell el))))

(defn row-as-tuple
  "Gibt die Daten einer Reihe als Tuple zurück
  angepasst nach Typ mit Anführungszeichen oder ohne"
  [rowvector]
  (str "(" (st/join ", " rowvector) ")" \newline))

(defn row-zu-vector
  "gibt einen Vector zurück, der die Elemente der Row enthält"
  [row]
  (map element row))

(defn rows-as-tuples
  "bekommt Reihen, gibt einen String wieder, der die Reihen als tuple wiedergibt
  getrennt durch Zeilenumbrueche"
  [rows]
  (reduce str (map row-as-tuple (map row-zu-vector rows))))

(defn get-cell
  "Gibt den Inhalt der n. Zelle Zeile row wieder"
  [n row]
  (.getCell row n))

(defn types-of-nth-column
  "Gibt die Typen der Zellen der n. Spalte der Tabelle sheet als Zahlen wieder"
  [n rows]
  (map (comp get-type (partial get-cell n)) rows))

(defn haupttyp
  "Gibt den Namen der typen wieder, wenn er bei allen gleich ist,
  sonst 'STRING'"
  [typen]
  (if (apply = typen)
    (get-datatype (first typen))
    "STRING"))

(defn anzahl-Reihen
  "Gibt Anzahl der Reihen der Tabellenseite wieder"
  [sheet]
  (count (dj/row-seq sheet)))

(defn anzahl-Spalten
  "gibt die Anzahl der Zellen der Reihe zurück"
  [row]
  (count (map element row)))

(defn haupttypen
  "Gibt die Datentypen der Reihen der Tabelle als list wieder"
  [rows]
  (loop [i (anzahl-Spalten (first rows)) erg []]
    (if (> 0 i )
      (reverse (rest erg))
      (recur (dec i) (conj erg (haupttyp (types-of-nth-column i (rest rows))))))))

(defn dateibegin
  "Erstellt den Anfang der B-Datei"
  [sheet]
  (str tupleanfang (reduce str (interpose "*" (haupttypen (dj/row-seq sheet)))) ")" ))

(defn tuple-properties
  "Erstellt den Properties Teil für Tuples"
  [rows]
  (str "PROPERTIES" \newline
  "Excel : POW(TUPLETYPE) & /* Typing */" \newline
  "Excel = { /* the Data */" \newline
  (rows-as-tuples (rest rows)) " }"))

(defn generate-function
  "erstellt das Grundgerüst der projection function für Tuples"
  [n]
  (let [vars (reduce str (interpose ",x" (range 0 (inc n))))]
    (str " = %(x" vars ").((x" vars "):TUPLETYPE|x")))

(defn tuples-functions
  "erstellt die projection functions für die Tupleversion"
  [row]
  (let [function (generate-function (count (map dj/read-cell row)))]
    (loop [i 0 cells (map dj/read-cell row) erg ""]
      (if (empty? cells)
        erg
        (recur (inc i) (rest cells) (str erg (first cells) function i ")" \newline))))))

(defn abstract-constants
  "erstellt den ABSTRACT_CONSTANTS-Teil"
  [row]
  (str "ABSTRACT_CONSTANTS " (reduce str (interpose ", " (map dj/read-cell row))) \newline "CONSTANTS Excel" \newline ))

(defn erstelle-tuplemachine
  "erstellt die Tupleversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test.txt"
  [sheet]
  (let [rows (dj/row-seq sheet)]
    (spit "test.txt"
      (str (dateibegin sheet)  \newline
      (abstract-constants (first rows)) \newline
      (tuple-properties rows) \newline
      (tuples-functions (first rows))))))

(defn erstelle-zuordnung
  "Erstellt eine Zuordnung vom Muster name1:wert1,name2:wert2..."
  [namen werte]
  (subs (reduce str (interleave (repeat ",") namen (repeat ":") werte )) 1))

(defn record-data
  "Erstellt den Data-Teil der Recordversion"
  [rows titles]
  (reduce str
    (for [row rows]
      (str "rec(" (erstelle-zuordnung titles (map element row)) ")" \newline))))

(defn record-properties
  "Erstellt den Properties Teil für Records"
  [sheet titles]
  (str "PROPERTIES" \newline "Excel : POW(struct(" (erstelle-zuordnung titles (haupttypen sheet)) "))"))

(defn function-line
  "Erstellt die Funktion für Variable var und Zuordnung pow"
  [pow var]
  (let [x (gensym "x")]
    (str "&" \newline var " = %" x ".(" x ":struct(" pow ")|" x "'" var ")" \newline)))

(defn record-functions
  "Erstellt die projection functions für die Recordversion"
  [sheet titles]
  (let [pow (erstelle-zuordnung titles (haupttypen sheet))]
    (reduce str (map (partial function-line pow) titles))))

(defn erstelle-recordmachine
  "erstellt die Recordversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test1.txt"
  [sheet]
  (let [rows (dj/row-seq sheet)
        titles (map dj/read-cell (first rows))]
    (spit "test1.txt"
     (str recanfang \newline
     (abstract-constants (first rows)) \newline
     (record-properties sheet titles) \newline
    "Excel = { /* the Data */}" \newline
    (record-data (rest rows) titles) \newline
    (record-functions sheet titles) \newline))))

(defn first-row [dateiname sheetname]
  (let [workbook (dj/load-workbook (str dateiname))
        sheet (dj/select-sheet (str sheetname) workbook)
        columns (dj/select-columns {:A :name, :B :price, :C :wert} sheet)
        rows (dj/row-seq sheet)]
    (dj/sheet-name (first (dj/sheet-seq workbook)))
    ))

(defn valid-b-identifier?
  "prüft, ob es sich bei dem übergebenen string um einen validen B-Identifier handelt"
  [string]
  (try
    (do  (BParser/parse (str "#FORMULA " string))
         true)
    (catch BException e false)))

(ns table-to-b.core
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [clojure.java.io :as io])
  (:import de.be4.classicalb.core.parser.BParser
           de.be4.classicalb.core.parser.exceptions.BException)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
    el))

(defn row-as-tuple
  "Gibt die Daten einer Reihe als Tuple zurück
  angepasst nach Typ mit Anführungszeichen oder ohne"
  [row]
  (str "(" (reduce str (interpose ", " (map element row))) ")" \newline))

(defn rows-as-tuples
  "bekommt Reihen, gibt einen String wieder, der die Reihen als tuple wiedergibt
  getrennt durch Zeilenumbrueche"
  [rows]
  (reduce str (map row-as-tuple rows)))

(defn get-cell
  "Gibt den Inhalt der n. Zelle Zeile row wieder"
  [n row]
  (.getCell row n))

(defn types-of-nth-column
  "Gibt die Typen der Zellen der n. Spalte der Tabelle sheet als Zahlen wieder"
  [n sheet]
  (map (comp get-type (partial get-cell n)) (dj/row-seq sheet)))

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

(defn haupttypen
  "Gibt die Datentypen der Reihen der Tabelle als list wieder"
  [sheet]
  (loop [i (anzahl-Reihen sheet) erg ()]
    (if (> 0 i)
      erg
      (recur (dec i) (conj erg (haupttyp (types-of-nth-column i sheet))) ))))

(defn dateibegin
  "Erstellt den Anfang der B-Datei"
  [sheet]
  (str tupleanfang (reduce str (interpose "*" (haupttypen sheet))) ")" ))

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
  [var pow i]
  (str "&" \newline var " = %x" i ".(x" i ":struct(" pow ")|x" i "'" var ")" \newline))

(defn record-functions
  "Erstellt die projection functions für die Recordversion"
  [sheet titles]
  (let [pow (erstelle-zuordnung titles (haupttypen sheet))]
    (loop [i 0 erg ""]
      (if (= i (count titles))
        erg
        (recur (inc i) (str erg (function-line (nth titles i) pow i)))))))

(defn erstelle-recordmachine
  "erstellt die Recordversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test1.txt"
  [sheet]
  (let [rows (dj/row-seq sheet)
        titles (map dj/read-cell (first rows))]
    (spit "test1.txt"
     (str recanfang \newline
     (abstract-constants (first rows)) \newline
     (record-properties sheet titles) \newline
    "Excel = { /* the Data /*}" \newline
    (record-data (rest rows) titles)
    (record-functions sheet titles) \newline))))

(defn first-row [dateiname sheetname]
  (let [workbook (dj/load-workbook (str dateiname))
        sheet (dj/select-sheet (str sheetname) workbook)
        columns (dj/select-columns {:A :name, :B :price, :C :wert} sheet)
        rows (dj/row-seq sheet)]
    (dj/sheet-name (first (dj/sheet-seq workbook)))
    ))

(defn valid-b-identifier? [string]
  (try
    (do  (BParser/parse (str "#FORMULA " string))
         true)
    (catch BException e false)))

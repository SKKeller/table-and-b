(ns table-to-b.core
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [clojure.java.io :as io]
            [stencil.core :as sc]
            [clojure.string :as st])
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

(defn test2 "funktioniert" []
  (sc/render-file  "templates/test2" {:name "Welt"}))

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
  (st/join ", " rowvector))

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

(defn dateibegin_tuple
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

(defn valid-b-identifier?
  "prüft, ob es sich bei dem übergebenen string um einen validen B-Identifier handelt"
  [string]
  (try
    (do  (BParser/parse (str "#FORMULA " string))
         true)
    (catch BException e false)))

(defn pruefe_titel
  "überprüft, ob es sich bei dem Titel um einen validen B-Identifier handelt,
  wenn nein, ersetzt alle Leerzeichen durch _ und prüft erneut"
  [titel]
  (if (valid-b-identifier? titel)
    titel
    (if (valid-b-identifier? (st/replace titel " " "_"))
      (st/replace titel #" " "_")
      titel)))

(defn ermittel_titel
  "prüft für alle Elemente des Vektors, ob es sich um einen gültigen Titel handelt
  und passt ihn ggf an"
  [titles]
  (map pruefe_titel titles))

(defn ermittel_tupletype
  ""
  [rows]
  (st/join "*" (haupttypen rows)))

(defn ermittel_tuples
  ""
  [rows]
  (loop [reihen rows erg []]
    (if (empty? reihen)
      erg
      (recur (rest reihen) (conj erg { :tuple (row-as-tuple (row-zu-vector (first reihen)))})))))

(defn ermittel_tuplefunctions
  ""
  [titles rows]
  (loop [i 0 erg []]
    (if (= (count titles) i)
      erg
      (recur (inc i) (conj erg {:title (nth titles i) :id i})))))

(defn tuplemachine
  ""
  [sheet]
  (let [rows (dj/row-seq sheet)
        erste_Reihe (map dj/read-cell (first rows))
        titles (ermittel_titel erste_Reihe)
        zuordnung (assoc {}
          :tupletype (ermittel_tupletype rows)
          :titles (st/join ", " titles)
          :tuples (ermittel_tuples (rest rows))
          :functions (ermittel_tuplefunctions titles (rest rows))
          :vars (st/join ",x" (range 0 (count titles))))]
        (spit "test3.txt"
          (sc/render-file "templates/tupleversion" zuordnung))))

(defn erstelle-tuplemachine
  "erstellt die Tupleversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test.txt"
  [sheet]
  (let [rows (dj/row-seq sheet)]
    (spit "test.txt"
      (str (dateibegin_tuple sheet)  \newline
      (abstract-constants (first rows)) \newline
      (tuple-properties rows) \newline
      (tuples-functions (first rows))))))

(defn erstelle-zuordnung
  "Erstellt eine Zuordnung vom Muster name1:wert1,name2:wert2..."
  [namen werte]
  (subs (reduce str (interleave (repeat ",") namen (repeat ":") werte )) 1))

(defn titles_werte
  "Ermittelt die Werte für den titles_Werte tag für das recordversion-Template"
  [rows titles]
  (loop [reihen rows erg []]
    (if (empty? reihen)
      erg
      (recur (rest reihen) (conj erg { :titles_Werte (erstelle-zuordnung titles (map element (first reihen)))})))))

(defn title_id
  "Ermittelt die Werte für den title tag und den id tag für das recordversion-Template"
  [titles]
  (loop [i 0 erg []]
    (if (= (count titles) i)
      erg
      (recur (inc i) (conj erg {:title (nth titles i) :id i})))))

(defn recordmachine
  "erstellt die Recordmachine mit Hilfe des templates"
  [sheet]
  (let [rows (dj/row-seq sheet)
        erste_Reihe (map dj/read-cell (first rows))
        titles (ermittel_titel erste_Reihe)
        zuordnung (assoc {}
          :titles_haupttypen (erstelle-zuordnung titles (haupttypen rows))
          :titles (st/join ", " titles)
          :rec (titles_werte (rest rows) titles)
          :functions (title_id titles))]
      (spit "test2.txt"
      (sc/render-file "templates/recordversion" zuordnung))))

(defn first-row [dateiname sheetname]
  (let [workbook (dj/load-workbook (str dateiname))
        sheet (dj/select-sheet (str sheetname) workbook)
        columns (dj/select-columns {:A :name, :B :price, :C :wert} sheet)
        rows (dj/row-seq sheet)]
    (dj/sheet-name (first (dj/sheet-seq workbook)))
    ))

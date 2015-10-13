(ns table-to-b.core
  (:require [dk.ative.docjure.spreadsheet :as dj]
            [clojure.java.io :as io]
            [stencil.core :as sc]
            [clojure.string :as st])
  (:import de.be4.classicalb.core.parser.BParser
           de.be4.classicalb.core.parser.exceptions.BException)
  (:gen-class))

(def workbook
  "läd spreadsheet.xlsx"
  (dj/load-workbook "spreadsheet.xlsx"))

(def s
  "tabelle Price List"
  (dj/select-sheet "Price List" workbook))

(def datatype
  "Zuordnung Datentypnr -> Bezeichnung"
  {:0 "INTEGER" :1 "STRING" :2 "Formula" :3 "Blank" :4 "BOOLEAN" :5 "Error"})

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
  (let [typ (get-type el)]
    	(if (= typ 1)
        (str \" el \")
        (if (= typ 0)
          (int (dj/read-cell el))
          el))))

(defn row-as-tuple
  "Gibt die Daten einer Reihe als Tuple zurück
  angepasst nach Typ mit Anführungszeichen oder ohne"
  [row x]
  (let [elemente (st/join ", " (map element row))
        n (count (map element row))]
    (if (< n x)
      (loop [i 0 erg elemente]
        (if (<= (- x n) i)
          erg
          (recur (inc i) (str erg ", \"\""))))
      elemente)))

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

(defn haupttypen
  "Gibt die Datentypen der Reihen der Tabelle als list wieder"
  [titles data]
  (loop [i (count titles) erg []]
    (if (> 0 i )
      (reverse (rest erg))
      (recur (dec i) (conj erg (haupttyp (types-of-nth-column i data)))))))

(defn valid-b-identifier?
  "prüft, ob es sich bei dem übergebenen string um einen validen B-Identifier handelt"
  [string]
  (try
    (do  (BParser/parse (str "#FORMULA " string))
         true)
    (catch BException e false)))

(defn pruefe_titel
  "überprüft, ob es sich bei dem Titel um einen validen B-Identifier handelt,
  wenn nein, ersetzt alle Leerzeichen durch _ und prüft erneut
  wenn der Fehler immer noch nicht behoben wurde, wird das Programm geschlossen"
  [titel]
  (if (valid-b-identifier? titel)
    titel
    (if (valid-b-identifier? (st/replace titel " " "_"))
      (st/replace titel " " "_")
      ((println titel " ist kein gültiger B-Identifier, deshalb wird das Programm beendet.")
      (System/exit 0)))))

(defn ermittel_titel
  "prüft für alle Elemente des Vektors, ob es sich um einen gültigen Titel handelt
  und passt ihn ggf an"
  [titles]
  (map pruefe_titel titles))

(defn tupletype
  "Gehört zur tupleversion,
  Ermittelt die Werte für den tupletype-tag"
  [titles data]
  (st/join "*" (haupttypen titles data)))

(defn tuples
  "Gehört zur tupleversion,
  Ermittelt die Werte für den tuple-tag im tuples-tag"
  [rows n]
  (into []
    (for [row rows]
      {:tuple (row-as-tuple row n)})))

(defn tuplefunctions
  ""
  [titles rows]
  (loop [i 0 erg []]
    (if (= (count titles) i)
      erg
      (recur (inc i) (conj erg {:title (nth titles i) :id i})))))

(defn tuplemachine
  "Erstellt die Tuplemachine mit Hilfe des templates"
  [sheet]
  (let [rows (dj/row-seq sheet)
        erste_Reihe (map dj/read-cell (first rows))
        titles (ermittel_titel erste_Reihe)
        data (rest rows)
        zuordnung (assoc {}
          :tupletype (tupletype titles data)
          :titles (st/join ", " titles)
          :tuples (tuples data (count titles))
          :functions (tuplefunctions titles data)
          :vars (st/join ",x" (range 0 (count titles))))]
        (spit "test3.txt"
          (sc/render-file "templates/tupleversion" zuordnung))))

(defn zuordnung
  "Erstellt eine Zuordnung vom Muster name1:wert1,name2:wert2..."
  [namen werte]
  (subs (reduce str (interleave (repeat ",") namen (repeat ":") werte )) 1))

(defn rec
  "Gehört zur recordversion,
  Ermittelt die Werte für den titles_Werte-tag im rec-tag"
  [rows titles]
  (into []
    (for [row rows]
      { :titles_Werte (zuordnung titles (map element row)) } )))

(defn functions
  "Gehört zur recordversion,
  Ermittelt die Werte für den title-tag und den id-tag im functions-tag"
  [titles]
  (into []
    (for [title titles]
      (let [x (gensym "x")] {:title title :id x}))))

(defn titles_haupttypen
  "Gehört zur recordversion,
  Ermittelt die Werte für den titles_haupttypen-tag"
  [titles data]
  (zuordnung titles (haupttypen titles data)))

(defn recordmachine
  "erstellt die Recordmachine mit Hilfe des templates"
  [sheet]
  (let [rows (dj/row-seq sheet)
        erste_Reihe (map dj/read-cell (first rows))
        titles (ermittel_titel erste_Reihe)
        data (rest rows)
        zuordnung (assoc {}
          :titles_haupttypen (titles_haupttypen titles data)
          :titles (st/join ", " titles)
          :rec (rec data titles)
          :functions (functions titles))]
      (spit "test2.txt"
      (sc/render-file "templates/recordversion" zuordnung))))

(defn -main
  "ruft, je nach Parametern, die Methode zu tuple oder record erstellen auf"
  [version dateiname s1]
  (let [wb (dj/load-workbook dateiname)
        sheet (dj/select-sheet s1 wb)]
    (if (= version "record")
      (recordmachine s)
      (if (= version "tuple")
        (tuplemachine s)))))

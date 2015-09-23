(ns table-to-b.core
  (:gen-class))

(use 'dk.ative.docjure.spreadsheet)
(use 'clojure.java.io)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def workbook
  "läd spreadsheet.xlsx"
  (load-workbook "spreadsheet.xlsx"))

(def s
  "tabelle Price List"
  (select-sheet "Price List" workbook))

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
  (map (comp get-type (partial get-cell n)) (row-seq sheet)))

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
  (count (row-seq sheet)))

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
  "erstellt das Grundgerüst der Projektionsfunction für Tuples"
  [n]
  (let [vars (reduce str (interpose ",x" (range 0 (inc n))))]
    (str " = %(x" vars ").((x" vars "):TUPLETYPE|x")))

(defn tuples-functions
  "erstellt die projection functions für die Tupleversion"
  [row]
  (let [function (generate-function (count (map read-cell row)))]
    (loop [i 0 cells (map read-cell row) erg ""]
      (if (empty? cells)
        erg
        (recur (inc i) (rest cells) (str erg (first cells) function i ")" \newline))))))

(defn abstract-constants
  "erstellt den ABSTRACT_CONSTANTS-Teil"
  [row]
  (str "ABSTRACT_CONSTANTS " (reduce str (interpose ", " (map read-cell row))) \newline "CONSTANTS Excel" \newline ))

(defn erstelle-tuplemachine
  "erstellt die Tupleversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test.txt"
  [sheet]
  (let [rows (row-seq sheet)]
    (spit "test.txt"
      (str (dateibegin sheet)  \newline
      (abstract-constants (first rows)) \newline
      (tuple-properties rows) \newline
      (tuples-functions (first rows))))))

(defn record-data
  "Erstellt den Data-Teil der Recordversion"
  [sheet]
  (let [rows (row-seq sheet)
        title (map read-cell(first rows))]
    (loop [i 1 erg ""]
     (if (= i (count rows))
       erg
       (recur (inc i) (str erg "rec(" (subs (reduce str (interleave (repeat ",") title (repeat ":") (map element (nth rows i)))) 1) ")" \newline))))))

(defn generate-pow
  "Erstellt den POW-Teil der Recordversion"
  [sheet]
  (let [typen (haupttypen sheet)
        title (map read-cell(first (row-seq sheet)))]
    (loop [i 0 erg ""]
      (if (= i (count title))
        (subs erg 1)
        (recur (inc i) (str erg "," (nth title i) ":" (nth typen i)))))))

(defn record-properties
  "Erstellt den Properties Teil für Records"
  [sheet]
  (str "PROPERTIES" \newline "Excel : POW(struct(" (generate-pow sheet) "))"))

(defn record-functions
  "Erstellt die projection functions für die Recordversion"
  [sheet]
  (let [title (map read-cell(first (row-seq sheet)))]
    (loop [i 0 erg ""]
      (if (= i (count title))
        erg
        (recur (inc i) (str erg "&" \newline (nth title i) " = %x" i ".(x" i ":struct(" (generate-pow sheet) ")|x" i "'" (nth title i) ")" \newline))))))

(defn erstelle-recordmachine
  "erstellt die Recordversion einer b-machine aus der angegebenen Tabelle und speichert sie unter test1.txt"
  [sheet]
  (let [rows (row-seq sheet)]
    (spit "test1.txt"
    (str recanfang \newline
    (abstract-constants (first rows)) \newline
    (record-properties sheet) \newline
    "Excel = { /* the Data /*}" \newline
    (record-data sheet)
    (record-functions sheet)))))

(defn first-row [dateiname sheetname]
  (let [workbook (load-workbook (str dateiname))
        sheet (select-sheet (str sheetname) workbook)
        columns (select-columns {:A :name, :B :price, :C :wert} sheet)
        rows (row-seq sheet)]
    (sheet-name (first (sheet-seq workbook)))
    ))

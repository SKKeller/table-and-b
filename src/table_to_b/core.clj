(ns table-to-b.core
  (:gen-class))

(use 'dk.ative.docjure.spreadsheet)
(use 'clojure.java.io)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn test1 [x]
  (->> (load-workbook "spreadsheet.xlsx")
       (select-sheet "Price List")
       (select-columns {:B :element})))

(defn number-of-columns [first-row]
  (count (cell-seq first-row)))

(defn nth-column-name [n]
  (nth (map (comp keyword str) (map char (range 65 (+ 65 26)))) n))


(defn nth-column
  "Bekommt ein Spreadsheet, gibt die n. Reihe als Vektor von Hashmaps mit Keyword :element wieder."
  [n sheet]
  (let [name (nth-column-name n)]
    (select-columns {name name} sheet)))

                                        ; bekommt eine Reihe als List und die Anzahl der Elemente
                                        ; gibt ein tuple der Elemente wieder
(defn row-as-tuple [row]
  (loop [elements row erg ""]
    (if (empty? elements)
      (str "(" (subs erg 2) ")")
      (recur (rest elements) (str erg ", " (let [el (first elements)] (if (= (type el) java.lang.String) (str \" el \") el )))))))

                                        ; bekommt Reihen
                                        ; gibt einen String wieder, der die Reihen als Tuple wiedergibt, getrennt durch Zeilenumbrüche
(defn rows-as-tuples [rows]
  (loop [el rows tuple ""]
    (if (empty? el)
      tuple
      (recur (rest el) (str tuple \newline (row-as-tuple (map read-cell (first el))))))))

(defn get-cell [n row]
  (.getCell row n))

(defn get-type [cell]
  (when cell
    (.getCellType cell)))

(defn types-of-nth-column [n sheet]
  (map (comp get-type (partial get-cell n)) (row-seq sheet)))

(types-of-nth-column 0 s)

(defn first-row [dateiname sheetname]
  (let [workbook (load-workbook (str dateiname))
        sheet (select-sheet (str sheetname) workbook)
        columns (select-columns {:A :name, :B :price, :C :wert} sheet)
        rows (row-seq sheet)]
    (sheet-name (first (sheet-seq workbook)))
    (number-of-columns (first rows))
    (map :element (nth-column 1 sheet))
                                        ;(spit "test.txt" (rows-as-tuples rows))
    ))

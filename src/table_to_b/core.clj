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

; bekommt ein spreadsheet
; gibt die n. Reihe als Vektor von Hashmaps mit keyword :element wieder, z.Z. nur Reihen 1-3
(defn nth-column [n sheet]
  (select-columns (hash-map (keyword (nth (list 'A 'B 'C) n)) :element) sheet))

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

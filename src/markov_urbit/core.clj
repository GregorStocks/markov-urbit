(ns markov-urbit.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn merge-corpuses [corpuses]
  (apply merge-with concat corpuses))
(def lookahead 2)

(def start-paragraph :start)
(def end-paragraph :end)

(defn paragraph->corpus [paragraph]
  (let [words (concat
               (repeat lookahead start-paragraph)
               (str/split paragraph #"\s+")
               [end-paragraph])]
    (loop [offset lookahead
           corpus {}]
      (let [[start end] (split-at offset words)
            preceders (take-last lookahead start)]
        (if-let [next-word (first end)]
          (recur (inc offset)
                 (merge-corpuses [corpus {preceders [next-word]}]))
          corpus)))))

(defn corpus->paragraph [corpus]
  (let [words (loop [words (into [] (repeat lookahead start-paragraph))]
                (let [next-word (rand-nth (corpus (take-last lookahead words)))]
                  (if (or (= next-word end-paragraph)
                          (> (count words) 1000))
                    words
                    (recur (conj words next-word)))))]
    (str/join " " (drop lookahead words))))

(def filenames ["0-intro"
                "1-nock"
                "2-philosophy"
                "3-syntax"
                "4-semantics"])
(defn filename->corpus [filename]
  (let [contents (slurp (io/resource (str "book/" filename ".markdown")))
        ;; drop the header for now
        paragraphs (str/split contents #"\n\n")
        body (rest paragraphs)]
    (merge-corpuses (map paragraph->corpus body))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [corpus (merge-corpuses (map filename->corpus filenames))]
    (println (str/join "\n\n" (repeatedly 20 #(corpus->paragraph corpus))))))

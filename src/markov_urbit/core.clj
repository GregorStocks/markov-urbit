(ns markov-urbit.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn merge-corpuses [corpuses]
  (merge-with concat corpuses))

(def start-paragraph :start)
(def end-paragraph :end)

(defn paragraph->corpus [paragraph]
  (let [words (concat
               [start-paragraph]
               (str/split paragraph "\s+")
               [end-paragraph])]
    (into {} (map (fn [word follower]
                    [word [follower]])
                  words
                  (drop 1 words)))))

(defn corpus->paragraph [corpus]
  (str/join
   " "
   (loop [words []
          current-word start-paragraph]
     (if (= current-word end-paragraph)
       words
       (recur (conj words current-word)
              (pick (corpus current-word)))))))

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
    (str/join "\n\n" (repeat 20 #(corpus->paragraph corpus)))))

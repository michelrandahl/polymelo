(ns polymelo.core
  (:require [polymelo.typhon :as typhon]))

(def notes
  (map vector
       (mapcat (partial repeat 12) (range))
       (cycle [:c :c+ :d :d+ :e :f :f+ :g :g+ :a :a+ :b])))

(def base [[0 :c+] [0 :e] [0 :g+] [0 :d+]])

(def seq1 [0  0  0  0  7])

(def seq2 [12 0  0])

(defn note->semitone [note]
  (-> (take-while (comp nil? #{note}) notes)
      count
      (+ 1)))

(defn semitone->note [semitone]
  (-> (take semitone notes)
      last))

(defn get-notes
  "List all the notes that will occour in a poymeter modulated melody"
  [base & mod-seqs]
  (let [total-steps (apply * (count base) (map count mod-seqs))]
    (->> (apply (partial map +)
                (map note->semitone (cycle base))
                (map cycle mod-seqs))
         (take total-steps)
         (mapv semitone->note))))

(comment
 
 (in-ns 'polymelo.core)
 (user/rf)
 (semitone->note 1)
 (note->semitone [0 :c])
 (user/pp (sort (set (get-notes base seq1 seq2))))
 
 )

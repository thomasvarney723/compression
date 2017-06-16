;; (def d "TOBEORNOTTOBEORTOBEORNOT")
; (compress (map char (range 256)) "TOBEORNOTTOBEORTOBEORNOT")
;; '(84 79 66 69 79 82 78 79 84 256 258 260 265 259 261 263)

(defn lzw [dict [byte & bytes :as data] seq-val]
  (lazy-seq
   (when-let [s (seq data)]
     (let [[found not-found]
           (split-with #(get dict %)
                       (reductions conj [byte] bytes))
           last-found (last found)]
       (cons (get dict last-found)
             (lzw (assoc dict (first not-found) seq-val)
                  (drop (count last-found) s)
                  (inc seq-val)))))))

(defn delzw [dict [byte & bytes :as data] seq-val]
  (lazy-seq
   (when-let [s (seq data)]
     (let [[dfbyte dsbyte] (map #(get dict %) s)]
       (cons (get dict byte)
             (delzw (assoc dict seq-val (conj dfbyte (first dsbyte)))
                    bytes
                    (inc seq-val)))))))

(defn compress [char-seq string]
  (lzw (zipmap (map vector char-seq) (range))
       string
       (count char-seq)))

(defn decompress [char-seq string]
  (->> (delzw (zipmap (range) (map vector char-seq))
              string
              (count char-seq))
       (apply concat)
       (apply str)))

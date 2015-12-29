(ns parensymphony.core
  (use [overtone.live])
  (require [quil.core :as q]
           [quil.middleware :as m]))

(def window-width 1000)
(def window-height 1000)

(definst beep [freq 440]
  (-> freq
      saw
      (* (env-gen (perc) :action FREE))))

(definst ping [freq 440]
  (-> freq
      square
      (* (env-gen (perc) :action FREE))))

(definst seeth [freq 440 dur 1.0]
  (-> freq
      saw
      (* (env-gen (perc (* dur 1/2) (* dur 1/2)) :action FREE))))


(definst saw-wave [freq 1 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (saw (* freq 100))
     vol))

(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur) :action FREE) reverb)))

(def prog-55 [(chord :A2 :minor)
              (chord :F2 :major)
              (chord :G2 :major)
              (chord :A2 :minor)])

(defn play-chord [inst a-chord]
  (doseq [note a-chord] (inst note :dur 8)))

(defn play-chord-with-key [{:keys [chord-progression] :as state}]
  (play-chord plucked-string (first chord-progression))
  (assoc state :chord-progression (rest chord-progression)))

(defn play-with-key [{:keys [phrase] :as state}]
  (plucked-string (first phrase))
  (assoc state :phrase (rest phrase)))

(defn inc-index [index]
  (rem (inc index) 8))

(defn inc-chord-index [index]
  (rem (inc index) 4))

(defn make-code-unit [x y]
  {:start-x x :start-y y
   :text-size 40 :text-color [255]
   :cursor-color [0 255 0]
   :cursor-index 0 :code ""})

(defn setup []
  (q/smooth)
  (q/background 0)
  {:pressed-key ""
   :chord-progression (cycle prog-55)
   :key-index 3 :phrase (get-penta-phrase 3)
   :code-unit-index 0 :code-list [(make-code-unit 20 100) (make-code-unit 20 200)]
   :pressing-ctr? false :pressing-alt? false})

(defn display-cursor [{:keys [start-x start-y text-size cursor-index code cursor-color]}]
  (let [c (map count (clojure.string/split code #"\n"))
        [cur-line-index line-count str-num-to-cursor line-breaks]
        (loop [i 0 acc 0 line-breaks ""]
          (if (and (< i (count c)) (> (- cursor-index i) (+ (nth c i) acc)))
            (recur (inc i) (+ acc (nth c i)) (str "\n" line-breaks))
            [(- cursor-index acc i) i acc line-breaks]))
        x (+ start-x (q/text-width (subs code (+ str-num-to-cursor line-count) cursor-index)))]
    (apply q/fill cursor-color)
    (q/text (str line-breaks "|") (- x (/ (q/text-width "|") 2)) start-y)))

(defn display-code [{:keys [start-x start-y code text-size text-color]}]
  (q/text-size text-size)
  (apply q/fill text-color)
  (q/text code start-x start-y))

(defn draw [state]
  (q/background 0)
  (q/fill 255)
  (doseq [code-unit (:code-list state)]
    (display-code code-unit)
    (display-cursor code-unit))
  (q/text-size 20)
  (if (:pressing-ctr? state)
    (q/text "pressing ctr" 300 300))
  (if (:pressing-alt? state)
    (q/text "pressing alt" 300 350))
  (q/text (str (:result (nth (:code-list state) (:code-unit-index state)))) 300 400)
  (q/text (str (:error (nth (:code-list state) (:code-unit-index state)))) 300 500))

(defn cursor-move-right [{:keys [cursor-index code] :as code-unit}]
  (if (< cursor-index (count code))
    (assoc code-unit :cursor-index (inc cursor-index))
    code-unit))

(defn cursor-move-left [{:keys [cursor-index] :as code-unit}]
  (if (not= cursor-index 0)
    (assoc code-unit :cursor-index (dec cursor-index))
    code-unit))

(defn split-by-index
    [strg idx] [(subs strg 0 idx) (subs strg idx (count strg))])

(defn insert-char [{:keys [code cursor-index] :as code-unit} char]
  (let [[left right] (split-by-index code cursor-index)]
    (assoc code-unit :code (str left char right))))

(defn delete-backward-char [{:keys [code cursor-index] :as code-unit}]
  (if (or (<= cursor-index 0) (> cursor-index (count code)))
    code-unit
    (-> code-unit
        (assoc :code (str (subs code 0 (dec cursor-index))
                          (subs code cursor-index (count code))))
        (cursor-move-left))))

(defn insert-paren [{:keys [code cursor-index] :as code-unit}]
  (let [[left right] (split-by-index code cursor-index)]
    (assoc code-unit :code (str left "()" right))))

(defn insert-bracket [{:keys [code cursor-index] :as code-unit}]
  (let [[left right] (split-by-index code cursor-index)]
    (assoc code-unit :code (str left "[]" right))))

(def special-key-code-dic (hash-map 8 :backspace
                                    9 :tab
                                    10 :enter))

(defn make-key-state []
  {:raw-key (q/raw-key)
   :key-code (q/key-code)
   :key-as-keyword-str (str (special-key-code-dic (q/key-code)
                                                  (q/key-as-keyword)))})

(defmulti key-pressed-functions (fn [key-state state]
                                  (:key-as-keyword-str key-state)))

(defn update-code [{:keys [code-list code-unit-index] :as state} f & arg]
  (let [code-unit (nth code-list code-unit-index)]
    (assoc state :code-list (assoc code-list code-unit-index (apply f code-unit arg)))))

(defmethod key-pressed-functions ":(" [key-state
                                       {:keys [code-list code-unit-index] :as state}]
  (-> state
      (play-chord-with-key)
      (update-code insert-paren)
      (update-code cursor-move-right)))

(defmethod key-pressed-functions ":[" [key-state
                                       {:keys [code-list code-unit-index] :as state}]
  (-> state
        (play-chord-with-key)
        (update-code insert-bracket)
        (update-code cursor-move-right)))

(defmethod key-pressed-functions ": " [key-state
                                       {:keys [code-list code-unit-index] :as state}]
  (-> state
      (assoc :phrase (get-penta-phrase (:key-index state)))
      (play-chord-with-key)
      (update-code insert-char (q/raw-key))
      (update-code cursor-move-right)))

(defmethod key-pressed-functions ":shift" [key-state state]
  state)

(defmethod key-pressed-functions ":right" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (plucked-string (note :E3) :dur 8)
  (-> state
      (update-code cursor-move-right)))

(defmethod key-pressed-functions ":left" [key-state
                                          {:keys [code-list code-unit-index] :as state}]
  (plucked-string (note :F3) :dur 8)
  (-> state
      (update-code cursor-move-left)))

(defmethod key-pressed-functions ":up" [key-state
                                        {:keys [code-list code-unit-index] :as state}]
  (assoc state :code-unit-index (mod (dec code-unit-index) (count code-list))))

(defmethod key-pressed-functions ":down" [key-state
                                        {:keys [code-list code-unit-index] :as state}]
  (assoc state :code-unit-index (mod (inc code-unit-index) (count code-list))))

(defmethod key-pressed-functions ":control" [key-state state]
  (assoc state :pressing-ctr? true))

(defmethod key-pressed-functions ":alt" [key-state state]
  (assoc state :pressing-alt? true))

(defn delete-all [code-unit]
  (assoc code-unit :code "" :cursor-index 0))

(defmethod key-pressed-functions ":backspace" [key-state
                                               {:keys [code-list code-unit-index] :as state}]
  (if (:pressing-ctr? state)
    (update-code state delete-all)
    (do (-> state
            (play-with-key)
            (update-code delete-backward-char)))))

(defn eval-code [code-unit]
  (try (let [sexp (read-string (:code code-unit))
             result (binding [*ns* (find-ns 'parensymphony.core)] (eval sexp))]
         (assoc code-unit :result result :error ""))
       (catch RuntimeException ex (assoc code-unit
                                         :result ""
                                         :error (.getMessage ex)))))

(defmethod key-pressed-functions ":enter" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (let [code-unit (nth code-list code-unit-index)]
    (if (:pressing-ctr? state)
      (update-code state eval-code)
      (do (-> state
              (update-code insert-char "\n")
              (update-code cursor-move-right))))))

(defmethod key-pressed-functions ":tab" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (-> state
      (update-code insert-char "  ")
      (update-code cursor-move-right)
      (update-code cursor-move-right)))


(defmethod key-pressed-functions :default [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (let [code-unit (nth code-list code-unit-index)]
    (-> state
        (play-with-key)
        (update-code insert-char (q/raw-key))
        (update-code cursor-move-right))))

(defmulti key-released-functions (fn [key-state state]
                                   (:key-as-keyword-str key-state)))

(defmethod key-released-functions ":control" [key-state state]
  (assoc state :pressing-ctr? false))

(defmethod key-released-functions ":alt" [key-state state]
  (assoc state :pressing-alt? false))

(defmethod key-released-functions :default [key-state state]
  state)

(defn key-pressed [state event]
  (let [key-state (make-key-state)]
    (println key-state)
    (key-pressed-functions key-state state)))

(defn key-released [state]
  (let [key-state (make-key-state)]
    (key-released-functions key-state state)))

(defn symphony-read [code]
  (try (read-string code)
       (catch RuntimeException ex nil)))

(defn symphony-eval [sexp]
  (try (eval sexp)
       (catch RuntimeException ex nil)))

(defn start []
  (q/defsketch parensymphony
    :title "symphony"
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :key-released key-released
    :renderer :p2d
    :middleware [m/fun-mode m/pause-on-error]
    :size [window-width window-height]))

(def char-keycode-map
  (hash-map :0 48 :1 49 :2 50 :3 51 :4 52 :5 53 :6 54 :7 55 :8 56 :9 57
            :a 65 :b 66 :c 67 :d 68 :e 69 :f 70 :g 71 :h 72 :i 73 :j 74
            :k 75 :l 76 :m 77 :n 78 :o 79 :p 80 :q 81 :r 82 :s 83 :t 84
            :u 85 :v 86 :w 87 :x 88 :y 89 :z 90))

(defn char->keycode [char]
  (char-keycode-map (keyword (str char)) 55))


(defn penta-scale [root]
  (let [penta (scale root :pentatonic)]
    (concat penta (reverse penta))))

(def phrase-dic
  (let [penta2 (penta-scale :c2)
        penta3 (penta-scale :c3)
        penta4 (penta-scale :c4)]
    (hash-map 2 (for [n (range 1 4)]
                  (cycle (take-nth n penta2)))
              3 (for [n (range 1 4)]
                  (cycle (take-nth n penta3)))
              4 (for [n (range 1 4)]
                  (cycle (take-nth n penta3))))))

(defn get-penta-phrase [key-index]
  (nthrest (choose (phrase-dic key-index (phrase-dic 3)))
           (int (* (rand) 16))))

(defn gen-pattern [key-index n]
  (cond
    (or (number? n) (symbol? n) (keyword? n)) (take (count (str n)) (get-penta-phrase key-index))
    (empty? n) nil
    :else (concat (gen-pattern key-index (first n))
                  '(rest)
                  (gen-pattern key-index (rest n)))))

(defn play [time sep notes]
  (let [note (first notes)]
    (cond
      (= note 'rest) (at time)
      (seq? note) (at time (play-chord plucked-string note))
      :else (at time (plucked-string note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time sep (rest notes)]))))



(def playing? false)
(defn play-loop []
  (play (now) 300 (gen-pattern 2 '(definst ping [freq 440]
                                      (-> freq
                                          square
                                          (* (env-gen (perc) :action FREE)))))))
(play-loop)
(println playing?)
(comment
  ;;'(((((((((Lisp)))))))))
  ;;program = data = score
  ;;S expression -> phrase
  (let []
    (play (now) 300 (gen-pattern 2 '(definst ping [freq 440]
                                      (-> freq
                                          square
                                          (* (env-gen (perc) :action FREE))))))
    (play (now) 300 (gen-pattern 3 '(definst beep [freq 440]
                                      (-> freq
                                          saw
                                          (* (env-gen (perc) :action FREE))))))
    (play (now) 300 (gen-pattern 4 '(defn key-pressed [state event]
                                      (let [key-state (make-key-state)]
                                        (println key-state)
                                        (key-pressed-functions key-state state))))))
  (stop)
  (take 10 (cycle (flatten prog-55)))
  (start)
  (kill plucked-string)
  (let [patterns [(scale :C3 :pentatonic)
                  (scale :D3 :pentatonic)
                  (scale :E3 :pentatonic)
                  (scale :F3 :pentatonic)
                  (scale :G3 :pentatonic)
                  (scale :A3 :pentatonic)
                  (scale :B3 :pentatonic)]]
                                        ;    (play (now) (flatten patterns) 100)
                                        ;    (play (now) (nth phrases 2) 100)
                                        ;    (play (now) (take-nth 2 (flatten (reverse patterns))) 100)
    (play (now) 300 (flatten (repeat 10
                                 [(nth (scale :C2 :pentatonic) 1)
                                  (nth (scale :C2 :pentatonic) 3)])))
    (play (now) 300 (flatten (repeat 10
                                 [(nth (scale :C3 :pentatonic) 3)
                                  (nth (scale :C3 :pentatonic) 4)])))
    (play (now) 300 (flatten (repeat 10
                                 [(nth (scale :C4 :pentatonic) 5)
                                  (nth (scale :C4 :pentatonic) 6)])))
                                        ; (play (now) (take-nth 2 (flatten (map (fn [x] (choose patterns)) patterns))) 100)
                                        ; (play (now) (take-nth 2 (flatten (map (fn [x] (choose patterns)) patterns))) 100)
    )  )
(comment
  (let []
    (play (now) 300 (take 30 (nthrest (cycle (take-nth 3 (concat (scale :C2 :pentatonic)
                                                                 (reverse (scale :c2 :pentatonic)))))
                                      (int (* (rand) 6)))))
    (play (now) 300 (take 30 (nthrest (cycle (take-nth 2 (concat (scale :C3 :pentatonic)
                                                                 (reverse (scale :c3 :pentatonic)))))
                                      (int (* (rand) 6)))))
    (:id (play (now) 300 (take 30 (nthrest (cycle (take-nth 4 (concat (scale :C4 :pentatonic)
                                                                        (reverse (scale :c4 :pentatonic)))))
                                             (int (* (rand) 6)))))))
  (kill-player)
  (let []
    (play (now) 300 (concat (get-pattern 3 2 :c2)
                        '(rest)
                        (get-pattern 4 3 :c2)
                        '(rest)
                        (get-pattern 5 3 :c2)))
    (play (now) 300 (concat (get-pattern 4 2 :c3)
                        '(rest)
                        (get-pattern 4 2 :c3)
                        '(rest )
                        (get-pattern 4 3 :c3)))
    (play (now) 300 (concat (get-pattern 5 4 :c4)
                            '(rest)
                            (get-pattern 6 2 :c4)
                            '(rest)
                            (get-pattern 3 2 :c4))))
  (play (now) 300 (concat (scale :c3 :pentatonic) '(rest) (scale :c3 :pentatonic))))

(defn get-pattern [n th root]
  (take n (nthrest (cycle (take-nth th (concat (scale root :pentatonic)
                                               (reverse (scale root :pentatonic)))))
                   (int (* (rand) 6)))))

(defn fizzbuzz [x]
  (cond (= (rem x 15) 0) "fizzbuzz"
        (= (rem x 5)  0) "buzz"
        (= (rem x 3)  0) "fizz"
        :else x))
(start)

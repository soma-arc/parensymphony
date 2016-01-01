(ns parensymphony.core
  (use [overtone.live])
  (require [quil.core :as q]
           [quil.middleware :as m]))

(def taiko1 (sample "resources/taiko1.wav"))
(def taiko2 (sample "resources/taiko2.wav"))
(def taiko3 (sample "resources/taiko3.wav"))
(def taiko4 (sample "resources/taiko4.wav"))

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

(def chord-dic
  (let [penta1 (scale :c1 :pentatonic)
        penta2 (scale :c2 :pentatonic)
        penta3 (scale :c3 :pentatonic)
        penta4 (scale :c4 :pentatonic)
        penta5 (scale :c5 :pentatonic)
        penta6 (scale :c6 :pentatonic)
        gen (fn [scales] (for [m (range 6)]
                          (for [n (range m (+ m 3))]
                            (nth scales n))))]
    (hash-map 1 (gen penta1)
              2 (gen penta2)
              3 (gen penta3)
              4 (gen penta4)
              5 (gen penta5)
              6 (gen penta6)
              7 (gen penta6))))

(defn penta-scale [root]
  (let [penta (scale root :pentatonic)]
    (concat penta (reverse penta))))

(def phrase-dic
  (let [penta1 (penta-scale :c1)
        penta2 (penta-scale :c2)
        penta3 (penta-scale :c3)
        penta4 (penta-scale :c4)
        penta5 (penta-scale :c5)
        penta6 (penta-scale :c6)]
    (hash-map 1 (for [n (range 1 5)]
                  (cycle (take-nth n penta1)))
              2 (for [n (range 1 5)]
                  (cycle (take-nth n penta2)))
              3 (for [n (range 1 5)]
                  (cycle (take-nth n penta3)))
              4 (for [n (range 1 5)]
                  (cycle (take-nth n penta4)))
              5 (for [n (range 1 5)]
                  (cycle (take-nth n penta5)))
              6 (for [n (range 1 5)]
                  (cycle (take-nth n penta6)))
              7 (for [n (range 1 5)]
                  (cycle (take-nth n penta6))))))

(defn get-penta-phrase [key-index]
  (nthrest (choose (phrase-dic key-index (phrase-dic 3)))
           (int (* (rand) 16))))

(defn gen-pattern [key-index n]
  (cond
    (or (number? n) (symbol? n)
        (keyword? n) (char? n)) (take (count (str n)) (get-penta-phrase key-index))
    (empty? n) nil
    :else (concat (gen-pattern key-index (first n))
                  '(rest)
                  (gen-pattern key-index (rest n)))))

(defn play-chord [inst a-chord]
  (doseq [note a-chord] (inst note :dur 12)))

(defn play [time sep notes]
  (let [note (first notes)]
    (cond
      (= note 'rest) (at time)
      (= note 'taiko) (at time (taiko4))
      (= note 'taiko-w) (at time (taiko2))
      (seq? note) (at time (play-chord plucked-string note))
      :else (at time (plucked-string note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time sep (rest notes)]))))

(defn play-start [{:keys [code-list] :as state}
                  code-unit-index]
  (let [{:keys [code last-play?] :as code-unit} (nth code-list code-unit-index)
        key-index (+ code-unit-index 2)
        pattern (try (gen-pattern key-index (read-string code))
                     (catch RuntimeException ex '(rest)))
        step-millis 300
        length (* step-millis (count pattern))]
    (if (nil? pattern)
      state
      (do
        (if last-play?
          (play (now) step-millis  pattern)
          (play (now) step-millis (cycle pattern)))
        (assoc state
               :code-list (assoc code-list code-unit-index
                                 (assoc code-unit
                                        :playing-millis 0
                                        :playing? (not last-play?)
                                        :end-millis length)))))))

(defn play-chord-with-key [{:keys [chord-progression] :as state}]
  (play-chord plucked-string (choose chord-progression))
  (assoc state :chord-progression  chord-progression))

(defn play-with-key [{:keys [phrase] :as state}]
  (plucked-string (first phrase))
  (assoc state :phrase (rest phrase)))

(defn inc-index [index]
  (rem (inc index) 8))

(defn inc-chord-index [index]
  (rem (inc index) 4))

(def +code-margin-left+ 20)

(defn make-code-unit [x y width height]
  (let [text-size 37]
    (q/text-size text-size)
    {:start-x x :start-y y
     :code-start-x (+ x +code-margin-left+)
     :code-start-y (+ y text-size)
     :text-size text-size :text-color [255]
     :selected-bg-color [50]
     :cursor-color [0 255 0]
     :result "" :result-color [0 255 0]
     :error "" :error-color [255 0 0]
     :width width :height height
     :cursor-index 0 :code ""
     :playing-millis 0 :playing? false :end-millis 0
     :last-play? false}))

(defn make-repl-pane [x y width height]
  (let [message "parensymphony> "
        code-unit (make-code-unit x y width height)]
    (assoc code-unit
           :message message
           :code-start-x (+ x +code-margin-left+ (q/text-width message)))))

(defn setup []
  (q/smooth)
  (q/background 0)
  {:pressed-key ""
   :key-index 2
   :chord-progression (chord-dic 3)
   :phrase (get-penta-phrase 2)
   :code-unit-index 0
   :repl-index 4
   :code-list [(make-code-unit 0 50
                               (/ (q/screen-width) 2) 350)
               (make-code-unit 0 400
                               (/ (q/screen-width) 2) 350)
               (make-code-unit (/ (q/screen-width) 2) 50
                               (/ (q/screen-width) 2) 350)
               (make-code-unit (/ (q/screen-width) 2) 400
                               (/ (q/screen-width) 2) 350)
               (make-repl-pane 0 750
                               (q/screen-width) (- (q/screen-height) 750))]
   :pressing-ctr? false :pressing-alt? false
   :start-millis 0})

(defn update-state [{:keys [code-list] :as state}]
  state)


(defn display-cursor [{:keys [code-start-x code-start-y text-size cursor-index code cursor-color]}]
  (let [c (map count (clojure.string/split code #"\n"))
        [cur-line-index line-count str-num-to-cursor line-breaks]
        (loop [i 0 acc 0 line-breaks ""]
          (if (and (< i (count c)) (> (- cursor-index i) (+ (nth c i) acc)))
            (recur (inc i) (+ acc (nth c i)) (str "\n" line-breaks))
            [(- cursor-index acc i) i acc line-breaks]))
        x (+ code-start-x (q/text-width (subs code (+ str-num-to-cursor line-count) cursor-index)))]
    (apply q/fill cursor-color)
    (q/text (str line-breaks "|") (- x (/ (q/text-width "|") 2)) code-start-y)))

(defn fill-selected-bg [{:keys [start-x start-y width height selected-bg-color]
                        :as code-unit}]
  (q/with-fill selected-bg-color
    (q/rect start-x start-y
            width height)))

(defn display-code-pane [{:keys [start-x start-y code-start-x code-start-y width height
                                 code text-size text-color selected-bg-color result error
                                 result-color error-color]
                     :as code-unit}
                    index code-unit-index]
  (if (= index code-unit-index) (fill-selected-bg code-unit))
  (q/text-size text-size)
  (q/with-fill text-color
    (q/text code code-start-x code-start-y))
  (display-cursor code-unit)
  (if (not= "" result)
    (q/with-fill result-color
      (q/text (str "-> " result) code-start-x (- (+ start-y height) 10))))
  (if (not= "" error)
    (q/with-fill error-color
      (q/text (str "-> " error) code-start-x (- (+ start-y height) 10)))))

(defn display-repl [{:keys [start-x start-y code-start-x code-start-y width height
                            code text-size text-color selected-bg-color]
                     :as code-unit}
                    index code-unit-index]
  (if (= index code-unit-index) (fill-selected-bg code-unit))
  (q/text-size text-size)
  (q/with-fill text-color
    (q/text code code-start-x code-start-y)
    (q/text (:message code-unit) (+ +code-margin-left+ start-x) code-start-y))
  (display-cursor code-unit))

(defn display-header [{:keys [code-unit-index repl-index]}]
  (q/with-fill [255]
    (q/text-size 25)
    (q/text "(((((((((Parensymphony)))))))))" 10 30)
    (if (= code-unit-index repl-index)
      (q/text "(current-pane) -> REPL"
              (- (/ (q/screen-width) 2) 150) 30)
      (q/text (str "(current-pane) -> pane-" code-unit-index)
              (- (/ (q/screen-width) 2) 150) 30 30))))

(defn draw [{:keys [code-list code-unit-index pressing-ctr? repl-index] :as state}]
  (q/background 0)
  (display-header state)
  (doseq [[code-unit index] (map list code-list (range (count code-list)))]
    (if (= repl-index index)
      (display-repl code-unit index code-unit-index)
      (display-code-pane code-unit index code-unit-index)))
  (q/with-stroke [255]
    (q/line 0 50
            (q/screen-width) 50)
    (q/line (/ (q/screen-width) 2) 50
            (/ (q/screen-width) 2) 750)
    (q/line 0 400
            (q/screen-width) 400)
    (q/line 0 750
            (q/screen-width) 750)))

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
                                    10 :enter
                                    80 :p))

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
                                       {:keys [code-list code-unit-index key-index] :as state}]
  (-> state
      (play-chord-with-key)
      (assoc :phrase (get-penta-phrase key-index))
      (update-code insert-char (q/raw-key))
      (update-code cursor-move-right)))

(defmethod key-pressed-functions ":shift" [key-state state]
  state)

(defmethod key-pressed-functions ":right" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (-> state
      (play-with-key)
      (update-code cursor-move-right)))

(defmethod key-pressed-functions ":left" [key-state
                                          {:keys [code-list code-unit-index] :as state}]
  (-> state
      (play-with-key)
      (update-code cursor-move-left)))

(defmethod key-pressed-functions ":up" [key-state
                                        {:keys [code-list code-unit-index] :as state}]
  (let [code-unit-index (mod (dec code-unit-index) (count code-list))
        key-index (+ 2 code-unit-index)]
    (assoc state
           :code-unit-index code-unit-index
           :key-index key-index
           :phrase (get-penta-phrase key-index)
           :chord-progression (chord-dic (+ 1 key-index)))))

(defmethod key-pressed-functions ":down" [key-state
                                          {:keys [code-list code-unit-index] :as state}]
  (let [code-unit-index (mod (inc code-unit-index) (count code-list))
        key-index (+ 2 code-unit-index)]
    (assoc state
           :code-unit-index code-unit-index
           :key-index key-index
           :phrase (get-penta-phrase key-index)
           :chord-progression (chord-dic (+ 1 key-index)))))

(defmethod key-pressed-functions ":control" [key-state state]
  (assoc state :pressing-ctr? true))

(defn kill-pattern [code-unit]
  (assoc code-unit :last-play? true))

(defn restart-all [{:keys [code-list code-unit-index start-millis] :as state}]
  (stop)
  (assoc state
         :start-millis (now)
         :code-list
         (loop [i 0 new-code-list code-list]
           (if (< i (count code-list))
             (recur (inc i) (assoc new-code-list i
                                   (let [{:keys [playing?] :as code-unit} (nth code-list i)]
                                     (if playing?
                                       (nth (:code-list (play-start state i)) i)
                                       code-unit))))
             new-code-list))))

(defmethod key-pressed-functions ":p" [key-state {:keys [code-list code-unit-index] :as state}]
  (if (:pressing-ctr? state)
    (-> state
        (update-code kill-pattern)
        (restart-all))
    (-> state
        (play-with-key)
        (update-code insert-char (q/raw-key))
        (update-code cursor-move-right))))

(defmethod key-pressed-functions ":alt" [key-state state]
  (-> state
      (assoc :pressing-alt? true)))

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
             result (binding [*ns* (find-ns 'parensymphony.core)]
                      (eval sexp))]
         (assoc code-unit :result result :error ""))
       (catch RuntimeException ex (assoc code-unit
                                         :result ""
                                         :error (.getMessage ex)))))

(defmethod key-pressed-functions ":enter" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (let [code-unit (nth code-list code-unit-index)]
    (if (:pressing-ctr? state)
      (-> state
          (update-code eval-code)
          (restart-all)
          (play-start code-unit-index))
      (-> state
          (play-chord-with-key)
          (update-code insert-char "\n")
          (update-code cursor-move-right)))))

(defmethod key-pressed-functions ":tab" [key-state
                                           {:keys [code-list code-unit-index] :as state}]
  (-> state
      (play-chord-with-key)
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
    :update update-state
    :key-pressed key-pressed
    :key-released key-released
    :features [:present
               :keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]
    :size :fullscreen))

(def char-keycode-map
  (hash-map :0 48 :1 49 :2 50 :3 51 :4 52 :5 53 :6 54 :7 55 :8 56 :9 57
            :a 65 :b 66 :c 67 :d 68 :e 69 :f 70 :g 71 :h 72 :i 73 :j 74
            :k 75 :l 76 :m 77 :n 78 :o 79 :p 80 :q 81 :r 82 :s 83 :t 84
            :u 85 :v 86 :w 87 :x 88 :y 89 :z 90))

(defn char->keycode [char]
  (char-keycode-map (keyword (str char)) 55))

(defn play-loop []
  (play (now) 300 (gen-pattern 2 '(definst ping [freq 440]
                                      (-> freq
                                          square
                                          (* (env-gen (perc) :action FREE)))))))

(comment
  ;;'(((((((((Lisp)))))))))
  ;;program = data = score
  ;;S expression -> phrase
  (let []
    (stop)
    (play (now) 300  (gen-pattern 1 '(definst ping [freq 440]
                                         (-> freq
                                             square
                                             (* (env-gen (perc) :action FREE))))))
    (play (now) 300  (gen-pattern 2 '(definst ping [freq 440]
                                         (-> freq
                                             square
                                             (* (env-gen (perc) :action FREE))))))
    (play (now) 300  (gen-pattern 3 '(definst beep [freq 440]
                                         (-> freq
                                             saw
                                             (* (env-gen (perc) :action FREE))))))
    (play (now) 300 (gen-pattern 4 '(defn key-pressed [state event]
                                      (let [key-state (make-key-state)]
                                        (println key-state)
                                        (key-pressed-functions key-state state)))))
    (play (now) 300 (gen-pattern 5 '(definst saw-wave [freq 1 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
                                      (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
                                         (saw (* freq 100))
                                         vol))))
    (play (now) 300 (fizzbuzz-seq 60)))
  (stop)
  (start)
  )



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
                                           (int (* (rand) 6))))))
    (play (now) 300 (flatten (map fizzbuzz (range 100)))))

  (let []
    (play (now) 300 (concat (get-pattern 3 2 :c5)
                        '(rest)
                        (get-pattern 4 3 :c5)
                        '(rest)
                        (get-pattern 5 3 :c5)))
    (play (now) 300 (concat (get-pattern 4 2 :c3)
                        '(rest)
                        (get-pattern 4 2 :c3)
                        '(rest )
                        (get-pattern 4 3 :c3)))
    (play (now) 300 (concat (get-pattern 5 4 :c6)
                            '(rest)
                            (get-pattern 6 2 :c6)
                            '(rest)
                            (get-pattern 3 2 :c6))))
  (play (now) 300 (concat (scale :c3 :pentatonic) '(rest) (scale :c3 :pentatonic))))

(defn get-pattern [n th root]
  (take n (nthrest (cycle (take-nth th (concat (scale root :pentatonic)
                                               (reverse (scale root :pentatonic)))))
                   (int (* (rand) 6)))))

(defn fizzbuzz [x]
  (cond (= (rem x 15) 0) '(taiko taiko-w)
        (= (rem x 5)  0) 'taiko
        (= (rem x 3)  0) 'taiko-w
        :else 'rest))

(defn fizzbuzz-seq [len]
  (flatten (map fizzbuzz (range len))))

(defn play-inst []
  (play (now) 300 (fizzbuzz-seq 100)))


(defn fizzbuzz2 [x]
  (cond (= (rem x 35) 0) '(kotudumi rest)
        (= (rem x 5)  0) '(kotudumi)
        (= (rem x 7)  0) '(rest)
        :else '(rest)))

(defn gen-seq [num]
  (flatten (repeat num (for [x (range 11)]
                        (cond (= x 0) 'kotudumi
                              (= x 10) 'clappers
                              :else 'rest)))))

(defn tudumi-seq [len]
  (flatten (map (fn [x] (if (= (rem x 10) 0)
                         'kotudumi
                         'rest))
                (range len))))

(defn fibs [a b] (cons a (lazy-seq (fibs b (+' a b)))))

(start)
(stop)

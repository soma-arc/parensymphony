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

(defn play-chord-with-key [{:keys [chord-index chord-root chord-name]}]
  (play-chord plucked-string (nth prog-55 chord-index)))

(defn play-with-key [{:keys [scale-index scale-root scale-name]}]
  (plucked-string (nth (scale scale-root scale-name) scale-index)))

(defn inc-index [index]
  (rem (inc index) 8))

(defn inc-chord-index [index]
  (rem (inc index) 4))

(defn setup []
  (q/smooth)
  (q/background 0)
  {:pressed-key ""
   :scale-index 0 :scale-root :E3 :scale-name :phrygian
   :chord-index 0 :chord-root :C3 :chord-name :major
   :cursor-index 0 :code ""
   :pressing-ctr? false})

(defn draw [state]
  (q/background 0)
  (q/fill 255)
  (q/text-size 20)
  (q/text (str (:pressed-key state)) 20 20)
  (q/text (:code state) 20 100)
  (if (:pressing-ctr? state)
    (q/text "pressing ctr" 300 300)))

(defn cursor-move-right [cursor-index]
  (inc cursor-index))

(defn cursor-move-left [cursor-index]
  (dec cursor-index))

(defn split-by-index
    [strg idx] [(subs strg 0 idx) (subs strg idx (count strg))])

(defn insert-char [code char cursor-index]
  (let [[left right] (split-by-index code cursor-index)]
    (str left char right)))

(defn delete-backward-char [{:keys [code cursor-index] :as state}]
  (if (or (<= cursor-index 0) (> cursor-index (count code)))
    state
    (assoc state
           :code (str (subs code 0 (dec cursor-index)) (subs code cursor-index (count code)))
           :cursor-index (cursor-move-left cursor-index))))

(defn insert-paren [code cursor-index]
  (let [[left right] (split-by-index code cursor-index)]
    (str left "()" right)))

(defn insert-bracket [code cursor-index]
  (let [[left right] (split-by-index code cursor-index)]
    (str left "[]" right)))

(def special-key-code-dic (hash-map 8 :delete))

(defn make-key-state []
  {:raw-key (q/raw-key)
   :key-code (q/key-code)
   :key-as-keyword-str (str (special-key-code-dic (q/key-code)
                                                 (q/key-as-keyword)))})

(defmulti key-pressed-functions (fn [key-state state]
                                  (:key-as-keyword-str key-state)))

(defmethod key-pressed-functions ":(" [key-state state]
  (play-chord-with-key state)
  (-> state
      (update-in [:chord-index] inc-chord-index)
      (assoc :code (insert-paren (:code state) (:cursor-index state)))
      (update-in [:cursor-index] cursor-move-right)))

(defmethod key-pressed-functions ":[" [key-state state]
  (play-chord-with-key state)
  (-> state
      (update-in [:chord-index] inc-chord-index)
      (assoc :code (insert-bracket (:code state) (:cursor-index state)))
      (update-in [:cursor-index] cursor-move-right)))

(defmethod key-pressed-functions ":[" [key-state state]
  (play-chord-with-key state)
  (-> state
      (update-in [:chord-index] inc-chord-index)
      (assoc :code (insert-paren (:code state) (:cursor-index state)))
      (update-in [:cursor-index] cursor-move-right)))

(defmethod key-pressed-functions ": " [key-state state]
  (play-chord-with-key state)
  (-> state
      (update-in [:chord-index] inc-chord-index)
      (assoc  :code (insert-char (:code state) (q/raw-key) (:cursor-index state)))
      (update-in [:cursor-index] cursor-move-right)))

(defmethod key-pressed-functions ":shift" [key-state state]
  state)

(defmethod key-pressed-functions ":right" [key-state state]
  (update-in state [:cursor-index] cursor-move-right))

(defmethod key-pressed-functions ":left" [key-state state]
  (update-in state [:cursor-index] cursor-move-left))

(defmethod key-pressed-functions ":control" [key-state state]
  (assoc state :pressing-ctr? true))

(defmethod key-pressed-functions ":delete" [key-state state]
  (play-with-key state)
  (update-in state [:scale-index] inc-index)
  (delete-backward-char state))

(defmethod key-pressed-functions :default [key-state state]
  (println (:key-as-keyword-str key-state))
  (println (keyword?  (:key-as-keyword-str key-state)))
  (println (string? (:key-as-keyword-str key-state)))

  (play-with-key state)
  (-> state
      (update-in [:scale-index] inc-index)
      (assoc  :code (insert-char (:code state)
                                 (q/raw-key)
                                 (:cursor-index state)))
      (update-in [:cursor-index] cursor-move-right)))

(defmulti key-released-functions (fn [key-state state]
                                   (:key-as-keyword-str key-state)))

(defmethod key-released-functions ":control" [key-state state]
  (assoc state :pressing-ctr? false))

(defmethod key-released-functions :default [key-state state]
  state)

(defn key-pressed [state event]
  (let [key-state (make-key-state)]
    (println key-state)
    (key-pressed-functions key-state state)))

(defn key-released [state]
  (let [key-state (make-key-state)]
    (key-released-functions key-state state)))

(q/defsketch parensymphony
  :title "symphony"
  :setup setup
  :draw draw
  :key-pressed key-pressed
  :key-released key-released
  :renderer :p2d
  :middleware [m/fun-mode m/pause-on-error]
  :size [window-width window-height])

(def char-keycode-map (hash-map :0 48 :1 49 :2 50 :3 51 :4 52 :5 53 :6 54 :7 55 :8 56 :9 57
                                :a 65 :b 66 :c 67 :d 68 :e 69 :f 70 :g 71 :h 72 :i 73 :j 74
                                :k 75 :l 76 :m 77 :n 78 :o 79 :p 80 :q 81 :r 82 :s 83 :t 84
                                :u 85 :v 86 :w 87 :x 88 :y 89 :z 90))

(defn char->keycode [char]
  (char-keycode-map (keyword (str char)) 55))

(defn gen-pattern [n]
  (cond
    (or (number? n) (symbol? n) (keyword? n)) (map char->keycode (seq (str n)))
    (empty? n) nil
    :else (concat (gen-pattern (first n))
                  (list (chord :C3 :major))
                  (gen-pattern (rest n)))))

(defn play [time notes sep]
  (let [note (first notes)]
    (cond
      (seq? note) (at time (play-chord plucked-string note))
      :else (at time (plucked-string note)))
    (let [next-time (+ time sep)]
      (apply-by next-time play [next-time (rest notes) sep]))))

(comment
  ;;'(((((((((Lisp)))))))))
  ;;program = data = score
  ;;S expression -> phrase
  (play (now) (gen-pattern '(definst beep [freq 440]
                              (-> freq
                                  saw
                                  (* (env-gen (perc) :action FREE))))) 300)
  (stop)
)

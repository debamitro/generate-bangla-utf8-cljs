(ns generate-bangla-utf8-cljs.converter
  )

(def bangla-join-character "\u09CD")

(def bangla-consonants
  {
   :k "\u0995"
   :kh "\u0996"
   :g "\u0997"
   :gh "\u0998"
   :ng "\u0999"
   :ch "\u099A"
   :Ch "\u099B"
   :j "\u099C"
   :jh "\u099D"
   :NG "\u099E"
   :T "\u099F"
   :Th "\u09A0"
   :D "\u09A1"
   :Dh "\u09A2"
   :N "\u09A3"
   :t "\u09A4"
   :th "\u09A5"
   :d "\u09A6"
   :dh "\u09A7"
   :n "\u09A8"
   :p "\u09AA"
   :f "\u09AB"
   :b "\u09AC"
   :bh "\u09AD"
   :m "\u09AE"
   :y "\u09AF"
   :r "\u09B0"
   :l "\u09B2"
   :sh "\u09B6"
   :Sh "\u09B7"
   :s "\u09B8"
   :h "\u09B9"
   :rh "\u09DC"
   :Rh "\u09DD"
   }
  )

(def bangla-one-character-patterns
  {
   "a" :a
   "A" :A
   "i" :i
   "I" :I
   "u" :u
   "U" :U
   "e" :e
   "o" :o
   "k" :k
   "K" :K
   "g" :g
   "G" :G
   "j" :j
   "J" :J
   "T" :T
   "D" :D
   "N" :N
   "t" :t
   "d" :d
   "n" :n
   "p" :p
   "f" :f
   "b" :b
   "m" :m
   "y" :y
   "r" :r
   "l" :l
   "s" :s
   "h" :h
   }
  )

(def bangla-vowels
  {
   :a "\u0985"
   :A "\u0986"
   :i "\u0987"
   :I "\u0988"
   :u "\u0989"
   :U "\u098A"
   :e "\u098F"
   :oi "\u0990"
   :o "\u0993"
   :ou "\u0994"
   }
  )

(def bangla-two-character-patterns
  {
   "oi" :oi
   "ou" :ou
   "kh" :kh
   "gh" :gh
   "ng" :ng
   "ch" :ch
   "Ch" :Ch
   "NG" :NG
   "Th" :Th
   "Dh" :Dh
   "th" :th
   "dh" :dh
   "bh" :bh
   "sh" :sh
   "Sh" :Sh
   "rh" :rh
   "Rh" :Rh
   }
  )

(def bangla-vowel-prefixes
  {
   :a ""
   :A "\u09BE"
   :i "\u09BF"
   :I "\u09C0"
   :u "\u09C1"
   :U "\u09C2"
   :e "\u09C7"
   :oi "\u09C8"
   :o "\u09CB"
   :ou "\u09CC"
   }
  )

(defn is-space?
  [c]
  (= c \space)
  )

(defn is-vowel?
  [c]
  (or (= c \a)
      (= c \A)
      (= c \i)
      (= c \I)
      (= c \u)
      (= c \U)
      (= c \e)
      (= c \o)
      false)
  )

(defn is-symbol?
  [c]
  (or (= c \^)
      (= c \:)
      (= c \;)
      false)
  )

(defn is-consonant?
  [c]
  (and (not= c nil)
       (not (is-space? c))
       (not (is-vowel? c))
       (not (is-symbol? c))
       )
  )

(def bangla-symbols
  {
   "^" "\u0981"
   ":" "\u0983"
   ";" "\u0982"
   }
  )

(defn convert-symbol
  [chars]
  {:converted (bangla-symbols (str (first chars)))
   :unconverted (rest chars)}
  )

(defn convert-space
  "This function does not do anything to
  the first character of the input at all"
  [chars]
  {:converted (str (first chars)) :unconverted (rest chars)}
  )

(defn prepend-string-to-conversion
  [string {:keys [converted unconverted]}]
  {:converted (str string converted) :unconverted unconverted}
  )

(defn first-two-to-str
  [chars]
  (str (first chars) (nth chars 1))
  )

(defn try-two-conversions
  [chars fn1 fn2]
  ((fn [{:keys [converted unconverted]}]
     (if (empty? converted)
       {:converted (fn2 (str (first chars)))
        :unconverted (rest chars)}
       {:converted converted :unconverted unconverted}
       )
     )
   (fn1 chars)
   )
  )

(defn convert-two-character-pattern
  [chars bangla-symbol-to-str]
  (if (> (count chars) 1)
    {:converted (bangla-symbol-to-str (bangla-two-character-patterns (first-two-to-str chars)))
     :unconverted (rest (rest chars))}
    {:converted "" :unconverted chars}
    )
  )

(defn make-two-character-converter
  [bangla-symbol-to-str]
  (fn [chars]
    (convert-two-character-pattern chars bangla-symbol-to-str)
    )
  )

(defn make-one-character-converter
  [bangla-symbol-to-str]
  (fn [chars]
    (bangla-symbol-to-str (bangla-one-character-patterns chars))
    )
  )

(defn convert-one-consonant
  [chars]
  (try-two-conversions chars
                       (make-two-character-converter bangla-consonants)
                       (make-one-character-converter bangla-consonants)
                       )
  )

(defn convert-vowel
  "This function accepts a string
  or collection of characters and returns a
  map having a string with key :converted (representing
  the output of conversion) and a collection of
  characters with key :unconverted (representing what
  was left untouched)"
  [chars]
  (try-two-conversions chars
                       (make-two-character-converter bangla-vowels)
                       (make-one-character-converter bangla-vowels)
                       )
  )

(defn convert-vowel-prefix
  [chars]
  (try-two-conversions chars
                       (make-two-character-converter bangla-vowel-prefixes)
                       (make-one-character-converter bangla-vowel-prefixes)
                       )
  )

(defn insert-join-character
  [chars]
  (apply str (first chars)
         (map (fn [char]
                (str bangla-join-character char)
                )
              (rest chars)
              )
         )
  )

(defn convert-consonant
  [{:keys [converted unconverted]}]
  (let [firstchar (first unconverted)]
    (if (is-consonant? firstchar)
      (convert-consonant
       (prepend-string-to-conversion
        converted
        (convert-one-consonant unconverted)
        )
       )
      {:converted (insert-join-character converted) :unconverted unconverted}
      )
    )
  )

(defn convert-vowel-prefix-following-consonant
  [{:keys [converted unconverted]}]
     (if (not (empty? unconverted))
       (if (is-vowel? (first unconverted))
         (prepend-string-to-conversion
          converted
          (convert-vowel-prefix unconverted)
          )
         {:converted converted :unconverted unconverted}
         )
       {:converted converted :unconverted unconverted}
       )
  )

(defn convert-consonant-and-vowel
  "This function tries to convert a vowel
  prefix following a consonant, if applicable.
  Otherwise it does nothing other than convert-consonant"
  [chars]
  (convert-vowel-prefix-following-consonant
   (convert-consonant {:converted "" :unconverted chars})
   )
  )

(defn convert
  "This function decides which routine to call
  on the character stream chars out of
  convert-space
  convert-symbol
  convert-vowel
  convert-consonant.
  It also calls the selected routine.
  All of the underlying routines return a map
  with the converted output stored under key :converted
  and unconverted input stored under key :unconverted
  "
  [chars]
  (let [firstchar (first chars)]
    (if (is-space? firstchar)
      (convert-space chars)
      (if (is-vowel? firstchar)
        (convert-vowel chars)
        (if (is-symbol? firstchar)
          (convert-symbol chars)
          (convert-consonant-and-vowel chars)
          )
        )
      )
    )
  )

(defn ^:export to-bangla-utf8
  "This routine is the only functionality exported from this namespace"
  [englishInput]
  (if (empty? englishInput)
    ""
    ((fn [{:keys [converted unconverted]}]
       (str converted (to-bangla-utf8 unconverted))
       )
     (convert englishInput)
     )
    )
  )

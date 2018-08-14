(ns generate-bangla-utf8-cljs.converter
  )

(def bangla-consonants
  {
   "k" "\u0995"
   "K" "\u0996"
   "g" "\u0997"
   "G" "\u0998"
   }
  )

(def bangla-vowels
  {
   "a" "\u0985"
   "A" "\u0986"
   "i" "\u0987"
   "I" "\u0988"
   "u" "\u0989"
   "U" "\u098A"
   "e" "\u098F"
   "oi" "\u0990"
   "o" "\u0993"
   "ou" "\u0994"
   }
  )

(def bangla-vowel-prefixes
  {
   "a" ""
   "A" "\u09BE"
   "i" "\u09BF"
   "I" "\u09C0"
   "u" "\u09C1"
   "U" "\u09C2"
   "e" "\u09C7"
   "oi" "\u09C8"
   "o" "\u09CB"
   "ou" "\u09CC"
   }
  )

(defn convert-one-character
  [oneEnglishCharacter]
  (get bangla-consonants oneEnglishCharacter oneEnglishCharacter)
  )

(defn is-space?
  [c]
  (= c \space)
  )

(defn is-vowel?
  [c]
  (not= (bangla-vowels c) nil)
  )

(defn is-symbol?
  [c]
  (or (= c \^)
      (= c \:)
      (= c \;)
      false)
  )

(defn convert-vowel
  "This function accepts a string
  or collection of characters and returns a
  map having a string with key :converted (representing
  the output of conversion) and a collection of
  characters with key :unconverted (representing what
  was left untouched)"
  [chars]
  {:converted (bangla-vowels (str (first chars)))
   :unconverted (rest chars)}
  )

(defn convert-vowel-prefix
  [chars]
  {:converted (bangla-vowel-prefixes (str (first chars)))
   :unconverted (rest chars)}
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

(defn convert-consonant
  [chars]
  {:converted (bangla-consonants (str (first chars)))
   :unconverted (rest chars)}
  )

(defn prepend-string-to-conversion
  [string {:keys [converted unconverted]}]
  {:converted (str string converted) :unconverted unconverted}
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
   (convert-consonant chars)
   )
  )

(defn convert
  "This function decides which routine to call
  on the character stream chars out of
  convert-space
  convert-symbol
  convert-vowel
  convert-consonant.
  It also calls the selected routine"
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

(defn parse-all
  "This function accepts a map having
  a string with key :parsed and
  a string or collection of characters with key :unparsed.
  This returns a concatenation of :parsed and the
  result of parsing :unparsed."
  [{:keys [parsed unparsed]}]
  (if (empty? unparsed)
    parsed
    (str parsed
         (
          (fn [{:keys [converted unconverted]}]
            (parse-all {:parsed converted :unparsed unconverted})
            )

          (convert unparsed)
          )
         )
    )
  )

(defn to-bangla-utf8
  "This routine is the only functionality exported from this namespace"
  [englishInput]
  (parse-all {:parsed "" :unparsed englishInput})
  )

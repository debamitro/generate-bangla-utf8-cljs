(ns generate-bangla-utf8-cljs.converter
  )

(def bangla-consonants
  {"k" "\u0995"
   "K" "\u0996"
   "g" "\u0997"
   "G" "\u0998"}
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
  the output of parsing) and a collection of
  characters with key :unconverted"
  [chars]
  {:converted (bangla-vowels (str (first chars)))
   :unconverted (rest chars)}
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

          (convert-vowel unparsed)
          )
         )
    )
  )

(defn to-bangla-utf8
  "This routine is the only functionality exported from this namespace"
  [englishInput]
  (parse-all {:parsed "" :unparsed englishInput})
  )

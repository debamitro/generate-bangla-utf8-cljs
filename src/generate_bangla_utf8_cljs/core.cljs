(ns generate-bangla-utf8-cljs.core
  )

(defn convert-to-bangla-utf8
  [englishInput]
  "\u0995"
  )

(defn get-document-querySelector
  [selector]
  (js/document.querySelector selector)
  )

(defn handle-convert-request
  []
  (set! (.-innerHTML
         (get-document-querySelector "#banglaOutput")
         )
         (convert-to-bangla-utf8
          (.-value
           (get-document-querySelector "#englishInput")
           )
          )
         )

  )

(defn attach-events
  []
  (.addEventListener
   (get-document-querySelector "#convertButton")
   "click"
   handle-convert-request
   )
  )

(js/ready attach-events)

;; This part is for nodejs
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (js/console.log "Hello, World!"))

(set! *main-cli-fn* -main)
(ns generate-bangla-utf8-cljs.core
  (:require [generate-bangla-utf8-cljs.converter :as converter])
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
         (converter/to-bangla-utf8
          (.-value
           (get-document-querySelector "#englishInput")
           )
          )
         )

  )

(defn ^:export attach-events
  []
  (.addEventListener
   (get-document-querySelector "#convertButton")
   "click"
   handle-convert-request
   )
  )

;; This part is for nodejs
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (when (> (count (.-argv js/process)) 2)
    (js/console.log
     (converter/to-bangla-utf8
      (nth (.-argv js/process) 2)
      )
     )
    )
  )

(set! *main-cli-fn* -main)

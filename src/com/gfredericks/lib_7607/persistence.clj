(ns com.gfredericks.lib-7607.persistence
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]
            [com.gfredericks.lib-7607.pprint :refer [pprint]])
  (:import java.io.File))

(defn write
  [file search-manager]
  (let [file' (File/createTempFile "lib-7607-persistence" "")]
    (with-open [w (io/writer file')]
      (binding [*out* w]
        (pprint search-manager)
        (.flush w)))
    (.renameTo file' file))
  :ok)

(defn read
  [file]
  (-> file
      io/reader
      java.io.PushbackReader.
      clojure.core/read))

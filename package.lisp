;;;; package.lisp

(defpackage #:sja
  (:use #:cl)
  (:export
   :str-trim
   :str-split-trim
   :make-string-table
   :dict-keys
   :dict-values
   :unique-string-key
   :update-hash))

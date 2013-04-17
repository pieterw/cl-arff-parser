;;;
;;; A parser for weka arff Machine learning datasets.  See
;;; http://www.cs.waikato.ac.nz/ml/weka/arff.html for more information
;;;

(in-package :asdf)

(defsystem :cl-arff-parser
  :description "A parser for Weka arff (Attribute-Relation File
  Format) Machine learning datasets."
  :author "Pieter Wellens"
  :license "see LICENSE.txt"
  :version "0.8"
  :components ((:file "cl-arff-parser")))


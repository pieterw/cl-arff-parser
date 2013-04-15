;;;
;;; A parser for weka arff Machine learning datasets.  See
;;; http://www.cs.waikato.ac.nz/ml/weka/arff.html for more information
;;;

(in-package :asdf)

(defsystem :arff-parser
  :description "A parser for weka arff Machine learning datasets."
  :components 
  ((:file "arff-parser")))


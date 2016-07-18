(in-package :cl-user)
(defpackage lexer-test
  (:use :cl
   :prove
   :elvis-parsley))
(in-package :lexer-test)
(plan 2)

;; TODO write "correct" response tests for lexer
;;This test is based on an error I found from my own testing
(diag "Unterminated object with number values (\"numbers are known as unquoted objects\")")
(let ((unterminated-object-unquoted (make-string-input-stream "{\"tk1\":1, \"tk2\":2")))
  (is (tokens (make-instance 'json-ast :the-source unterminated-object-unquoted))
      '((:type :punctuation :value #\{) (:type :string :value "tk1") (:type :punctuation :value #\:) (:type :number :value 1) (:type :punctuation :value #\,) (:type :string :value "tk2") (:type :punctuation :value #\:) (:type :number :value 2))))

(diag "Unterminated array of numbers")
(let ((unterminated-array-unquoted (make-string-input-stream "[1,2,3")))
  (is (tokens (make-instance 'json-ast :the-source unterminated-array-unquoted))
      '((:type :punctuation :value #\[) (:type :number :value 1) (:type :punctuation :value #\,) (:type :number :value 2) (:type :punctuation :value #\,) (:type :number :value 3))))
(finalize) 

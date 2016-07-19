(in-package :cl-user)
(defpackage lexer-test
  (:use :cl
   :prove
   :elvis-parsley))
(in-package :lexer-test)

(plan 6)

(diag "Normal object test case")
(let ((normal-object (make-string-input-stream "{\"tk1\":1, \"tk2\":2.0, \"tk3\":\"tv3\", \"tk4\":true, \"tk5\":false, \"tk6\":null, \"tk7\":{\"sk1\":\"sv1\"}, \"tk8\":[1,2,3]}")))
  (is (tokens (make-instance 'json-ast :the-source normal-object))
      '((:type :punctuation :value #\{)
        (:type :string :value "tk1")
        (:type :punctuation :value #\:)
        (:type :number :value "1")
        (:type :punctuation :value #\,)
        (:type :string :value "tk2")
        (:type :punctuation :value #\:)
        (:type :number :value "2.0")
        (:type :punctuation :value #\,)
        (:type :string :value "tk3")
        (:type :punctuation :value #\:)
        (:type :string :value "tv3")
        (:type :punctuation :value #\,)
        (:type :string :value "tk4")
        (:type :punctuation :value #\:)
        (:type :keyword :value "true")
        (:type :punctuation :value #\,)
        (:type :string :value "tk5")
        (:type :punctuation :value #\:)
        (:type :keyword :value "false")
        (:type :punctuation :value #\,)
        (:type :string :value "tk6")
        (:type :punctuation :value #\:)
        (:type :keyword :value "null")
        (:type :punctuation :value #\,)
        (:type :string :value "tk7")
        (:type :punctuation :value #\:)
        (:type :punctuation :value #\{)
        (:type :string :value "sk1")
        (:type :punctuation :value #\:)
        (:type :string :value "sv1")
        (:type :punctuation :value #\})
        (:type :punctuation :value #\,)
        (:type :string :value "tk8")
        (:type :punctuation :value #\:)
        (:type :punctuation :value #\[)
        (:type :number :value "1")
        (:type :punctuation :value #\,)
        (:type :number :value "2")
        (:type :punctuation :value #\,)
        (:type :number :value "3")
        (:type :punctuation :value #\])
        (:type :punctuation :value #\}))))

(diag "Normal array test case")
(let ((array-stream (make-string-input-stream "[\"a1\",\"b2\",\"c3\"]")))
  (is (tokens (make-instance 'json-ast :the-source array-stream))
      '((:type :punctuation :value #\[)
        (:type :string :value "a1")
        (:type :punctuation :value #\,)
        (:type :string :value "b2")
        (:type :punctuation :value #\,)
        (:type :string :value "c3")
        (:type :punctuation :value #\]))))

(diag "Invalid number test case")
(let ((bad-number-stream (make-string-input-stream "{\"tk1\":1a3gb4afs}")))
  (is-error (tokens (make-instance 'json-ast :the-source bad-number-stream))
            'invalid-number-token))

(diag "Invalid keyword test case")
(let ((bad-keyword-stream (make-string-input-stream "{\"tk1\": trug}")))
  (is-error (tokens (make-instance 'json-ast :the-source bad-keyword-stream))
            'invalid-keyword-token))

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

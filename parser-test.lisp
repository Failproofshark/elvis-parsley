(in-package :cl-user)
(defpackage :parser-test
  (:use :cl
        :prove
        :elvis-parsley))
(in-package :parser-test)

(defun create-test-object (json-string)
  (make-instance 'json-ast :source (make-string-input-stream json-string)))
(plan 21)

;;TODO format these like the lexer tests
(diag "Simple object")
(let ((simple-flat-object (create-test-object "{\"tk1\":\"tk2\", \"tk2\":2, \"tk3\":3.4, \"tk4\": true, \"tk5\": null}")))
  (is (parse simple-flat-object) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :string))
                                                                   (:key "tk2" :value (:type :int))
                                                                   (:key "tk3" :value (:type :float))
                                                                   (:key "tk4" :value (:type :boolean))
                                                                   (:key "tk5" :value (:type :null))))))

(diag "embedded object")
(let ((embedded-object (create-test-object "{\"tk1\":{\"stk1\":\"stv2\", \"stk2\":2}, \"tk2\":{\"s1tk1\": {\"s2tk1\": { \"s3tk1\":1, \"s3tk2\":\"stv2\"}, \"s2tk2\":5.6}}, \"tk3\":\"tv3\"}")))
  (is (parse embedded-object) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :object :key-value-pairs ((:key "stk1" :value (:type :string))
                                                                                                                    (:key "stk2" :value (:type :int)))))
                                                                (:key "tk2" :value (:type :object :key-value-pairs ((:key "s1tk1" :value (:type :object :key-value-pairs ((:key "s2tk1" :value (:type :object :key-value-pairs ((:key "s3tk1" :value (:type :int))
                                                                                                                                                                                                                                (:key "s3tk2" :value (:type :string)))))
                                                                                                                                                                          (:key "s2tk2" :value (:type :float))))))))
                                                                (:key "tk3" :value (:type :string))))))

(diag "simple array")
(let ((simple-array (create-test-object "[1,2,3]")))
  (is (parse simple-array) '(:type :array :array-structure (:type :int))))

(diag "array with simple objects")
(let ((object-array (create-test-object "[{\"o1\":1}, {\"o1\":2}]")))
  (is (parse object-array) '(:type :array :array-structure (:type :object :key-value-pairs ((:key "o1" :value (:type :int)))))))

(diag "array with complex objects")
(let ((complex-object-array (create-test-object "[{\"o1k1\":{\"o1s1ok\":1, \"o1s2ok\":\"blah\"}, \"o1k2\":4.5}, {\"o1k1\":{\"o1s1ok\":8, \"o1s2ok\":\"melp\"}, \"o1k2\":20.567}]")))
  (is (parse complex-object-array) '(:type :array :array-structure (:type :object :key-value-pairs ((:key "o1k1" :value (:type :object :key-value-pairs ((:key "o1s1ok" :value (:type :int))
                                                                                                                                                         (:key "o1s2ok" :value (:type :string)))))
                                                                                                    (:key "o1k2" :value (:type :float)))))))

(diag "object with embedded array")
(let ((object-with-array-element (create-test-object "{\"tk1\":[1,2,3], \"tk2\":[{\"e1\":\"v1\"}, {\"e1\":\"v2\"}], \"tk3\":4, \"tk4\":[\"blah\",\"bar\"]}")))
  (is (parse object-with-array-element) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :array :array-structure (:type :int)))
                                                                            (:key "tk2" :value (:type :array :array-structure (:type :object :key-value-pairs ((:key "e1" :value (:type :string))))))
                                                                            (:key "tk3" :value (:type :int))
                                                                            (:key "tk4" :value (:type :array :array-structure (:type :string)))))))

;;TODO actually rename each error type
(diag "unterminated object")
(let ((unterminated-object (create-test-object "{\"tk1\":\"tv1\"")))
  (is-error (parse unterminated-object) 'invalid-json-format))

(diag "missing key")
(let ((missing-key (create-test-object "{:\"tv1\"}")))
  (is-error (parse missing-key) 'invalid-json-format))

(let ((second-missing-key (create-test-object "{\"tk1\":\"tv1\", :\"tv2\"}")))
  (is-error (parse second-missing-key) 'invalid-json-format))

(let ((third-missing-key (create-test-object "{\"tk1\":\"tv1\", \"tv2\"}")))
  (is-error (parse third-missing-key) 'invalid-json-format))

(diag "missing value")
(let ((missing-value (create-test-object "{\"tk1\": }")))
  (is-error (parse missing-value) 'invalid-json-format))
(let ((second-missing-value (create-test-object "{\"tk1\", }")))
  (is-error (parse second-missing-value) 'invalid-json-format))
(let ((third-missing-value (create-test-object "{\"tk1\" ]}")))
  (is-error (parse third-missing-value) 'invalid-json-format))

(let ((missing-value-middle (create-test-object "{\"tk1\":\"tv1\", \"tk2\":, \"tk3\",\"tv3\"}")))
  (is-error (parse missing-value-middle) 'invalid-json-format))

(diag "missing colon")
(let ((missing-colon (create-test-object "{\"tk1\"\"tv1\"}")))
  (is-error (parse missing-colon) 'invalid-json-format))

(diag "missing comma")
(let ((missing-comma (create-test-object "{\"tk1\":\"tv1\" \"tk2\":\"tv2\"}")))
  (is-error (parse missing-comma) 'invalid-json-format))

(diag "unterminated embedded object")
(let ((unterminated-embedded-object (create-test-object "{\"tk1\":\"tv1\", \"tk2\":{\"sk1\":{\"s2k1\":\"s2v1\"}}")))
  (is-error (parse unterminated-embedded-object) 'invalid-json-format))

(diag "unterminated array")
(let ((unterminated-array (create-test-object "[1,2,3")))
  (is-error (parse unterminated-array) 'invalid-json-format))

(diag "array missing comma")
;; TODO at some point please handle this case properly (not necessarily needed at this point given we only only at the first element)
;; Technically speaking this isn't a huge problem given that we really only look at the FIRST element of the array but we make sure this works for completeness and correctness
;; If we implement this it is beest to simply run the values through the main parse and check if there's a comma in between (essentially properly implement the proper state mechanism for this
(let ((array-missing-comma (create-test-object "[1 2,3]")))
  (declare (ignore array-missing-comma))
  (pass "This case will be implemented later"))

(diag "unterminated object array")
(let ((unterminated-object-array (create-test-object "[{\"tk1\":\"tv1\"]")))
  (is-error (parse unterminated-object-array) 'invalid-json-format))

(diag "unterminated array in object")
(let ((unterminated-array-object (create-test-object "{\"tk1\":[1,2,3, \"tk2\":\"tv2\"}")))
  (is-error (parse unterminated-array-object) 'invalid-json-format))

;;Unterminated strings and stuff are the lexers job to catch
;;If we're missing an opening bracket/brace the parser will simply return the type of the first object.
;;TODO make missing key or value for embedded and array

(finalize)

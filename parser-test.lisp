(in-package :cl-user)
(defpackage :parser-test
  (:use :cl
        :prove
        :elvis-parsley))
(in-package :parser-test)

(defparameter *simple-flat-object* (make-instance 'json-ast :the-source (make-string-input-stream "{\"tk1\":\"tk2\", \"tk2\":2, \"tk3\":3.4, \"tk4\": true, \"tk5\": null}")))

(defparameter *embedded-object* (make-instance 'json-ast :the-source (make-string-input-stream "{\"tk1\":{\"stk1\":\"stv2\", \"stk2\":2}, \"tk2\":{\"s1tk1\": {\"s2tk1\": { \"s3tk1\":1, \"s3tk2\":\"stv2\"}, \"s2tk2\":5.6}}, \"tk3\":\"tv3\"}")))

(defparameter *simple-array* (make-instance 'json-ast :the-source (make-string-input-stream "[1,2,3]")))

(defparameter *object-array* (make-instance 'json-ast :the-source (make-string-input-stream "[{\"o1\":1}, {\"o1\":2}]")))

(defparameter *complex-object-array* (make-instance 'json-ast :the-source (make-string-input-stream "[{\"o1k1\":{\"o1s1ok\":1, \"o1s2ok\":\"blah\"}, \"o1k2\":4.5}, {\"o1k1\":{\"o1s1ok\":8, \"o1s2ok\":\"melp\"}, \"o1k2\":20.567}]")))

(defparameter *object-with-array-element* (make-instance 'json-ast :the-source (make-string-input-stream "{\"tk1\":[1,2,3], \"tk2\":[{\"e1\":\"v1\"}, {\"e2\":\"v2\"}], \"tk3\":4, \"tk4\":[\"blah\",\"bar\"]}")))

;;TODO create test cases for invalid JSON including mismatch brackets, incorrect numbers, and incorrect keywords, missing commas
(plan 6)

(diag "Simple object")
(is (parse *simple-flat-object*) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :string))
                                                                   (:key "tk2" :value (:type :int))
                                                                   (:key "tk3" :value (:type :float))
                                                                   (:key "tk4" :value (:type :boolean))
                                                                   (:key "tk5" :value (:type :null)))))

(diag "embedded object")
(is (parse *embedded-object*) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :object :key-value-pairs ((:key "stk1" :value (:type :string))
                                                                                                                    (:key "stk2" :value (:type :int)))))
                                                                (:key "tk2" :value (:type :object :key-value-pairs ((:key "s1tk1" :value (:type :object :key-value-pairs ((:key "s2tk1" :value (:type :object :key-value-pairs ((:key "s3tk1" :value (:type :int))
                                                                                                                                                                                                                                 (:key "s3tk2" :value (:type :string)))))
                                                                                                                                                                           (:key "s2tk2" :value (:type :float))))))))
                                                                (:key "tk3" :value (:type :string)))))

(diag "simple array")
(is (parse *simple-array*) '(:type :array :array-structure (:type :int)))

(diag "array with simple objects")
(is (parse *object-array*) '(:type :array :array-structure (:type :object :key-value-pairs ((:key "o1" :value (:type :int))))))

(diag "array with complex objects")
(is (parse *complex-object-array*) '(:type :array :array-structure (:type :object :key-value-pairs ((:key "o1k1" :value (:type :object :key-value-pairs ((:key "o1s1ok" :value (:type :int))
                                                                                                                                                         (:key "o1s2ok" :value (:type :string)))))
                                                                                                    (:key "o1k2" :value (:type :float))))))

(diag "object with embedded array")
(is (parse *object-with-array-element*) '(:type :object :key-value-pairs ((:key "tk1" :value (:type :array :array-structure (:type :int)))
                                                                          (:key "tk2" :value (:type :array :array-structure (:type :object :key-value-pairs ((:key "e1" :value (:type :string))))))
                                                                          (:key "tk3" :value (:type :int))
                                                                          (:key "tk4" :value (:type :string)))))

(finalize)

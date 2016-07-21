(in-package :cl-user)
(defpackage :compiler-to-java-test
  (:use :cl
        :elvis-parsley
        :prove))
(in-package :compiler-to-java-test)

(defun make-object (json)
  (make-instance 'json-ast :source (make-string-input-stream json)))

(plan 6)

(diag "Flat object")
(let ((flat-object (make-object "{\"tk1\":\"tv1\", \"tk2\":23, \"tk3\":234.51, \"tk4\":true, \"tk5\":null}")))
  (declare (ignore flat-object))
  (pass "A regular flat object"))

(diag "Embedded Object")
(let ((embedded-object (make-object "{\"tk1\":\"tv1\", \"tk2\":{\"sk1\":1, \"sk2\":{\"zk1\":true, \"zk2\":4.45}, \"sk2\":false}, \"tk3\":\"blah\"}")))
  (declare (ignore embedded-object))
  (pass "An embedded object"))

(diag "Object with plain array")
(let ((object-with-plain-array (make-object "{\"tk1\":\"tv1\", \"tk2\":[1,2,3], \"tk4\":false}")))
  (declare (ignore object-with-plain-array))
  (pass "Object with plain array"))

(diag "Object with array populated by objects")
(let ((object-array (make-object "{\"tk1\":4.5, \"tk2\":[{\"ak1\":true, \"ak2\":5}, {\"ak1\":false, \"ak2\":2}], \"tk3\":\"foo\"}")))
  (declare (ignore object-array))
  (pass "Object with array populated by objects"))

(diag "Plain Array")
(let ((plain-array (make-object "[1,2,3,4]")))
  (declare (ignore plain-array))
  (pass "regular array"))

(diag "Array of Objects")
(let ((array-of-objects "[{\"tk1\":1, \"tk2\":\"hi\"}, {\"tk1:2, \"tk2\":\"bye\"}]"))
  (declare (ignore array-of-objects))
  (pass "array of objects"))

(finalize)

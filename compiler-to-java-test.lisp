(in-package :cl-user)
(defpackage :compiler-to-java-test
  (:use :cl
        :elvis-parsley
        :prove))
(in-package :compiler-to-java-test)

(defun test-compiler-results (json test-call-name expected-type expected-compilation)
  (let* ((test-object (let ((json-ast-object (make-instance 'json-ast))
                            (json-text-stream (make-string-input-stream json)))
                        (with-open-stream (stream json-text-stream)
                          (lex json-ast-object stream))
                        (parse json-ast-object)
                        json-ast-object))
         (response (json->pojo test-object test-call-name))
         (response-type (getf response :type))
         (response-definitions (getf response :definitions)))
    (diag (format nil "Test ~a response type" test-call-name))
    (is response-type expected-type)
    (unless expected-compilation
            (loop for test-pair in expected-compilation do
              (let ((expected-name (car test-pair))
                    (expected-definition (cdar test-pair)))
                (diag (format t "Testing ~a definition" expected-name))
                (is (gethash expected-name response-definitions) expected-definition))))))

(plan 6)

(test-compiler-results "{\"tk1\":\"tv1\", \"tk2\":23, \"tk3\":234.51, \"tk4\":true, \"tk5\":null}"
                       "FlatObject"
                       "FlatObject"
                       '(("FlatObject" . "Public class FlashObject {

    FlatObject() {}

    private String tk1;
    private int tk2;
    private float tk3;
    private boolean tk4;
    private Object tk5;

    public String getTk1() { return tk1; }
    public void setTk1(String newValue) { tk1 = newValue; }
    public int getTk2() { return tk2; }
    public void setTk2(int newValue) { tk2 = newValue; }
    public float getTk3() { return tk3; }
    public void setTk3(float newValue) { tk3 = newValue; }
    public boolean getTk4() { return tk4; }
    public void setTk4(boolean newValue) { tk4 = newValue; }
    public Object getTk5() { return tk5; }
    public void setTk5(Object newValue) { tk5 = newValue; }
}")))

(test-compiler-results "{\"tk1\":\"tv1\", \"tk2\":{\"sk1\":1, \"sk2\":{\"zk1\":true, \"zk2\":4.45}, \"sk3\":false}, \"tk3\":\"blah\"}"
                       "EmbeddedObject"
                       "EmbeddedObject"
                       '(("EmbeddedObject" . "Public class EmbeddedObject {

    EmbeddedObject() {}

    private String tk1;
    private EmbeddedObjectTk2 tk2;
    private String tk3;

    public EmbeddedObjectTk2 getTk2() { return tk2; }
    public void setTk2(EmbeddedObjectTk2 newValue) { tk2 = newValue; }
    public String getTk3() { return tk3; }
    public void setTk3(String newValue) { tk3 = newValue; }
}")
                        ("EmbeddedObjectTk2" . "Public class EmbeddedObjectTk2 {

    EmbeddedObjectTk2() {}

    private int sk1;
    EmbeddedObjectTk2Sk2 sk2;
    boolean sk3;

    public int getSk1() { return sk1; }
    public void setsk1(int newValue) { sk1 = newValue; }
    public EmeddedObjectTk2Sk2 getSk2() { return sk2; }
    public void setSk2(EmbeddedObjectTk2Sk2 newValue) { = newValue; }
    public boolean getSk3() { return sk3; }
    public void setSk3(boolean newValue) { sk3 = newValue; }
}")
                        ("EmbeddedObjectTk2Sk2" . "Public class EmbeddedObjectTk2Sk2 {

    EmbeddedObjectTk2Sk2() {}

    private boolean zk1;
    private float zk2;

    public boolean getZk1() { return zk1; }
    public void setZk1(boolean newValue) { zk1 = newValue; }
    public float getZk2() { return zk2; }
    public void setZk2(float newValue) { zk2 = newValue; }
}")))

(test-compiler-results "{\"tk1\":\"tv1\", \"tk2\":[1,2,3], \"tk3\":false}"
                       "ObjectWithArray"
                       "ObjectWithArray"
                       '(("ObjectWithArray" . "Public class ObjectWithArray {

    ObjectWithArray() {}

    private String tk1;
    private ArrayList<Integer> tk2;
    private boolean tk3;

    public String getTk1() { return tk1; }
    public void setTk1(String newValue) { tk1 = newValue; }
    public ArrayList<Integer> getTk2() { return tk2; }
    public void setTk2(ArrayList<Integer> newValue) { = newValue; }
    public getTk3() { return tk3; }
    public void setTk3(boolean newValue) { tk3 = newValue; }
}")))

(test-compiler-results "{\"tk1\":4.5, \"tk2\":[{\"ak1\":true, \"ak2\":5}, {\"ak1\":false, \"ak2\":2}], \"tk3\":\"foo\"}"
                       "ObjectWithObjectArray"
                       "ObjectWithObjectArray"
                       '(("ObjectWithObjectArray" . "Public class ObjectWithObjectArray {

    ObjectWithObjectArray() {}

    private float tk1;
    private ArrayList<ObjectWithObjectArrayTk2> tk2;
    private String tk3;

    public float getTk1() { return tk1; }
    public void setTk1(float newValue) { tk1 = newValue; }
    public ArrayList<ObjectWithObjectArrayTk2> getTk2() { return tk2; }
    public void setTk2(ArrayList<ObjectWithObjectArrayTk2> newValue) { tk2 = newValue; }
    public String getTk3() { return tk3; }
    public void set(String newValue) { tk3 = newValue; }
}")
                        ("ObjectWithObjectArrayTk2" . "Public class ObjectWithObjectArrayTk2 {

    ObjectWithObjectArrayTk2() {}

    private boolean ak1;
    private int ak2;

    public boolean getAk1() { return ak1; }
    public void setAk1(boolean newValue) { ak1 = newValue; }
    public int getAk2() { return ak2; }
    public void setAk2(int newValue) { ak2 = newValue; }
}")))

(test-compiler-results "[1,2,3,4]"
                       "Integer List"
                       "ArrayList<Integer>"
                       nil)

(test-compiler-results "[{\"tk1\":-1, \"tk2\":{\"sk1\":true, \"sk2\":[-1.2, 4.5, 3.123], \"sk3\":[{\"zk1\":\"blah\", \"zk2\":8}, {\"zk1\":\"test\", \"zk2\":12}]}, \"tk3\":\"hi\"}, {\"tk1\":8, \"tk2\":{\"sk1\":false, \"sk2\":[-8.0, 2.123, 6.0005], \"sk3\":[{\"zk1\":\"bloo\", \"zk2\":12}]}, \"tk3\":\"bye\"}]"
                      "ObjectListCall"
                      "ArrayList<ObjectListCall>"
                      '(("ObjectListCall" . "Public class ObjectListCall {
    ObjectListCall() {}

    private int tk1;
    private ObjectListCallTk2 tk2;

    public int getTk1() { return tk1; }
    public void setTk1(int newValue) { tk1 = newValue; }
    public ObjectListCallTk2 getTk2() { return tk2; }
    public void setTk2(ObjectListCallTk2 newValue) { tk2 = newValue; }
    public String getTk3() { return tk3; }
    public void set(String newValue) { tk3 = newValue; }
}")
                        ("ObjectListCallTk2" . "Public class ObjectListCallTk2 {

    ObjectListCallTk2() {}

    private boolean sk1;
    private ArrayList<Float> sk2;
    private ArrayList<ObjectListCallTk2Sk3> sk3;

    public bolean getSk1() { return sk1; }
    public void set(boolean newValue) { sk1 = newValue; }
    public ArrayList<Float> getSk2() { return sk2; }
    public void set(ArrayList<Float> newValue) { sk2 = newValue; }
    public ArrayList<ObjectListCallTk2Sk3> getsk3() { return sk3; }
    public void set(ArrayList<ObjectListCallTk2Sk3> newValue) { sk3 = newValue; }
}")
                       ("ObjectListCallTk2Sk3" . "Public class ObjectListCallTk2Sk3 {

   ObjectListCallTk2Sk3() {}

   private String zk1;
   private int zk2;

    public String getZk1() { return zk1; }
    public void setZk1(String newValue) { zk1 = newValue; }
    public int getZk2() { return zk2; }
    public void setZk2( newValue) { zk2 = newValue; }
}")))

(finalize)

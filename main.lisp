;;TODO add support for exponential numbers
;;TODO write tests for the lexer and parser (JSON)
(in-package :cl)

(defpackage :elvis-parsley
  (:use :cl
   :alexandria)
  (:import-from :cl-ppcre
               :create-scanner
               :scan)
  (:export :json-ast
           :lex
           :parse
           :tokens
           :json->pojo
           :invalid-token
           :erroneous-value
           :invalid-json-format))

(in-package :elvis-parsley)

(defclass ast ()
  ((tokens :accessor tokens)
   (tree :accessor tree)))

(defclass json-ast (ast)
  ())

(defgeneric lex (ast source))
(defgeneric parse (ast))

(define-condition invalid-token (error)
  ((erroneous-value :initarg :erroneous-value
                    :accessor erroneous-value)))

(define-condition invalid-parsing-case (error)
  ((erroneous-value :initarg :erroneous-value
                    :accessor erroneous-value)))

(defmethod lex ((current-ast json-ast) source)
  (let ((integer-scanner (create-scanner '(:sequence :start-anchor (:greedy-repetition 0 1 (:alternation #\+ #\-)) (:greedy-repetition 1 nil :digit-class) :end-anchor)))
        ;; This covers the case of a number followed by a period then nothing following e.g. 2. or 1.
        ;; We do not include this in the regular fraction scanner because [0-9]*\.[0-9]* would mean that a lone period is a valid number which is incorrect
        (whole-fraction-scanner (create-scanner '(:sequence
                                                  :start-anchor
                                                  (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                                  (:greedy-repetition 1 nil :digit-class)
                                                  #\.
                                                  :end-anchor)))
        (fraction-scanner (create-scanner '(:sequence
                                            :start-anchor
                                            (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                            (:greedy-repetition 0 nil :digit-class)
                                            #\.
                                            (:greedy-repetition 1 nil :digit-class)
                                            :end-anchor))))
    (with-accessors ((tokens tokens)) current-ast
      (setf tokens
            (labels ((contains (current-character search-string)
                       (loop for search-character across search-string
                             when (char= current-character search-character)
                               do (return t)))
                     
                     (is-punctuation (current-character)
                       (contains current-character ":,{}[]"))

                     (is-start-of-keyword (current-character)
                       ;;Note we know it's a keyword because it does NOT follow a double quotation mark
                       (contains current-character "tfn"))
                     
                     (is-open-quote (current-character)
                       (char= current-character #\"))
                     
                     (is-keyword (word)
                       (loop for keyword in '("true" "false" "null")
                             when (string= word keyword)
                               do (return t)))

                     (is-number (token)
                       (or (scan integer-scanner token)
                           (scan fraction-scanner token)
                           (scan whole-fraction-scanner token)))
                     
                     (read-until-termination (stream condition)
                       (with-open-stream (new-string (make-string-output-stream))
                         (loop for current-character = (peek-char t stream nil :END-OF-FILE)
                               do (cond ((eql current-character :END-OF-FILE) (return (values-list `(:END-OF-FILE ,(get-output-stream-string new-string)))))
                                        ((funcall condition current-character) (return (get-output-stream-string new-string)))
                                        (t (write-char (read-char stream) new-string))))))

                     (read-unquoted-string (stream)
                       (let* ((value-read (multiple-value-list (read-until-termination stream #'(lambda (current-character)
                                                                                                  (or (char= current-character #\})
                                                                                                      (char= current-character #\])
                                                                                                      (char= current-character #\,)
                                                                                                      ;;TODO may need to change this depending on if this is present in a given lisp implementation
                                                                                                      (char= current-character #\space))))))
                              (value (if (> (length value-read) 1)
                                         (cadr value-read)
                                         (car value-read))))
                         (cond ((is-keyword value) `(:type :keyword :value ,value))
                               ((is-number value) `(:type :number :value ,value))
                               (t (error 'invalid-token :erroneous-value value)))))
                     
                     (read-punctuation (stream)
                       `(:type :punctuation :value ,(read-char stream)))
                     
                     (read-in-string (stream)
                       (read-char stream)
                       (let ((value (read-until-termination stream #'(lambda (current-character)
                                                                       (char= current-character #\")))))
                         (if (eql :END-OF-FILE value)
                             (error 'invalid-token :erroneous-value value)
                             (progn
                               (read-char stream)
                               `(:type :string :value ,value))))))
              
              (loop for current-character = (peek-char t source nil)
                    while current-character
                    collect (cond ((is-punctuation current-character) (read-punctuation source))
                                  ((is-open-quote current-character) (read-in-string source))
                                  ((is-start-of-keyword current-character) (read-unquoted-string source))
                                  ((or (digit-char-p current-character)
                                       (char= current-character #\.)
                                       (char= current-character #\+)
                                       (char= current-character #\-)) (read-unquoted-string source))
                                  (t (error 'invalid-parsing-case :erroneous-value current-character)))))))))

;;TODO use pop to move the list along OR keep track of how far you need to move with some sort of counter and use nthcdr
;;TODO move this from recursive to iterative, or simply make this destructive ...
(define-condition invalid-json-format (error)
  ())

(defmethod parse ((current-ast json-ast))
  ;; Each parse-function accepts a token and a state which is a symbol :key or :value indicating what exactly we're looking (useful for naming and stuff)
  (let ((float-scanner (create-scanner '(:sequence (:greedy-repetition 0 nil :digit-class) 
                                         #\. 
                                         (:greedy-repetition 1 nil :digit-class)))))
    (labels ((parse-implementation (token-stream)
               (labels ((parse-object (token-stream)
                          (labels ((parse-key-value-pairs (token-stream current-state key-value-pairs current-key)
                                     (let* ((current-token (car token-stream))
                                            (token-type (getf current-token :type))
                                            (token-value (getf current-token :value)))
                                       (cond ((eql current-state :comma-or-terminate) (cond ((and (eql token-type :punctuation)
                                                                                                  (char= token-value #\,))
                                                                                             (parse-key-value-pairs (cdr token-stream)
                                                                                                                    :key
                                                                                                                    key-value-pairs
                                                                                                                    nil))
                                                                                            ((and (eql token-type :punctuation)
                                                                                                  (char= token-value #\}))
                                                                                             (values-list `(,key-value-pairs
                                                                                                            ,(cdr token-stream))))
                                                                                            (t (error 'invalid-json-format))))
                                             
                                             ;; we wrap the result in a list because key-value pairs is supposed to be a list of p-lists
                                             ((eql current-state :key) (if (not (eql token-type :string))
                                                                           (error 'invalid-json-format)
                                                                           (parse-key-value-pairs (cdr token-stream)
                                                                                                  :kv-separator
                                                                                                  key-value-pairs
                                                                                                  (car token-stream))))
                                             
                                             ((eql current-state :kv-separator) (if (and (eql token-type :punctuation)
                                                                                         (char= token-value #\:))
                                                                                    (parse-key-value-pairs (cdr token-stream)
                                                                                                           :value
                                                                                                           key-value-pairs
                                                                                                           current-key)
                                                                                    (error 'invalid-json-format)))
                                             
                                             ((eql current-state :value) (multiple-value-bind (parsed-value remaining-tokens)
                                                                             (parse-implementation token-stream)
                                                                           ;; Any illegal value will be caught in the main value of this function call
                                                                           (parse-key-value-pairs remaining-tokens
                                                                                                  :comma-or-terminate
                                                                                                  (append key-value-pairs
                                                                                                          `((:key ,(getf current-key :value)
                                                                                                             :value ,parsed-value)))
                                                                                                  nil)))))))
                            (multiple-value-bind (key-value-pairs remaining-tokens)
                                (parse-key-value-pairs token-stream
                                                       :key
                                                       nil
                                                       nil)
                              (values-list `((:type :object
                                              :key-value-pairs ,key-value-pairs)
                                             ,remaining-tokens)))))
                        ;; We are assuming a uniform array structure hence we simply return the first item we parse to signify the overall structure and return the remaining tokens after the closing square bracket
                        (parse-array (token-stream)
                          (multiple-value-bind (array-structure remaining-tokens)
                              (parse-implementation token-stream)
                            (let ((skip-count (loop for token in remaining-tokens
                                                    count t into token-count
                                                    when (and (eq (getf token :type) :punctuation) (char= (getf token :value) #\]))
                                                      do (return token-count))))
                              (if skip-count
                                  (values-list `((:type :array
                                                  :array-structure ,array-structure)
                                                 ,(nthcdr skip-count remaining-tokens)))
                                  (error 'invalid-json-format)))))
                        (parse-string (token-stream)
                          (values-list
                           `((:type :string)
                             ,(cdr token-stream))))
                        (parse-number (token-stream)
                          (values-list `(,(let ((value (getf (car token-stream) :value)))
                                            (if (scan float-scanner value)
                                                `(:type :float)
                                                `(:type :int)))
                                         ,(cdr token-stream))))

                        (parse-keyword (token-stream)
                          (let* ((current-token-value (getf (car token-stream) :value))
                                 ;;TODO add the default case to raise a condition of malformed keyword
                                 (type (cond ((or (string= current-token-value "true") (string= current-token-value "false")) `(:type :boolean))
                                             ((string= current-token-value "null") `(:type :null)))))
                            (values-list `(,type
                                           ,(cdr token-stream))))))
                 (let ((current-token (car token-stream)))
                   (cond ((and (eq :punctuation (getf current-token :type)) (char= #\{ (getf current-token :value))) (parse-object (cdr token-stream)))
                         ((and (eq :punctuation (getf current-token :type)) (char= #\[ (getf current-token :value))) (parse-array (cdr token-stream)))
                         ((eq :string (getf current-token :type)) (parse-string token-stream))
                         ((eq :number (getf current-token :type)) (parse-number token-stream))
                         ((eq :keyword (getf current-token :type)) (parse-keyword token-stream))
                         ;;Empty token stream is valid
                         (t (error 'invalid-json-format)))))))
      (setf (tree current-ast) (parse-implementation (tokens current-ast))))))

;; to-be-defined-stack item structure should include an ancestor list for class name generation or have parent name?
(defun json->pojo (json-ast name-of-root &optional debug)
  (declare (ignore debug))
  (let ((object-definitions (make-hash-table :test 'equalp))
        (to-be-defined-stack `((,(tree json-ast) ,name-of-root))))
    (flet ((compile-object (object name)
             (let ((constituents nil)
                   (getters-and-setters nil))
               (flet ((add-to-component-stacks (field-type field-name)
                        (push (format nil
                                      "Private ~a ~a;"
                                      field-type
                                      field-name)
                              constituents)
                        (push (format nil
                                      "Public ~a get~@(~a~)() { return ~:*~a; }~%    Public ~2:*~a set~@(~a~)(~2:*~a newValue) { ~a = newValue; }"
                                      field-type
                                      field-name)
                              getters-and-setters)))
                 (loop for pair in (getf object :key-value-pairs) do
                   (let ((field-type (getf (getf pair :value) :type))
                         (field-name (getf pair :key)))
                     (cond ((or (eql field-type :int)
                                (eql field-type :float)
                                (eql field-type :boolean))
                            (add-to-component-stacks (string-downcase (symbol-name field-type)) field-name))
                           ((eql field-type :string) (add-to-component-stacks (string-capitalize (symbol-name field-type)) field-name))
                           ;; Just to hold a null object for completeness
                           ((eql field-type :null) (add-to-component-stacks "Object" field-name)))))
                 (setf (gethash name object-definitions)
                       (format nil
                               "Public class ~@(~a~) {~%~{    ~a~%~}~%~%~{    ~a~%~}~%}"
                               name
                               constituents
                               getters-and-setters))))))
      (loop for component in to-be-defined-stack do
        (apply #'compile-object component)))
    object-definitions))

;;TODO need to account for escaped quotes in lexer
;;TODO need to be able to throw "malformed json"
;;TODO add support for exponential numbers
;;TODO write tests for the lexer and parser (JSON)
(in-package :cl)

(defpackage :elvis-parsley
  (:use :cl
   :alexandria)
  (:import-from :cl-ppcre
               :create-scanner
               :scan)
  (:export :parse))

(in-package :elvis-parsley)

;; TODO Should x. where x is a number count as a float?
(defvar *float-re-test* (create-scanner '(:sequence (:greedy-repetition 1 nil :digit-class) 
                                          #\. 
                                          (:greedy-repetition 1 nil :digit-class))))

(defclass ast ()
  ((source :initarg :the-source
           :accessor source)
   (tokens :accessor tokens)
   (tree :accessor tree)))

(defclass json-ast (ast)
  ())

(defgeneric lex (ast))
(defgeneric parse (ast))
(defgeneric compile-tree (ast))

(defmethod initialize-instance :after ((current-ast json-ast) &key &allow-other-keys)
  (lex current-ast))

(defmethod lex ((current-ast json-ast))
  (with-accessors ((source source) (tokens tokens)) current-ast
    (setf tokens
          (labels ((contains (current-character search-string)
                     (loop for search-character across search-string
                           when (char= current-character search-character)
                             do (return t)))
                   
                   (is-punctuation (current-character)
                     (contains current-character ":,{}[]"))

                   ;;TODO Sort of hackish this should be fixed
                   (is-start-of-keyword (current-character)
                     (contains current-character "tfn"))
                   
                   (is-open-quote (current-character)
                     (char= current-character #\"))
                   
                   (is-keyword (word)
                     (loop for keyword in '("true" "false" "null")
                           when (string= word keyword)
                             do (return t)))

                   (read-until-termination (stream condition)
                     (with-output-to-string (new-string)
                       (loop for current-character = (peek-char t stream)
                             until (funcall condition current-character)
                             do (write-char (read-char stream) new-string))))

                   (read-unquoted-string (stream)
                     (let ((value (read-until-termination stream #'(lambda (current-character)
                                                                     (or (char= current-character #\})
                                                                         (char= current-character #\])
                                                                         (char= current-character #\,)
                                                                         ;;TODO may need to change this depending on if this is present in a given implementation
                                                                         (char= current-character #\space))))))
                       (if (is-keyword value)
                           `(:type :keyword :value ,value)
                           `(:type :number :value ,value))))
                   
                   (read-punctuation (stream)
                       `(:type :punctuation :value ,(read-char stream)))
                   
                   (read-in-string (stream)
                     (read-char stream)
                     (let ((value (read-until-termination stream #'(lambda (current-character)
                                                                     (char= current-character #\")))))
                       (read-char stream)
                       `(:type :string :value ,value))))
            
            (loop for current-character = (peek-char t source nil)
                  while current-character
                  collect (cond ((is-punctuation current-character) (read-punctuation source))
                                ((is-open-quote current-character) (read-in-string source))
                                ((is-start-of-keyword current-character) (read-unquoted-string source))
                                ((digit-char-p current-character) (read-unquoted-string source))))))))

(defmethod parse ((current-ast json-ast)))

;; broken case: (defparameter *foo* (make-instance 'json-ast :the-source (make-string-input-stream "{\"tk\":{\"subk1\": \"subv1\", \"subk2\":2, \"subk3\":{\"ssubk1\":\"ssubv1\"}}, \"tk2\":\"tv2\", \"num1\":1.4}")))

;;TODO use pop to move the list along OR keep track of far you need to move with some sort of counter and use nthcdr
;;TODO move this from recursive to iterative, or simply make this destructive ...
(defun temp-parse (token-stream)
  (declare (optimize (debug 3)))
  ;; Each parse-function accepts a token and a state which is a symbol :key or :value indicating what exactly we're looking (useful for naming and stuff)
  ;;TODO parse-array
  (labels ((parse-object (token-stream)
             (labels ((parse-key-value-pairs (token-stream current-state key-value-pairs current-key)
                        (let* ((current-token (car token-stream))
                               (token-type (getf current-token :type))
                               (token-value (getf current-token :value)))
                          (cond ((and (eq :punctuation token-type) (char= #\} token-value)) (values-list `(,key-value-pairs
                                                                                                           ,(cdr token-stream))))
                                ((and (eq :punctuation token-type)
                                      (or (char= #\, token-value) (char= #\: token-value))) (parse-key-value-pairs (cdr token-stream)
                                                                                                                   current-state
                                                                                                                   key-value-pairs
                                                                                                                   current-key))
                                ;; we wrap the result in a list because key-value pairs is supposed to be a list of p-lists
                                ((eq current-state :key) (parse-key-value-pairs (cdr token-stream)
                                                                                :value
                                                                                key-value-pairs
                                                                                (car token-stream)))
                                ((eq current-state :value) (multiple-value-bind (parsed-value remaining-tokens)
                                                               (temp-parse token-stream)
                                                             (parse-key-value-pairs remaining-tokens
                                                                                    :key
                                                                                    (append key-value-pairs
                                                                                            `((:key ,(getf current-key :value)
                                                                                                :value ,parsed-value)))
                                                                                    nil)))))))
               (multiple-value-bind (key-value-pairs remaining-tokens)
                   (parse-key-value-pairs (cdr token-stream)
                                      :value
                                      nil
                                      (car token-stream))
                 (values-list `((:type :object
                                 :key-value-pairs ,key-value-pairs)
                                ,remaining-tokens)))))
           ;; We are assuming a uniform array structure hence we simply return the first item we parse to signify the overall structure and return the remaining tokens after the closing square bracket
           (parse-array (token-stream)
             (multiple-value-bind (array-structure remaining-tokens)
                 (temp-parse token-stream)
               (let* ((skip-count (loop for token in remaining-tokens
                                        count t into token-count
                                        when (and (eq (getf token :type) :punctuation) (char= (getf token :value) #\]))
                                          do (return token-count)))
                      (post-array-tokens (nthcdr skip-count token-stream)))
                 (values-list `((:type :array
                                 :array-structure ,array-structure)
                                ,post-array-tokens)))))
           (parse-string (token-stream)
             (values-list
              `((:type :string :value ,(getf (car token-stream) :value))
                ,(cdr token-stream))))
           (parse-number (token-stream)
             (values-list `(,(let ((value (getf (car token-stream) :value)))
                               (if (scan *float-re-test* value)
                                   `(:type :float :value ,value)
                                   `(:type :int :value ,value)))
                            ,(cdr token-stream)))))
    (let ((current-token (car token-stream)))
      (cond ((and (eq :punctuation (getf current-token :type)) (char= #\{ (getf current-token :value))) (parse-object (cdr token-stream)))
            ((and (eq :punctuation (getf current-token :type)) (char= #\[ (getf current-token :value))) (parse-array (cdr token-stream)))
            ((eq :string (getf current-token :type)) (parse-string token-stream))
            ((eq :number (getf current-token :type)) (parse-number token-stream))))))

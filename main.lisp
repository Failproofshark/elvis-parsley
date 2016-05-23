;;TODO need to account for escaped quotes in lexer
;;TODO need to be able to throw "malformed json"
;;TODO add support for exponential numbers
;;TODO write tests for the lexer and parser (JSON)
(in-package :cl)

(defpackage :elvis-parsley
  (:use :cl
        :alexandria)
  (:export :parse))

(in-package :elvis-parsley)

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

(defun temp-parse (token-stream)
  (declare (optimize (debug 3)))
  ;; Each parse-function accepts a token and a state which is a symbol :key or :value indicating what exactly we're looking (useful for naming and stuff)
  ;;TODO parse-array
  (labels ((parse-object (token-stream)
             `(:type :object
               :key-value-pairs ,(labels ((parse-key-value-pairs (token-stream key-value-pairs)
                                            (let ((current-token (car token-stream)))
                                              (if (and (eq :punctuation (getf current-token :type))
                                                       (char= #\} (getf current-token :value)))
                                                  key-value-pairs
                                                    ;; The next call should be
                                                  (parse-key-value-pairs (cdddr token-stream) (append key-value-pairs `(:key ,(getf current-token :value)
                                                                                                                             ;; The value occurs AFTER the ":" token
                                                                                                                        :value ,(temp-parse (cddr token-stream)))))))))
                                   (parse-key-value-pairs token-stream '()))))
           (parse-string (token-stream)
             `(:type :string :value ,(getf (car token-stream) :value))))
    (let ((current-token (car token-stream)))
      (cond ((and (eq :punctuation (getf current-token :type)) (char= #\{ (getf current-token :value))) (parse-object (cdr token-stream)))
            ((eq :string (getf current-token :type)) (parse-string token-stream))))))

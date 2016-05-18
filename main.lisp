(in-package :cl)

(defpackage :elvis-parsley
  (:use :cl
        :alexandria)
  (:export :parse))

(in-package :elvis-parsley)

(defclass ast ()
  (source :initarg :source
          :accessor :source)
  (tokens :accessor :tokens)
  (tree :accessor :tree))

(defgeneric lex (ast))
(defgeneric parse (ast))

(defmethod lex (ast))

;;TODO need to account for escaped quotes in lexer
;;TODO need to be able to throw "malformed json"
;;TODO add support for exponential numbers
(defparameter *foo* (labels ((contains (current-character search-string)
                               (loop for search-character across search-string
                                     when (char= current-character search-character)
                                       do (return t)))
                             
                             (is-punctuation (current-character)
                               (contains current-character ":,{}[]"))
                             
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
                                     `(:type "keyword" :value ,value)
                                     `(:type "number" :value ,value))))
                             
                             (read-punctuation (stream)
                               `(:type "punctuation" :value ,(read-char stream)))
                             
                             (read-in-string (stream)
                               (read-char stream)
                               (let ((value (read-until-termination stream #'(lambda (current-character)
                                                                               (char= current-character #\")))))
                                 (read-char stream)
                                 `(:type "string" :value ,value))))
                      
                      (with-input-from-string (the-json "{\"test_key\" : \"test_value\", \"another_test\":true , \"yet_another_value\":\"blah\", \"somenumber\":1234, \"another number\":23.5}")
                        (loop for current-character = (peek-char t the-json nil)
                              while current-character
                              collect (cond ((is-punctuation current-character) (read-punctuation the-json))
                                            ((is-open-quote current-character) (read-in-string the-json))
                                            ((is-start-of-keyword current-character) (read-unquoted-string the-json))
                                            ((digit-char-p current-character) (read-unquoted-string the-json)))))))

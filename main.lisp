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

;; Lexer helper functions
(defgeneric contains (ast current))

(defgeneric parse (ast))

(defmethod lex (ast))

(defparameter *foo* (labels ((contains (current-character search-string)
                               (loop for search-character across search-string
                                     when (char= current-character search-character)
                                       do (return t)))
                             (is-punctuation (current-character)
                               (contains current-character ":,{}[]"))
                             (is-start-of-keyword (current-char)
                               (contains current-character "tfn"))
                             (is-open-quote (current-character)
                               (char= current-character #\"))
                             (is-keyword (word)
                               (loop for keyword in '("true" "false" "null")
                                     when (= word keyword)
                                       do (return t)))
                             (read-pair (stream))
                             (read-value (stream))
                             (read-punctuation (stream)
                               `(:type "punctuation" :value ,(read-char stream)))
                             (read-in-string (stream)
                               (read-char stream)
                               (let ((read-value (with-output-to-string (new-string)
                                                   (loop for current-character = (read-char stream)
                                                         until (char= current-character #\")
                                                         do (write-char current-character new-string)))))
                                 (read-char stream)
                                 `(:type "string" :value ,read-value))))
                      (with-input-from-string (the-json "{\"test_key\" : \"test_value\"}")
                        (loop for current-char = (peek-char t the-json nil)
                              while current-char
                              collect (cond ((is-punctuation current-char) (read-punctuation the-json))
                                            ((is-open-quote current-char) (read-in-string the-json)))))))

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
(defgeneric contains (ast current-character search-string))
(defgeneric is-punctuation (ast current-character))
(defgeneric is-start-of-keyword (ast current-character))
(defgeneric is-open-quote (ast current-character))
(defgeneric is-keyword (ast word))

(defgeneric parse (ast))

(defmethod lex (ast))

;;TODO need to account for escaped quotes in lexer
;;TODO need to be able to throw "malformed json"
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
                                     when (= word keyword)
                                       do (return t)))
                             (read-keyword (stream)
                               (let ((read-value (with-output-to-string (new-string)
                                                   (loop for current-character = (peek-char t stream)
                                                         until (or (char= current-character #\})
                                                                   (char= current-character #\])
                                                                   (char= current-character #\,)
                                                                   ;;TODO may need to change this depending on if this is present in a given implementation
                                                                   (char= current-character #\space))
                                                         do (write-char (read-char stream) new-string)))))
                                 `(:type "keyword" :value ,read-value)))
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
                                 `(:type "string" :value ,read-value))))
                      (with-input-from-string (the-json "{\"test_key\" : \"test_value\", \"another_test\":true , \"yet_another_value\":\"blah\"}")
                        (loop for current-character = (peek-char t the-json nil)
                              while current-character
                              collect (cond ((is-punctuation current-character) (read-punctuation the-json))
                                            ((is-open-quote current-character) (read-in-string the-json))
                                            ((is-start-of-keyword current-character) (read-keyword the-json)))))))

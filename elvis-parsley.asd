(in-package :asdf-user)
(defsystem "elvis-parsley"
  :version "1.0.0"
  :author "Bryan Baraoidan"
  :license "MIT"
  :depends-on (:alexandria
               :cl-ppcre
               :prove)
  :components ((:file "main")))

(in-package :cl-user)
(defpackage cl-jenkins-asd
  (:use :cl :asdf))
(in-package :cl-jenkins-asd)

(defsystem cl-jenkins
  :version "0.1"
  :author "Jeremy Keffer <jkeffer@purestorage.com>"
  :license "Apache License, Version 2.0"
  :depends-on (:cl-ppcre
               :alexandria
               :cl-utilities

               ;; JSON parser
               :jonathan

               ;; HTTP client
               :drakma

               ;; XML handling
               :xmls

               ;; Logging
               :verbose)
  :components ((:module "./"
                :components
                ((:file "xml-utils")
                 (:file "label-matcher")
                 (:file "cl-jenkins" :depends-on ("xml-utils"
                                                 "label-matcher")))))
  :description "A Common Lisp library for interacting with Jenkins")

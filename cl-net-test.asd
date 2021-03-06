;;;; cl-net-test.asd

(asdf:defsystem #:cl-net-test
  :version "0.1"
  :serial t
  :description "Various utils to test ITEP network"
  :author "Alexandr Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:clesh #:cl-ppcre #:cl-interpol #:iterate #:cl-smtp
		       #:cl-yaclyaml #:esrap-liquid
		       #:cl-cron)
  :components ((:file "package")
               (:file "cl-net-test")))


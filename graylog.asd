(asdf:defsystem #:graylog
  :description "Graylog Lisp client"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "graylog"))
  :depends-on (:local-time
			   :usocket
               :babel
               :salza2
               :cl-json))

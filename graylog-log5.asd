(asdf:defsystem #:graylog-log5
  :description "Log5 Graylog plugin"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "log5"))
  :depends-on (:graylog
               :log5))

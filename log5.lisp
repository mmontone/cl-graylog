(defpackage :graylog/log5
  (:use :cl)
  (:export :graylog-sender))

(in-package :graylog/log5)

(defclass graylog-sender (log5::sender-with-categories log5:stream-sender-mixin)
  ((host :initarg :host
         :initform "localhost"
         :accessor graylog-host)
   (port :initarg :port
         :initform 12201
         :accessor graylog-port)
   (connection :accessor graylog-connection)))

(defmethod print-object ((sender graylog-sender) stream)
  (print-unreadable-object (sender stream :type t :identity t)
    (format stream "~a at ~A:~A"
            (log5::name sender)
            (graylog-host sender)
            (graylog-port sender))))

(defmethod initialize-instance :after ((sender graylog-sender) &rest initargs)
  (setf (graylog-connection sender)
        (graylog:connect-graylog :host (graylog-host sender)
                                 :port (graylog-port sender))))

(defun ensure-graylog-connection (sender)
  (assert (graylog-connection sender) nil "Not connected to graylog"))

(defmethod log5::start-handling ((sender graylog-sender))
  '((ensure-graylog-connection log5::sender)))

(defmethod log5::finish-handling ((sender graylog-sender))
  `((graylog:graylog (get-output-stream-string log5::stream)
                     :connection (graylog-connection log5::sender))))


  

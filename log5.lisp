(defpackage :graylog/log5
  (:use :cl)
  (:export :graylog-sender))

(in-package :graylog/log5)

(defclass graylog-sender (log5::sender-with-categories log5:basic-sender)
  ((host :initarg :host
         :initform "localhost"
         :accessor graylog-host)
   (port :initarg :port
         :initform 12201
         :accessor graylog-port)
   (hostname :initarg :hostname
             :initform "lisp"
             :accessor graylog-hostname)
   (connection :accessor graylog-connection)))

(defmethod print-object ((sender graylog-sender) stream)
  (print-unreadable-object (sender stream :type t :identity t)
    (format stream "~a at ~A:~A"
            (log5::name sender)
            (graylog-host sender)
            (graylog-port sender))))

(defmethod initialize-instance :after ((sender graylog-sender) &rest initargs)
  (setf (slot-value sender 'log5::handle-message-fn)
        'graylog-handle-message)
  (setf (graylog-connection sender)
        (graylog:connect-graylog :host (graylog-host sender)
                                 :port (graylog-port sender)
                                 :hostname (graylog-hostname sender))))

(defmethod log5::handle-output ((sender graylog-sender) output)
  ;; This is just for plugging
  )

(defun ensure-graylog-connection (sender)
  (assert (graylog-connection sender) nil "Not connected to graylog"))

(defmethod log5::start-handling ((sender graylog-sender))
  '((ensure-graylog-connection log5::sender)))

;; Graylog severity levels

;; 0 	Emergency 	emerg 	panic[7] 	System is unusable.

;; A panic condition.[8]
;; 1 	Alert 	alert 		Action must be taken immediately.

;; A condition that should be corrected immediately, such as a corrupted system database.[8]
;; 2 	Critical 	crit 		Critical conditions, such as hard device errors.[8]
;; 3 	Error 	err 	error[7] 	Error conditions.
;; 4 	Warning 	warning 	warn[7] 	Warning conditions.
;; 5 	Notice 	notice 		Normal but significant conditions.

;; Conditions that are not error conditions, but that may require special handling.[8]
;; 6 	Informational 	info 		Informational messages.
;; 7 	Debug 	debug 		Debug-level messages.

(defun graylog-level (category)
  (case (log5::category-name category)
    ((log5:info log5:info+) 6)
    ((log5:warn log5:warn+) 4)
    ((log5:error log5:error+) 3)
    (log5:fatal 2)
    (t nil)))

(defun graylog-context-arguments (log5-context)
  (loop for ctx in log5-context
     appending (list (car ctx)
                     (cdr ctx))))

(defun graylog-handle-message (category-id sender message)
  (let ((category (log5::id->category category-id)))
    (apply #'graylog:graylog message
           :connection (graylog-connection sender)
           :level (or (graylog-level category) 6)
           :backtrace (when (member (log5::category-name category)
                                    '(log5:error log5:error+))
                        (trivial-backtrace:backtrace-string))
           (graylog-context-arguments (log5::log5-context (log5::log-manager))))))

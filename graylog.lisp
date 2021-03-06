(in-package :graylog)

(defvar *graylog-connection* nil)
(defvar *default-gelf-port* 12201)
(defvar *default-host-name* "lisp")

(defun connect-graylog (&key (host "localhost")
                          (port *default-gelf-port*)
                          (hostname *default-host-name*))
  "Connect to a graylog instance

HOSTNAME: According to GELF spec, it can be the name of the host, source or application that sent this message"
  (list :socket
        (usocket:socket-connect host port
                                :protocol :datagram
                                :element-type '(unsigned-byte 8))
        :hostname hostname))

(defun connect-graylog-toplevel (&rest args)
  "Connect to graylog and set the global connection"
  (setf *graylog-connection*
        (apply #'connect-graylog args)))

(defun call-with-graylog-connection (function &key (host "localhost")
                                                (port *default-gelf-port*)
                                                (hostname *default-host-name*))
  (let ((*graylog-connection*
         (list :socket
               (usocket:socket-connect host port
                                       :protocol :datagram
                                       :element-type '(unsigned-byte 8))
               :hostname hostname)))
    (funcall function)))

(defmacro with-graylog-connection ((&key (host "localhost") (port *default-gelf-port*)) &body body)
  "Run body within the scope of a graylog connection"
  `(call-with-graylog-connection
    (lambda () @,body)
    :host ,host :port ,port))

(defun graylog-connection (&key (error-p t))
  "Return the current graylog connection"
  (or *graylog-connection*
      (and error-p
           (error "Graylog: not connected"))))

(defun graylog (message &rest args &key (level 1) backtrace host
                                     (connection (graylog-connection))
                                     &allow-other-keys)
  "Log to graylog using GELF

https://www.graylog.org/resources/gelf/

Version 1.1 (11/2013)
A GELF message is a GZIP’d or ZLIB’d JSON string with the following fields:

version string (UTF-8)

GELF spec version – “1.1”; MUST be set by client library.

host string (UTF-8)

the name of the host, source or application that sent this message; MUST be set by client library.

short_message string (UTF-8)

a short descriptive message; MUST be set by client library.

full_message string (UTF-8)

a long message that can i.e. contain a backtrace; optional.

timestamp number

Seconds since UNIX epoch with optional decimal places for milliseconds; SHOULD be set by client library. Will be set to NOW by server if absent.

level number

the level equal to the standard syslog levels; optional, default is 1 (ALERT).

facility string (UTF-8)

optional, deprecated. Send as additional field instead.

line number

the line in a file that caused the error (decimal); optional, deprecated. Send as additional field instead.

file string (UTF-8)

the file (with path if you want) that caused the error (string); optional, deprecated. Send as additional field instead.

_[additional field] string (UTF-8) or number

every field you send and prefix with a _ (underscore) will be treated as an additional field. Allowed characters in field names are any word character (letter, number, underscore), dashes and dots. The verifying regular expression is: ^[\w\.\-]*$

Libraries SHOULD not allow to send id as additional field (_id). Graylog server nodes omit this field automatically."
  (let* ((msg (salza2:compress-data
               (babel:string-to-octets
                (json:encode-json-plist-to-string
                 `(:version "1.1"
                            :host ,(or host (getf connection :hostname))
                            :|short_message| ,message
                            ,@(when backtrace
                                (list :|full_message| backtrace))
                            :timestamp ,(local-time:timestamp-to-unix (local-time:now))
                            :level ,level
                            ,@(loop
                                 :for key :in args :by #'cddr
                                 :for value :in (cdr args) :by #'cddr
                                 :when (not (member key '(:host :level :backtrace :connection)))
                                 :appending (list (read-from-string (format nil "|_~A|" key))
                                                  value))))
                :encoding :utf-8)
               'salza2:zlib-compressor)))
    (usocket:socket-send (getf connection :socket)
                         msg (length msg))))

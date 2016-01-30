# CL-GRAYLOG

Graylog Common Lisp client

https://www.graylog.org/

## Install

Install graylog server. For easy setup, you can use docker:

```
$ docker pull graylog2/allinone
$ docker run -t -p 9000:9000 -p 12201:12201 graylog2/allinone
```

Load the library:

```lisp
(ql:quickload :graylog)
```

## Usage

```lisp
(graylog:connect-graylog-toplevel)
(graylog:graylog "hello world")
```

## Log5

The library comes with a Log5 sender so you can simply use Log5 to log to Graylog

```lisp
(ql:quickload :graylog-log5)

(log5:defcategory app)

(log5:start-sender 'app
        (graylog/log5:graylog-sender)
        :category-spec '(app log5:info+)
        :output-spec '(log5:time log5:category log5:message))

(log5:log-for app "Hello from log5")
```
## Functions

### connect-graylog

```lisp
(&key (host "localhost") (port *default-gelf-port*)
 (hostname *default-host-name*))
```

Connect to a graylog instance

HOSTNAME: According to GELF spec, it can be the name of the host, source or application that sent this message

### connect-graylog-toplevel

```lisp
(&rest args)
```

Connect to graylog and set the global connection


### graylog

```lisp
(message &rest args &key (level 1) backtrace host
 (connection (graylog-connection)))
```

Log to graylog using GELF



[https://www.graylog.org/resources/gelf/](https://www.graylog.org/resources/gelf/)

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

every field you send and prefix with a _ (underscore) will be treated as an additional field. Allowed characters in field names are any word character (letter, number, underscore), dashes and dots. The verifying regular expression is: ^[w.-]*$

Libraries SHOULD not allow to send id as additional field (_id). Graylog server nodes omit this field automatically.

### graylog-connection

```lisp
(&key (error-p t))
```

Return the current graylog connection

## Macros
### with-graylog-connection

```lisp
((&key (host) (port)) &body body)
```

Run body within the scope of a graylog connection
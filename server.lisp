(in-package :simple-http-server)

(defclass server ()
  ((name
    :initarg :name
    :initform (package-name *package*)
    :reader server-name)
   (port
    :initarg :port
    :reader server-port)
   (address
    :initarg :address
    :initform "127.0.0.1"
    :reader server-address)
   (handlers
    :initform '()
    :accessor server-handlers)
   (document-root
    :initform nil
    :initarg :document-root
    :reader server-document-root)
   (process
    :accessor server-process)))

(defun publish (server &key path (method :get) function)
  (push (list :path path :method method :function function)
        (server-handlers server)))

(defun start (server)
  (multiple-value-bind (process startup-condition)
      (comm:start-up-server
       :service (server-port server)
       :address (server-address server)
       :function (lambda (socket-handle)
                   (apply-with-error-handle 'handle-new-connection server socket-handle)))
    (when startup-condition
      (error startup-condition))
    (setf (server-process server) process)))

(defun stop (server)
  (comm:server-terminate (server-process server)))

(defun handle-new-connection (server socket-handle)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket socket-handle
                               :direction :io
                               :element-type 'base-char)))
    (mp:process-run-function "connection" () 'handle-connection server stream)))

(defun handle-connection (server stream)
  (loop
    (let ((request (read-http-request stream)))
      (unless request (return))
      (write-http-response server request stream)
      (unless (request-keep-alive-p request) (return)))))

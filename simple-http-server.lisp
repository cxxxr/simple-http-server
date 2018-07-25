(in-package :simple-http-server)

(defparameter *server-name* (package-name *package*))

(defclass server ()
  ((port
    :initarg :port
    :reader server-port)
   (address
    :initarg :address
    :initform "127.0.0.1"
    :reader server-address)
   (handlers
    :initform '()
    :accessor server-handlers)
   (process
    :accessor server-process)))

(defstruct http-request
  method
  path
  version
  fields
  message-body)

(defstruct (http-response (:conc-name response-))
  content-type)

(defun start (server)
  (multiple-value-bind (process startup-condition)
      (comm:start-up-server
       :service (server-port server)
       :address (server-address server)
       :function (lambda (socket-handle)
                   (apply-with-error-handle 'process-http-request server socket-handle)))
    (when startup-condition
      (error startup-condition))
    (setf (server-process server) process)))

(defun stop (server)
  (comm:server-terminate (server-process server)))

(defun publish (server &key path (method :get) function)
  (push (list :path path :method method :function function)
        (server-handlers server)))

(defun read-http-request (stream)
  (let ((request (make-http-request)))
    (labels ((method-string-to-keyword (str)
               (intern str :keyword))
             (request-line ()
               ;; TODO: ここで不正なrequest-lineなら404を返す (RFC7230 3.1.1)
               (let ((line (read-line stream)))
                 (setf line (string-right-trim '(#\Return) line))
                 (destructuring-bind (method path version)
                     (split-sequence " " line)
                   (setf (http-request-method request) (method-string-to-keyword method)
                         (http-request-path request) path
                         (http-request-version request) version))))
             (header-fields ()
               (let ((fields '()))
                 (loop :for line := (read-line stream nil nil)
                       :until (or (null line)
                                  (string= line ""))
                       :do (destructuring-bind (key value)
                               (split-sequence ":" line :max-elements 2)
                             (push (cons key
                                         (string-right-trim ""
                                                            (string-trim " " value)))
                                   fields)))
                 (setf (http-request-fields request)
                       (nreverse fields))))
             (content-length ()
               (if-let (v (assoc "Content-Length" (http-request-fields request)
                                 :test #'string=))
                   (parse-integer v)
                 0))
             (message-body ()
               (let* ((content-length (content-length))
                      (buffer (make-array content-length)))
                 (read-sequence buffer stream)
                 (setf (http-request-message-body request) buffer))))
      (request-line)
      (header-fields)
      (message-body)
      request)))

(defun find-handler (server request)
  (dolist (handler (server-handlers server))
    (destructuring-bind (&key path method function) handler
      (when (and (equal path (http-request-path request))
                 (eq method (http-request-method request)))
        (return function)))))

(defun write-header-fields (alist stream)
  (loop :for (key . value) :in alist :do
        (format stream "~A: ~A" key value)
        (write-sequence #(#\return #\linefeed) stream)))

(defun write-http-response (server request stream)
  (let ((function (find-handler server request)))
    (when function
      (let* ((response (make-http-response))
             (body (funcall function request response)))
        (write-line "HTTP/1.0 200 OK" stream)
        (write-header-fields `(("Server" . ,*server-name*)
                               ("Date" . ,(rfc7231-date))
                               ,@(and (response-content-type response)
                                      `(("Content-Type" . ,(response-content-type response))))
                               ,@(and body
                                      `(("Content-Length" . ,(length body)))))
                             stream)
        (write-sequence #(#\return #\linefeed) stream)
        (write-sequence body stream)
        (force-output stream)))))

(defun process-http-request (server socket-handle)
  (let* ((stream (make-instance 'comm:socket-stream
                                :socket socket-handle
                                :direction :io
                                :element-type 'base-char))
         (request (read-http-request stream)))
    (pprint request *log-stream*)
    (write-http-response server request stream)))

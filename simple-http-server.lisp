(in-package :simple-http-server)

(defparameter *server-name* (package-name *package*))

(defclass server ()
  ((port :initarg :port
         :reader server-port)
   (address :initarg :address
            :initform "127.0.0.1"
            :reader server-address)
   (handlers :initform '()
             :accessor server-handlers)))

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
    process))

(defun publish (server &key path (method :get) function)
  (push (list :path path :method method :function function)
        (server-handlers server)))

(defun apply-with-error-handle (function &rest args)
  (handler-bind ((error (lambda (condition)
                          (capi:display-message "~A"
                                                (with-output-to-string (out)
                                                  (format out "~A~%" condition)
                                                  (uiop:print-backtrace :stream out
                                                                        :condition condition)))
                          (return-from apply-with-error-handle))))
    (apply function args)))

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
               (let ((fields (make-hash-table :test 'equal)))
                 (loop :for line := (read-line stream nil nil)
                       :until (or (null line)
                                  (string= line ""))
                       :do (destructuring-bind (key value)
                               (split-sequence ":" line :max-elements 2)
                             (setf (gethash key fields)
                                   (string-right-trim ""
                                                      (string-trim " " value)))))
                 (setf (http-request-fields request)
                       fields)))
             (content-length ()
               (if-let (v (gethash "Content-Length" (http-request-fields request)))
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

(defun get-current-date ()
  (multiple-value-bind (second minute hour date month year day)
      (get-decoded-time)
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~D ~
		 ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
		 ~D ~D:~D:~D GMT"
            day date month year hour minute second)))

(defun write-http-response (server request stream)
  (let ((function (find-handler server request)))
    (when function
      (let* ((response (make-http-response))
             (body (funcall function request response)))
        (write-line "HTTP/1.0 200 OK" stream)
        (write-header-fields `(("Server" . ,*server-name*)
                               ("Date" . ,(get-current-date))
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
    (write-http-response server request stream)))


(defvar *hello-counter* 0)

(defun index (request response)
  (declare (ignore request))
  (setf (response-content-type response) "text/html")
  (incf *hello-counter*)
  (format nil "<h1>Counter: ~D</h1>" *hello-counter*))

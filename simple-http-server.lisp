(in-package :simple-http-server)

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
   (document-root
    :initform nil
    :initarg :document-root
    :reader server-document-root)
   (process
    :accessor server-process)))

(defstruct request
  method
  path
  query
  version
  fields
  message-body)

(defstruct response
  status)

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

(defun parse-query (str)
  (when-let (pos (position #\# str))
    (setf str (subseq str 0 pos)))
  (mapcar (lambda (str)
            (split-sequence "=" str))
          (split-sequence "&" str)))

(defun read-http-request (stream)
  (let ((request (make-request)))
    (labels ((method-string-to-keyword (str)
               (intern str :keyword))
             (request-line ()
               ;; TODO: ここで不正なrequest-lineなら404を返す (RFC7230 3.1.1)
               (let ((line (read-line stream))
                     query-str)
                 (setf line (string-right-trim '(#\Return) line))
                 (destructuring-bind (method path version)
                     (split-sequence " " line)
                   (when-let (query-pos (position #\? path))
                     (setf query-str (subseq path (1+ query-pos))
                           path (subseq path 0 query-pos)))
                   (setf (request-method request) (method-string-to-keyword method)
                         (request-path request) path
                         (request-query request) (and query-str (parse-query query-str))
                         (request-version request) version))))
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
                 (setf (request-fields request)
                       (nreverse fields))))
             (content-length ()
               (if-let (v (cdr (assoc "Content-Length" (request-fields request)
                                      :test #'string=)))
                   (parse-integer v)
                 0))
             (message-body ()
               (let* ((content-length (content-length))
                      (buffer (make-array content-length)))
                 (read-sequence buffer stream)
                 (setf (request-message-body request) buffer))))
      (request-line)
      (header-fields)
      (message-body)
      (pprint request *log-stream*)
      (terpri *log-stream*)
      (force-output *log-stream*)
      request)))

(defun find-handler (server request)
  (dolist (handler (server-handlers server))
    (destructuring-bind (&key path method function) handler
      (when (and (equal path (request-path request))
                 (eq method (request-method request)))
        (return function)))))

(defun write-header-fields (alist stream)
  (loop :for (key . value) :in alist :do
        (format stream "~A: ~A" key value)
        (write-sequence #(#\return #\linefeed) stream)))

(defun path-from-document-root (path root)
  (make-pathname :directory (cons :absolute (append (rest (pathname-directory root))
                                                    (rest (pathname-directory path))))
                 :name (pathname-name path)
                 :type (pathname-type path)))

(defun in-root-directory-p (path root)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (setf root (probe-file root))
  (setf path (probe-file path))
  (when (and root path)
    (let ((root-directory (pathname-directory root))
          (path-directory (pathname-directory path)))
      (assert (eq :absolute (first root-directory)))
      (assert (eq :absolute (first path-directory)))
      (pop root-directory)
      (pop path-directory)
      (loop :for rest1 :on root-directory
            :for rest2 :on path-directory
            :do (cond ((not (equal (car rest1) (car rest2)))
                       (return nil))
                      ((and (null (cdr rest2))
                            (not (null (cdr rest1))))
                       (return nil)))
            :finally (return t)))))

(defun guess-content-type-from-pathname (pathname)
  (assoc (pathname-type pathname)
         '(("html" . "text/html")
           ("htm" . "text/html")
           ("txt" . "text/plain")
           ("css" . "text/css")
           ("png" . "image/png")
           ("jpg" . "image/jpeg")
           ("jpeg" . "image/jpeg")
           ("gif" . "image/gif"))
         :test #'string=))

(defun response-path (server path stream)
  (when (uiop:pathname-equal path (server-document-root server))
    (setf path (merge-pathnames "index.html" path)))
  (when (uiop:directory-pathname-p path)
    (404-not-found stream)
    (return-from response-path))
  (write-line "HTTP/1.1 200 OK" stream)
  (let ((body (read-file-base-char-string path)))
    (write-header-fields `(("Server" . ,*server-name*)
                           ("Date" . ,(rfc7231-date))
                           ("Connection" . "close")
                           ("Content-Type" . ,(guess-content-type-from-pathname path))
                           ("Content-Length" . ,(length body)))
                         stream)
    (write-sequence #(#\return #\linefeed) stream)
    (write-sequence body stream)
    (force-output stream)))

(defun 404-not-found (stream)
  (write-line "HTTP/1.1 404 Not Found" stream)
  (let ((body "<h1>not found</h1>"))
    (write-header-fields `(("Server" . ,*server-name*)
                           ("Date" . ,(rfc7231-date))
                           ("Connection" . "close")
                           ("Content-Type" . "text/html")
                           ("Content-Length" . ,(length body)))
                         stream)
    (write-sequence #(#\return #\linefeed) stream)
    (write-sequence body stream)
    (force-output stream)))

(defun write-http-response (server request stream)
  (if-let (function (find-handler server request))
      (let* ((response (make-response))
             (body (funcall function request response)))
        (write-line "HTTP/1.1 200 OK" stream)
        (write-header-fields `(("Server" . ,*server-name*)
                               ("Date" . ,(rfc7231-date))
                               ("Connection" . "close")
                               ,@(and (response-content-type response)
                                      `(("Content-Type" . ,(response-content-type response))))
                               ,@(and body
                                      `(("Content-Length" . ,(length body)))))
                             stream)
        (write-sequence #(#\return #\linefeed) stream)
        (write-sequence body stream)
        (force-output stream))
    (if-let (document-root (and (equal "GET" (request-method request))
                                (server-document-root server)))
        (let ((path (path-from-document-root (request-path request) document-root)))
          (if (in-root-directory-p path document-root)
              (response-path server path stream)
              (404-not-found stream)))
      (404-not-found stream))))

(defun process-http-request (server socket-handle)
  (let* ((stream (make-instance 'comm:socket-stream
                                :socket socket-handle
                                :direction :io
                                :element-type 'base-char))
         (request (read-http-request stream)))
    (write-http-response server request stream)))

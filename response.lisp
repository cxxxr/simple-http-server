(in-package :simple-http-server)

(defvar *status-line-table*
  (let ((table (make-hash-table)))
    (loop :for (code msg) :in
          '((100 "Continue")
            (101 "Switching Protocols")
            (200 "OK")
            (201 "Created")
            (202 "Accepted")
            (203 "Non-Authoriative Information")
            (204 "No Content")
            (205 "Reset Content")
            (300 "Multiple Choices")
            (301 "Moved Permanently")
            (302 "Found")
            (303 "See Other")
            (305 "Use Proxy")
            (307 "Temporary Redirect")
            (400 "Bad Request")
            (402 "Payment Required")
            (403 "Forbidden")
            (404 "Not Found")
            (405 "Method Not Allowed")
            (406 "Not Acceptable")
            (408 "Request Timeout")
            (409 "Conflict")
            (410 "Gone")
            (411 "Length Required")
            (413 "Payload Too Large")
            (414 "URI Too Long")
            (415 "Unsupported Media Type")
            (417 "Expectation Failed")
            (426 "Upgrade Required")
            (428 "Precondition Required")
            (429 "Too Many Requests")
            (431 "Request Header Fields Too Large")
            (500 "Internal Server Error")
            (501 "Not Implemented")
            (502 "Bad Gateway")
            (503 "Service Unavailable")
            (504 "Gateway Timeout")
            (505 "HTTP Version Not Supported")
            (511 "Network Authentication Required"))
          :do (setf (gethash code table)
                    (format nil "HTTP/1.1 ~D ~A~C~C" code msg #\return #\linefeed)))
    table))

(defclass cookie ()
  ((name
    :initarg :name
    :reader cookie-name)
   (value
    :initarg :value
    :reader cookie-value)
   (expires
    :initform nil
    :reader cookie-expires)
   (max-age
    :initform nil
    :reader cookie-max-age)
   (domain
    :initform nil
    :reader cookie-domain)
   (path
    :initform nil
    :reader cookie-path)
   (secure
    :initform nil
    :reader cookie-secure)
   (http-only
    :initform nil
    :reader cookie-http-only)))

(defclass response ()
  ((content-type
    :initform nil
    :initarg :content-type
    :accessor response-content-type)
   (cookies
    :initform nil
    :accessor response-cookies)
   (status
    :initform 200
    :initarg :status
    :accessor response-status)))

(defun find-handler (server request)
  (dolist (handler (server-handlers server))
    (destructuring-bind (&key path method function) handler
      (when (and (equal path (request-path request))
                 (eq method (request-method request)))
        (return function)))))

(defun call-handler (function request response)
  (let ((body (funcall function request response)))
    (to-simple-char-string (babel:string-to-octets body))))

(defun write-newline (stream)
  (write-sequence #(#\return #\linefeed) stream))

(defun write-status-line (stream code)
  (write-string (gethash code *status-line-table*) stream))

(defun write-header-field (stream key value)
  (format stream "~A: ~A" key value)
  (write-newline stream))

(defun bake-cookie (cookie)
  (with-output-to-string (stream)
    (format stream "~A = ~A"
            (url-encode (cookie-name cookie))
            (url-encode (cookie-value cookie)))
    (when-let (expires (cookie-expires cookie))
      (format stream "; Expires=~A" (rfc-1123-date expires)))
    (when-let (max-age (cookie-max-age cookie))
      (format stream "; Max-Age=~A" max-age))
    (when-let (domain (cookie-domain cookie))
      (format stream "; Domain=~A" domain))
    (when-let (path (cookie-path cookie))
      (format stream "; Path=~A" path))
    (when (cookie-secure cookie)
      (write-string "; Secure" stream))
    (when (cookie-http-only cookie)
      (write-string "; HttpOnly" stream))))

(defun write-set-cookie (stream cookies)
  (dolist (cookie cookies)
    (write-header-field stream "Set-Cookie" (bake-cookie cookie))))

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
    (404-not-found server stream)
    (return-from response-path))
  (write-status-line stream 200)
  (let ((body (read-file-base-char-string path)))
    (write-header-field stream "Server" (server-name server))
    (write-header-field stream "Date" (rfc-1123-date))
    (write-header-field stream "Connection" "close")
    (write-header-field stream "Content-Type" (guess-content-type-from-pathname path))
    (write-header-field stream "Content-Length" (length body))
    (write-newline stream)
    (write-sequence body stream)
    (force-output stream)))

(defun 404-not-found (server stream)
  (let ((body "<h1>404 not found</h1>"))
    (write-status-line stream 404)
    (write-header-field stream "Server" (server-name server))
    (write-header-field stream "Date" (rfc-1123-date))
    (write-header-field stream "Connection" "close")
    (write-header-field stream "Content-Type" "text/html")
    (write-header-field stream "Content-Length" (length body))
    (write-newline stream)
    (write-sequence body stream)
    (force-output stream)))

(defun 400-bad-request (server stream)
  (let ((body "<h1>400 bad request</h1>"))
    (write-status-line stream 400)
    (write-header-field stream "Server" (server-name server))
    (write-header-field stream "Date" (rfc-1123-date))
    (write-header-field stream "Connection" "close")
    (write-header-field stream "Content-Type" "text/html")
    (write-header-field stream "Content-Length" (length body))
    (write-newline stream)
    (write-sequence body stream)
    (force-output stream)))

(defun write-http-response (server request stream)
  (when (request-condition request)
    (400-bad-request server stream)
    (return-from write-http-response))
  (if-let (function (find-handler server request))
      (let* ((response (make-instance 'response))
             (body (call-handler function request response)))
        (write-status-line stream (response-status response))
        (write-header-field stream "Server" (server-name server))
        (write-header-field stream "Date" (rfc-1123-date))
        (write-header-field stream "Connection" "close")
        (when (response-content-type response)
          (write-header-field stream "Content-Type" (response-content-type response)))
        (when (response-cookies response)
          (write-set-cookie stream (response-cookies response)))
        (when body
          (write-header-field stream "Content-Length" (length body)))
        (write-newline stream)
        (write-sequence body stream)
        (force-output stream))
    (if-let (document-root (and (eq :GET (request-method request))
                                (server-document-root server)))
        (let ((path (path-from-document-root (request-path request) document-root)))
          (if (in-root-directory-p path document-root)
              (response-path server path stream)
              (404-not-found server stream)))
      (404-not-found server stream))))

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
    :accessor response-status)
   (headers
    :initform nil
    :accessor response-headers)
   (body
    :initform nil
    :accessor response-body)))

(defun add-response-header (response key value)
  (push (cons key value) (response-headers response)))

(defun add-default-resopnse-headers (response server)
  (add-response-header response "Server" (server-name server))
  (add-response-header response "Date" (rfc-1123-date))
  (add-response-header response "Connection" "close"))

(defun set-standard-response (response server status content-type body)
  (setf (response-status response) status)
  (add-default-resopnse-headers response server)
  (add-response-header response "Content-Type" content-type)
  (add-response-header response "Content-Length" (length body))
  (setf (response-body response) body))

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

(defun 404-not-found (server response)
  (set-standard-response response server 404 "text/html" "<h1>404 not found</h1>"))

(defun 400-bad-request (server response)
  (set-standard-response response server 400 "text/html" "<h1>400 bad request</h1>"))

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

(defun response-path (server path response)
  (when (uiop:pathname-equal path (server-document-root server))
    (setf path (merge-pathnames "index.html" path)))
  (when (uiop:directory-pathname-p path)
    (404-not-found server response)
    (return-from response-path))
  (let ((body (read-file-base-char-string path)))
    (set-standard-response response server 200 (guess-content-type-from-pathname path) body)))

(defun write-response (stream response)
  (write-status-line stream (response-status response))
  (loop :for (k . v) :in (response-headers response)
        :do (write-header-field stream k v))
  (write-set-cookie stream (response-cookies response))
  (write-newline stream)
  (write-sequence (response-body response) stream)
  (finish-output stream))

(defun write-http-response (server request stream)
  (let ((response (make-instance 'response)))
    (if (request-condition request)
        (400-bad-request server response)
        (if-let (function (find-handler server request))
            (let ((body (call-handler function request response)))
              (setf (response-body response) body))
          (if-let (document-root (and (eq :GET (request-method request))
                                      (server-document-root server)))
              (let ((path (path-from-document-root (request-path request) document-root)))
                (if (in-root-directory-p path document-root)
                    (response-path server path response)
                    (404-not-found server response)))
            (404-not-found server response))))
    (setf (response-headers response)
          (nreverse (response-headers response)))
    (write-response stream response)))

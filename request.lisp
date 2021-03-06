(in-package :simple-http-server)

(defparameter +allowed-http-methods+
  '(:GET :HEAD :POST :PUT :DELETE :CONNECT :OPTIONS :TRACE))

(defclass request ()
  ((method
    :initarg :method
    :accessor request-method)
   (path
    :initarg :path
    :accessor request-path)
   (query
    :initarg :query
    :accessor request-query)
   (version
    :initarg :version
    :accessor request-version)
   (fields
    :initarg :fields
    :accessor request-fields)
   (message-body
    :initarg :message-body
    :accessor request-message-body)
   (cookie-values
    :accessor request-cookie-values)
   (keep-alive-p
    :initform nil
    :accessor request-keep-alive-p)
   (condition
    :initform nil
    :accessor request-condition)))

(defstruct multipart-form-data
  headers
  value)

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream)
    (format stream "REQUEST~%")
    (loop :for slot :in (clos:class-slots (find-class 'request))
          :for name := (clos:slot-definition-name slot)
          :do (format stream "~A: ~S~%" name (slot-value request name)))))

(defun request-field-value (request key)
  (cdr (assoc key (request-fields request) :test #'string-equal)))

(defun get-cookie-values (request)
  (let ((cookie (request-field-value request "Cookie")))
    (parse-cookie-string cookie)))

(defmethod request-cookie-values :before ((request request))
  (unless (slot-boundp request 'cookie-values)
    (setf (request-cookie-values request)
          (get-cookie-values request))))

(defun parse-query (str)
  (when-let (pos (position #\# str))
    (setf str (subseq str 0 pos)))
  (mapcar (lambda (str)
            (destructuring-bind (key value) (split-sequence "=" str)
              (cons key value)))
          (split-sequence "&" str)))

(defun boundary-line-p (line boundary)
  (alexandria:starts-with-subseq boundary line))

(defun read-to-boundary (in boundary)
  (loop :for line := (read-line in)
        :until (boundary-line-p line boundary)))

(defun read-multipart-form-data-headers (in)
  (loop :for line := (read-line in)
        :until (string= line (string #\return))
        :collect (let ((parts (split-sequence ";" line)))
                   (destructuring-bind (k v) (split-sequence ":" (first parts))
                     (list* (cons k (string-trim " " v))
                            (loop :for part :in (rest parts)
                                  :collect (destructuring-bind (k v)
                                               (split-sequence "=" part)
                                             (setf k (string-trim " " k))
                                             (setf v (string-trim " " v))
                                             (cons k v))))))))

(defun read-multipart-form-data-value (in boundary)
  (with-output-to-string (out)
    (loop :for line := (read-line in)
          :for n :from 0
          :until (boundary-line-p line boundary)
          :do
          (when (plusp n) (terpri out))
          (write-string line out :end (1- (length line))))))

(defun multipart-form-data (boundary message-body)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (setf boundary (string-append "--" boundary))
  (with-input-from-string (in message-body)
    (read-to-boundary in boundary)
    (loop :with multipart-form-data-list := '()
          :while (peek-char nil in nil nil)
          :do (let ((headers (read-multipart-form-data-headers in))
                    (value (read-multipart-form-data-value in boundary)))
                (push (make-multipart-form-data :headers headers
                                                :value value)
                      multipart-form-data-list))
          :finally (return (nreverse multipart-form-data-list)))))

(defun maybe-set-post-parameters (request)
  (when-let (content-type (request-field-value request "Content-Type"))
    (multiple-value-bind (type subtype parameters)
        (parse-content-type content-type)
      (cond ((and (string-equal type "application")
                  (string-equal subtype "x-www-form-urlencoded"))
             (let ((query (parse-query (request-message-body request))))
               (dolist (elt query)
                 (setf (cdr elt) (url-decode (cdr elt))))
               (setf (request-query request) query)))
            ((and (string-equal type "multipart")
                  (string-equal subtype "form-data"))
             (when-let (boundary (cdr (assoc "boundary" parameters :test #'string=)))
               (setf (request-query request)
                     (multipart-form-data boundary (request-message-body request)))))))))

(defun read-crlf-line (stream)
  (let ((buffer (make-adjustable-string)))
    (loop :for prev := #\null :then char
          :for char := (read-char stream nil #\null)
          :do (cond ((char= char #\null)
                     (return))
                    ((and (char= prev #\return)
                          (char= char #\linefeed))
                     (return buffer))
                    ((char/= prev #\null)
                     (vector-push-extend prev buffer))))))

(defun read-http-request (stream)
  (let ((request (make-instance 'request)))
    (labels ((method-string-to-keyword (str)
               (intern str :keyword))
             (request-line ()
               (let* ((query-str nil)
                      (line (read-crlf-line stream))
                      (tokens (when line (split-sequence " " line))))
                 (when (null line)
                   (error 'http-request-eof))
                 (unless (= 3 (length tokens))
                   (error 'http-request-error))
                 (destructuring-bind (method path version) tokens
                   (setf method (method-string-to-keyword method))
                   (unless (find method +allowed-http-methods+)
                     (error 'http-request-error))
                   (when-let (query-pos (position #\? path))
                     (setf query-str (subseq path (1+ query-pos))
                           path (subseq path 0 query-pos)))
                   (setf (request-method request) method
                         (request-path request) path
                         (request-query request) (and query-str (parse-query query-str))
                         (request-version request) version))))
             (header-fields ()
               (let ((fields '()))
                 (loop :for line := (read-crlf-line stream)
                       :until (or (null line)
                                  (string= line ""))
                       :do (destructuring-bind (key value)
                               (split-sequence ":" line :max-elements 2)
                             (push (cons key (string-trim " " value))
                                   fields)))
                 (setf (request-fields request)
                       (nreverse fields))))
             (content-length ()
               (if-let (v (request-field-value request "Content-Length"))
                   (parse-integer v)
                 0))
             (message-body ()
               (let* ((content-length (content-length))
                      (buffer (make-array content-length)))
                 (read-sequence buffer stream)
                 (setf (request-message-body request) (coerce buffer 'string)))))
      (handler-case
          (progn
            (request-line)
            (header-fields)
            (message-body)
            (maybe-set-post-parameters request))
        (http-request-eof ()
          (return-from read-http-request nil))
        (http-request-error (condition)
          (setf (request-condition request) condition)))
      (unless (request-condition request)
        (when (or (string-equal "http/1.1" (request-version request))
                  (string-equal "keep-alive" (request-field-value request "Connection")))
          (setf (request-keep-alive-p request) t)))
      (pprint request *log-stream*)
      (terpri *log-stream*)
      request)))

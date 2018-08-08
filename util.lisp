(in-package :simple-http-server)

(defun apply-with-error-handle (function &rest args)
  (handler-bind ((error (lambda (condition)
                          (format *log-stream* "~A~%" condition)
                          (uiop:print-backtrace :stream *log-stream*
                                                :condition condition)
                          (return-from apply-with-error-handle))))
    (apply function args)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day) (decode-universal-time time)
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~2,'0D ~
		 ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
		 ~D ~2,'0D:~2,'0D:~2,'0D GMT"
            day date month year hour minute second)))

(defun to-simple-char-string (sequence)
  (map 'string 'code-char sequence))

(defun read-file-base-char-string (filename)
  (with-open-file (stream  filename
                           :direction :input
                           :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream))))
      (read-sequence buffer stream)
      (to-simple-char-string buffer))))

(defun make-adjustable-string ()
  (make-array 0 :fill-pointer 0 :adjustable t :element-type 'simple-char))

(defun url-decode (string)
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop :with i := 0
          :while (< i (length string))
          :for c := (aref string i)
          :do (case c
                (#\%
                 (incf i)
                 (vector-push-extend (parse-integer string :start i :end (+ i 2) :radix 16)
                                     buffer)
                 (incf i 2))
                (#\+
                 (vector-push-extend #.(char-code #\space) buffer)
                 (incf i))
                (otherwise
                 (vector-push-extend (char-code c) buffer)
                 (incf i))))
    (babel:octets-to-string buffer)))

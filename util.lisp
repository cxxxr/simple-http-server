(in-package :simple-http-server)

(defun apply-with-error-handle (function &rest args)
  (handler-bind ((error (lambda (condition)
                          (format *log-stream* "~A~%" condition)
                          (uiop:print-backtrace :stream *log-stream*
                                                :condition condition)
                          (return-from apply-with-error-handle))))
    (apply function args)))

(defun rfc7231-date ()
  (multiple-value-bind (second minute hour date month year day)
      (get-decoded-time)
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~2,'0D ~
		 ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
		 ~D ~2,'0D:~2,'0D:~2,'0D GMT"
            day date month year hour minute second)))

(defun read-file-base-char-string (filename)
  (with-open-file (stream  filename
                           :direction :input
                           :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream))))
      (read-sequence buffer stream)
      (map 'string 'code-char buffer))))

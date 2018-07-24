(in-package :simple-http-server)

(defun rfc7231-date ()
  (multiple-value-bind (second minute hour date month year day)
      (get-decoded-time)
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~2,'0D ~
		 ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
		 ~D ~2,'0D:~2,'0D:~2,'0D GMT"
            day date month year hour minute second)))

(defun apply-with-error-handle (function &rest args)
  (handler-bind ((error (lambda (condition)
                          (capi:display-message "~A"
                                                (with-output-to-string (out)
                                                  (format out "~A~%" condition)
                                                  (uiop:print-backtrace :stream out
                                                                        :condition condition)))
                          (return-from apply-with-error-handle))))
    (apply function args)))

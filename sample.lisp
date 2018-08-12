(in-package :simple-http-server)

(defvar *server*)

(defun test ()
  (setq *server* (make-instance 'server
                                :port 7000
                                :document-root (asdf:system-relative-pathname
                                                :simple-http-server "doc/")))
  (start *server*))

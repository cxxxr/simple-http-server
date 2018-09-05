(in-package :simple-http-server)

(defclass http-request-error (simple-error) ())
(defclass http-request-eof (http-request-error) ())

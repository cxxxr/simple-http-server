(in-package :simple-http-server)

(defstruct reader
  (string "")
  (pos 0))

(defun end-of-string-p (reader)
  (<= (length (reader-string reader)) (reader-pos reader)))

(defun character-at (reader)
  (aref (reader-string reader)
        (reader-pos reader)))

(defun looking-at-p (reader char)
  (if (end-of-string-p reader)
      nil
      (char= (character-at reader) char)))

(defun next-char (reader)
  (prog1 (character-at reader)
    (incf (reader-pos reader))))

(defun read-exact (reader char)
  (assert (char= (next-char reader) char)))

(defun skip-whitespace (reader)
  (loop :until (end-of-string-p reader)
        :for char := (character-at reader)
        :while (char= char #\space)
        :do (next-char reader)))

(defun read-quoted-string (reader)
  (assert (char= (next-char reader) #\"))
  (let ((buffer (make-adjustable-string)))
    (loop :for char := (next-char reader)
          :do (case char
                (#\" (return))
                (#\\ (vector-push-extend (next-char reader) buffer))
                (otherwise (vector-push-extend char buffer))))
    (coerce buffer 'simple-string)))

(defun read-token (reader &optional split-char)
  (let ((buffer (make-adjustable-string)))
    (loop :until (end-of-string-p reader)
          :for char := (character-at reader)
          :do (cond
                ((and split-char (char= char split-char))
                 (return))
                ((char<= #.(code-char #x21) char #.(code-char #x7e))
                 (vector-push-extend char buffer)
                 (next-char reader))
                (t
                 (next-char reader)
                 (return))))
    (coerce buffer 'simple-string)))

(defun read-parameter (reader)
  (let ((token (read-token reader #\=)))
    (read-exact reader #\=)
    (cons token
          (if (looking-at-p reader #\")
              (read-quoted-string reader)
              (read-token reader #\;)))))

(defun parse-media-type (reader)
  (let ((type nil)
        (subtype nil)
        (parameters '()))
    (setf type (read-token reader #\/))
    (read-exact reader #\/)
    (setf subtype (read-token reader #\;))
    (loop
      (skip-whitespace reader)
      (unless (looking-at-p reader #\;) (return))
      (next-char reader)
      (skip-whitespace reader)
      (push (read-parameter reader) parameters))
    (values type subtype (nreverse parameters))))

(defun parse-content-type (string)
  (parse-media-type (make-reader :string string)))

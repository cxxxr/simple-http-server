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

(defun read-media-type (reader)
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
  (read-media-type (make-reader :string string)))

(defun read-cookie-value (reader)
  (labels ((cookie-octet-p (char)
             (or (char= char #.(code-char #x21))
                 (char<= #.(code-char #x23) char #.(code-char #x2B))
                 (char<= #.(code-char #x2D) char #.(code-char #x3A))
                 (char<= #.(code-char #x3C) char #.(code-char #x5B))
                 (char<= #.(code-char #x5D) char #.(code-char #x7E))))
           (read-cookie-octet ()
             (let ((buffer (make-adjustable-string)))
               (loop :until (end-of-string-p reader)
                     :for char := (character-at reader)
                     :while (cookie-octet-p char)
                     :collect (vector-push-extend char buffer)
                     :do (next-char reader))
               (coerce buffer 'simple-string))))
    (read-cookie-octet)))

(defun read-cookie-pair (reader)
  (let (cookie-name cookie-value)
    (setf cookie-name (read-token reader #\=))
    (read-exact reader #\=)
    (setf cookie-value (read-cookie-value reader))
    (cons cookie-name cookie-value)))

(defun read-cookie-string (reader)
  (let ((pairs '()))
    (loop
      (skip-whitespace reader)
      (push (read-cookie-pair reader) pairs)
      (if (looking-at-p reader #\;)
          (next-char reader)
          (return)))
    (nreverse pairs)))

(defun parse-cookie-string (string)
  (read-cookie-string (make-reader :string string)))

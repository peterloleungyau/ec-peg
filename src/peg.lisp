(defpackage :ec-peg
  (:use :cl)
  ;; TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream wrapper
(defstruct char-input
  ;; to wrap around a stream, and track the line number and column
  in
  (filename "" :type string)
  (line 1 :type integer) ;; 1-based
  (col 0 :type integer) ;; 0-based
  )

(defun new-char-input (&key (filename "") in (line 1) (col 0))
  (make-char-input
   :in (if in in (open filename :direction :input))
   :filename filename
   :line line
   :col col))

(defun peek-next-char (in)
  (peek-char nil (char-input-in in) nil nil nil))

(defun read-next-char (in)
  (let ((c (read-char (char-input-in in) nil nil nil)))
    (case c
      ((#\newline)
       (incf (char-input-line in))
       (setf (char-input-col in) 0))
      (t (incf (char-input-col in))))
    c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tmp* (new-char-input :filename "test/test.pas"))

(loop :for c = (read-next-char *tmp*)
      :while c
      :do (format t "~S ~A, ~A: ~S~%"
                  (char-input-filename *tmp*)
                  (char-input-line *tmp*)
                  (char-input-col *tmp*)
                  c))

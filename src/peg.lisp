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
#+nil
(defparameter *tmp* (new-char-input :filename "test/test.pas"))

#+nil
(loop :for c = (read-next-char *tmp*)
      :while c
      :do (format t "~S ~A, ~A: ~S~%"
                  (char-input-filename *tmp*)
                  (char-input-line *tmp*)
                  (char-input-col *tmp*)
                  c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; each expression returns either the primary value T indicating
;; success, or NIL indicating failure.  and each may optionally return
;; other values.

(defmacro ok (&rest args)
  "Unconditional succeed, mainly used for return values."
  `(values t ,@args))

(defmacro ec (c &optional (next '(peek-next)))
  "Match constant, if succeed, will consume the next input."
  `(cond ((eql ,c ,next)
          ;; consume-next could be redefined using macrolet at higher level.
          (consume-next)
          (values t ,c))
         (t nil)))

;; sequence can be obtained with AND if there is an explicit need.
;; but sequence is often implicit in these macros.

(eval-when (:compile-toplevel)
  (defun terminal-exp-p (exp)
    "Predicate for terminal expression.
A non-keyword symbol will be regarded as non-terminal to call the parser.
Keyword is terminal.
A quoted value is terminal.
Other list is regarded as call.
Other values are terminal."
    (cond ((symbolp exp) (keywordp exp))
          ((consp exp) (eq 'quote (car exp)))
          (t t)))

  (defun gen-one (exp &optional (next '(peek-next)))
    "Generate the form for one simple expression EXP, for use in GEN-SEQ.
NEXT is the form to get the next input.
If "
    (cond ((terminal-exp-p exp)
           `(ec ,exp ,next))
          ((symbolp exp)
           ;; non-keyword symbol, call to parser
           (list exp))
          (t ;; non-quote list, a call
           exp)))

  (defun gen-seq (exp &optional (first-input '(peek-next)))
    (cond ((null exp) nil)
          ((terminal-exp-p exp)
           (gen-one exp first-input))
          (t
           `(and ,(gen-one (car exp) first-input)
                 ,@(mapcar #'gen-one (cdr exp))))))
  )

(defmacro is (con )
  ;; TODO
  nil)

(defmacro e@ (vars e &body exps)
  "Short-hand to give name(s) to value(s) of expression E, and if it is true, then evaluate the EXPS as sequence."
  `(multiple-value-bind ,vars ,e
     ,(gen-seq exps)))

(defmacro e/n (vars &body exps)
  "Ordered choice, each of EXPS is an implicit sequence if it is a list, and non-list EXP is as if it is a list of one item.
But since each expression may have more values other than the first T or NIL, VARS specifies the variables for these values.
If VARS is integer, then it is the number of GENSYMs to use (including the primary T or NIL);
Otherwise VARS should be a list of symbols, for use in MULTIPLE-VALUE-BIND.
This is intended to be used in macrolet to define E/ which has VARS already defined suitably.
Return the value(s) of the first expression that gives T, analogous to OR."
  (let ((vs (if (integerp vars)
                (loop :for i :below vars :collect (gensym))
                vars)))
    (labels ((gen-choices (exs)
               (if (null (cdr exs))
                   (gen-seq (car exs) '%cur)
                   `(multiple-value-bind ,vs ,(gen-seq (car exs) '%cur)
                      (declare (ignorable ,@vs))
                      (if ,(car vs)
                          (values ,@vs)
                          ,(gen-choices (cdr exs)))))))
      (if (null exps)
          nil
          `(let ((%cur (peek-next)))
             (declare (ignorable %cur))
             ,(gen-choices exps))))))

(defmacro e/1 (&body exps)
  `(e/n 1 ,@exps))

#+nil
(e/n (u v)
  (#\a abc (ok 'a))
  (#\b (ok 'b))
  (#\c #\d))

(defmacro e* (e)
  ;; TODO
  nil)

(defmacro e+ (e)
  ;; TODO
  nil)

(defmacro e? (e)
  ;; TODO
  nil)

(defmacro e& ()
  ;; TODO
  nil)

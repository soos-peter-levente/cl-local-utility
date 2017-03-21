(defpackage :local-utility-function-strings
  (:use :common-lisp
        :local-utility-function-general)
  (:nicknames :lus)
  (:export :split))

(in-package :local-utility-function-strings)

(defun split (string &optional char)
  (let ((separator (or char #\Space)))
    (loop for start = 0 then (1+ finish)
       for finish = (position separator string :start start)
       collecting (subseq string start finish)
       until (null finish))))

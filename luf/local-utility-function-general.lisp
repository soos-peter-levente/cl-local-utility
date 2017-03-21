(defpackage :local-utility-function-general
  (:use :common-lisp :local-utility)
  (:nicknames :luf)
  (:export :flatten :shuffle :strip :group
           :array-slice 
           :read-file))

(in-package :local-utility-function-general)

;; LISTS
(defun flatten (l) 
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defun group (lst &optional size)
  "One way of doing it."
  (let* ((subsize (if size size  (length lst)))
         (sub-end (if (<= (length lst) subsize) (length lst) subsize)))
    (cond ((null lst) nil)
          (t (cons (subseq lst 0 sub-end)
                   (group (subseq lst sub-end) subsize))))))

(defun strip (list)
  (cond ((null (car list)) '())
        ((listp (car list)) (strip (car list)))
        (t list)))

(defun shuffle (seq)
  (loop for i from (length seq) downto 2
     do (rotatef (elt seq (random i))
                 (elt seq (1- i)))
     :finally (return seq)))

;; ARRAYS
(defun array-slice (arr row)
  (make-array (array-dimension arr 1) 
              :displaced-to arr 
              :displaced-index-offset (* row (array-dimension arr 1))))

;; FILES
(defun read-file (input)
  (with-open-file (file input)
    (loop for line = (read-line file nil)
       while line collect line)))

(defpackage :local-utility-macro
  (:use :common-lisp :local-utility
        :local-utility-function-general)
  (:nicknames :lum)
  (:export :for-indices :defmem :with-profiles))

(in-package :local-utility-macro)

(defmacro for-indices (array bindings &rest body)
  "Gensyms for bindings!"
  (let* ((inputa (eval array))
         (limits (mapcar #'1- (array-dimensions inputa))))
    (macrolet ((make-do-loop (bindings limits &rest body)
                 (cond ((null bindings) (luf:strip body))
                       (t (first (list `(do ((,(car bindings) 0 (1+ ,(car bindings))))
                                            ((> ,(car bindings) ,(car limits)) t)
                                          (make-do-loop ,(cdr bindings) ,(cdr limits) ,body))))))))
      `(make-do-loop ,bindings ,limits ,@body))))

(defmacro defmem (name args &body body)
  (let ((cache (gensym "MEMO-")))
    `(let ((,cache (make-hash-table :test 'equal)))
       (defun ,name ,args
         (or (gethash (list ,@args) ,cache)
             (setf (gethash (list ,@args) ,cache)
                   ,@body))))))

(defmacro with-profiles (also-profile main-function-call &key (return-value t))
  #+sbcl
  (let ((previous-list (sb-profile:profile)))
    `(progn (format t "Profiling, please wait...")
            (sb-profile:unprofile)
            (sb-profile:profile ,(first main-function-call) ,@also-profile)
            (let ((result (time (progn ,main-function-call))))
              (progn
                (sb-profile:report)
                (sb-profile:reset)
                (sb-profile:profile
                 ,@(set-difference previous-list (sb-profile:profile)))
                (when ,return-value result))))))


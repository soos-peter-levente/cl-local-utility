(in-package :utility)

;; MACROS
(defmacro for-indices (array bindings &rest body)
  "Gensyms for bindings!"
  (let* ((inputa (eval array))
         (limits (mapcar #'1- (array-dimensions inputa))))
    (macrolet ((make-do-loop (bindings limits &rest body)
                 (cond ((null bindings) (util:strip body))
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

;; LISTS
(defun flatten (l) 
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defun group (list &optional (n 2))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if rest
                   (rec rest (cons (subseq list 0 n) acc))
                   (nreverse (cons list acc))))))
    (rec list nil)))

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
(defun read-file-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line collect line)))

(defun read-file-contents (path)
  (with-open-file (in path)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

;; STRINGS
(defun split (string &optional char)
  (let ((separator (or char #\Space)))
    (loop for start = 0 then (1+ finish)
          for finish = (position separator string :start start)
          collecting (subseq string start finish)
          until (null finish))))

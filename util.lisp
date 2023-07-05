(defun with-silenced-output (func)
  (with-output-to-string
      (*standard-output*
       (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
    (eval func)))

(defun print-colored-text (text c)
  (format t "~a~a~a" 
          (format nil "~a[3~a;1m" #\esc c)
          text
          (format nil "~a[0m" #\esc))
  (terpri))
;; (print-colored-text "This is red text!" 1)
;; (print-colored-text "This is green text!" 2)
;; (print-colored-text "This is blue text!" 4)

(defun undef (symbol)
  (makunbound symbol))

(defun println (s)
  (format t s))

(defun contains (list val)
  (not (not (member val list :test #'equal))))

(defun pair-list (lst)
  (if (null lst)
      nil
      (cons (list (car lst) (cadr lst))
            (pair-list (cddr lst)))))

(defun flatten (list)
  (cond ((null list) nil)
        ((atom list) (list list))
        (t (loop for a in list appending (flatten a)))))

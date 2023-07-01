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

(defun undef (symbol)
  (makunbound symbol))

(defun println (s)
  (format t s))

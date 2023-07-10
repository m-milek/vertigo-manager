(defmacro winsize-slot (ptr name)
  `(cffi:foreign-slot-value ,ptr '(:struct osicat-posix:winsize) ',name))

(defun size ()
  "Return two values: the column and line count."
  (cffi:with-foreign-object (window-size '(:struct osicat-posix:winsize))
    (osicat-posix:ioctl 0 osicat-posix:tiocgwinsz window-size)
    (list (winsize-slot window-size osicat-posix:col)
            (winsize-slot window-size osicat-posix:row))))

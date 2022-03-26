(defun find-definitions-find-symbol-or-package (name)
    (when (and (eql (search "(setf " name :test #'char-equal) 0)
               (char= (char name (1- (length name))) #\)))
          nil))

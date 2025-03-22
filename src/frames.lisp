(defpackage :alive/frames
    (:use :cl)
    (:export :list-step-frames
             :list-debug-frames))

(in-package :alive/frames)


(defun get-fun-name (frame)
    (let* ((dbg-fun (sb-di:frame-debug-fun frame))
           (name (when dbg-fun
                       (sb-di:debug-fun-name dbg-fun))))
        (princ-to-string (or name ""))))


(defun has-debug-block (loc)
    (handler-case
            (progn (sb-di:code-location-debug-block loc)
                   T)
        (sb-di:no-debug-blocks () nil)))


(defun get-top-form-offset (code-loc)
    (handler-case
            (sb-di:code-location-toplevel-form-offset code-loc)
        (T (c)
           (declare (ignore c))
           nil)))


(defun get-form-number (code-loc)
    (handler-case
            (sb-di:code-location-form-number code-loc)
        (T (c)
           (declare (ignore c))
           nil)))


(defun var-is-valid (var loc)
    (ecase (sb-di:debug-var-validity var loc)
        (:valid T)
        ((:invalid :unknown) nil)))


(defun get-frame-vars (frame code-loc)
    (loop :with dbg-vars := (or (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)) (make-array 0))
          :with vars := nil

          :for var :across dbg-vars
          :do (push (cons (sb-di::debug-var-symbol var)
                          (if (var-is-valid var code-loc)
                              (sb-di:debug-var-value var frame)
                              nil))
                    vars)

          :finally (return vars)))


(defun create-frame-obj (frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (code-loc (sb-di:frame-code-location frame))
           (has-dbg (has-debug-block code-loc))
           (dbg-src (when has-dbg
                          (sb-di:code-location-debug-source code-loc)))
           (src-name (when has-dbg
                           (sb-di:debug-source-namestring dbg-src)))
           (top-form (get-top-form-offset code-loc))
           (form-num (get-form-number code-loc))
           #+n (vars (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))
           (vars (get-frame-vars frame code-loc)))

        #+n (when vars
                  (loop :for var :across vars
                        :do (format T "***** VAR ~A ~A~%"
                                (sb-di::debug-var-symbol var)
                                (if (var-is-valid var code-loc)
                                    (sb-di:debug-var-value var frame)
                                    ""))))
        #+n (format T "***** VARS ~A ~A~%"
                (sb-di:frame-debug-fun frame)
                (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))
        (setf (gethash "function" obj) (get-fun-name frame))

        (when src-name
              (setf (gethash "file" obj) src-name)
              (setf (gethash "topForm" obj) top-form)
              (setf (gethash "formNumber" obj) form-num)
              (setf (gethash "vars" obj) vars))

        obj))


(defun list-frames (top &optional limit)
    (loop :with frame := top
          :with ndx := 0

          :while (and frame
                      (or (not limit)
                          (< ndx limit)))

          :collect (create-frame-obj frame)
          :do (setf frame (sb-di:frame-down frame))
              (incf ndx)))


(defun list-step-frames ()
    (list-frames (sb-di::find-stepped-frame)))


(defun list-debug-frames (&optional (limit nil))
    (let ((top-frame (or (sb-di::find-interrupted-frame)
                         (sb-di::find-stepped-frame)
                         (sb-di::find-caller-frame))))
        (if limit
            (list-frames top-frame limit)
            (list-frames top-frame))))

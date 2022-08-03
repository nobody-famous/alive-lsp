(defpackage :alive/frames
    (:use :cl)
    (:export :list-step-frames
             :list-debug-frames
             :print-obj))

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


(defun create-frame-obj (frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (code-loc (sb-di:frame-code-location frame))
           (has-dbg (has-debug-block code-loc))
           (dbg-src (when has-dbg
                          (sb-di:code-location-debug-source code-loc)))
           (src-name (when has-dbg
                           (sb-di:debug-source-namestring dbg-src)))
           (top-form (when has-dbg
                           (sb-di:code-location-toplevel-form-offset code-loc)))
           (form-num (when has-dbg
                           (sb-di:code-location-form-number code-loc))))

        (setf (gethash "function" obj) (get-fun-name frame))

        (when src-name
              (setf (gethash "file" obj) src-name)
              (setf (gethash "topForm" obj) top-form)
              (setf (gethash "formNumber" obj) form-num))

        obj))


(defun print-obj (frame)
    (when frame
          (loop :for value :being :the :hash-value :of frame
                :using (hash-key key)
                :do (format T "~A ~A~%" key value))))


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


(defun list-debug-frames ()
    (let ((top-frame (or (sb-di::find-interrupted-frame)
                         (sb-di::find-stepped-frame)
                         (sb-di::find-caller-frame))))
        (list-frames top-frame)))

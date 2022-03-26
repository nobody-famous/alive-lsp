
(defmacro with-interrupts-enabled% (flag body)
  `(progn
     ,@(if flag '((check-slime-interrupts)))
     (multiple-value-prog1
         (let ((*slime-interrupts-enabled* ,flag))
           ,@body)
       ,@(if flag '((check-slime-interrupts))))))


(defun queue-thread-interrupt (thread function)
  (interrupt-thread thread
                    (lambda ()
                      ;; safely interrupt THREAD
                      (when (invoke-or-queue-interrupt function)
                        (wake-thread thread)))))

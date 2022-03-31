(defimplementation utf8-to-string (octets)
  (sb-ext:octets-to-string octets :external-format '(:utf8 :replacement
                                                     #+sb-unicode #\Replacement_Character
                                                     #-sb-unicode #\? )))

(defimplementation preferred-communication-style ()
  (cond
    ((member :sb-thread *features*) :spawn)
    ((member :win32 *features*) nil)
    (t :fd-handler)))

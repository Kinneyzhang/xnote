(defun xnote-seconds-to-date (seconds)
  "Convert seconds to a date."
  (format-time-string "%Y-%m-%d" seconds))

(defun xnote-date-to-seconds (date)
  "Convert date to seconds."
  (time-convert (date-to-time (concat date " 00:00:00")) 'integer))

(defun xnote-current-seconds ()
  "Return seconds of current time."
  (time-convert (current-time) 'integer))

(defun xnote-current-date ()
  "Return date of current time."
  (format-time-string "%Y-%m-%d" (current-time)))

(defun xnote-wrap-plist (data &rest keys)
  ;; (("a" "1") ("b" "2") ("c" "3")) :time :id
  ;; ((:time "a" :id "1") (:time "b" :id "2") (:time "c" :id "3"))
  (mapcar (lambda (lst)
            (let ((len (length (car data)))
                  plst)
              (dotimes (i len)
                (setq plst (plist-put plst (nth i keys) (nth i lst))))
              plst))
          data))

(provide 'xnote-util)

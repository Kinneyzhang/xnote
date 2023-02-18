(defun xnote-seconds-to-date (seconds)
  "Convert seconds to a date."
  (format-time-string "%Y-%m-%d" seconds))
;; (xnote-seconds-to-date 1676649620)

(defun xnote-date-to-seconds (date)
  "Convert date to seconds."
  (time-convert (date-to-time (concat date " 00:00:00")) 'integer))
;; (xnote-date-to-seconds "2023-02-18")

(provide 'xnote-util)

(defconst zt--id-regexp (rx (= 8 digit) "T" (= 4 digit)))

(defconst zt--keywords `((,zt--id-regexp 0 'link t)))

(defun zt--enable-minor-mode ()
  (font-lock-add-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(defun zt--disable-minor-mode ()
  (font-lock-remove-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(define-minor-mode zt-minor-mode
  ""
  :lighter " zt"
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))

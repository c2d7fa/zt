(defconst zt--id-regexp (rx (= 8 digit)
                            (? "T")
                            (= 6 digit)))

(defconst zt--keywords `((,zt--id-regexp 0 'link t)))

(defun zt--generate-id ()
  (format-time-string "%Y%m%dT%H%M%S"))

(defun zt--enable-minor-mode ()
  (font-lock-add-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(defun zt--disable-minor-mode ()
  (font-lock-remove-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(defun zt-open-at-point ()
  (interactive)
  (message "open at point"))

(defun zt-insert-new-id ()
  (interactive)
  (insert (zt--generate-id)))

(defconst zt--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'zt-open-at-point)
    (define-key map (kbd "C-c C-t") 'zt-insert-new-id)
    map))

(define-minor-mode zt-minor-mode "zt"
  :lighter " zt"
  :keymap zt--keymap
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))


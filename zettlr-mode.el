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

(defun zt--id-at-point ()
  (let ((word (word-at-point t)))
    (when (string-match zt--id-regexp word)
      word)))

(defun zt-insert-new-id ()
  (interactive)
  (insert (zt--generate-id)))

(defun zt--search-id (id)
  "If a note with the given ID exists in the current directory,
   return its full path; if no file exists, or there are multiple
   matching files, return nil."
  (let* ((all-files (directory-files default-directory))
         (file-matches (lambda (file)
                         (s-prefix? id file)))
         (matching-files (seq-filter file-matches all-files)))
    (when (= (length matching-files) 1)
      (car matching-files))))

(defun zt--new-filename-id (id)
  (concat id ".txt"))

(defun zt-open-id (id)
  (find-file (or (zt--search-id id)
                 (zt--new-filename-id id))))

(defun zt-open-at-point ()
  (interactive)
  (if-let ((id (zt--id-at-point)))
      (zt-open-id id)
    (message "zt: no link at point")))

(defconst zt--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'zt-open-at-point)
    (define-key map (kbd "C-c C-t") 'zt-insert-new-id)
    map))

(define-minor-mode zt-minor-mode "zt"
  :lighter " zt"
  :keymap zt--keymap
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))


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

(defun zt--is-id (string)
  (string-match zt--id-regexp string))

(defun zt--id-at-point ()
  (let ((word (word-at-point t)))
    (when (zt--is-id word) word)))

(defun zt--formatted-link-at-point ()
  (let ((line (thing-at-point 'line t)))
    (string-match (rx
                   (group (= 8 digit)
                          (? "T")
                          (= 6 digit)
                          (opt " " (+? any) line-end)))
                  line)
    (match-string 1 line)))

(defun zt-insert-new-id ()
  (interactive)
  (insert (zt--generate-id)))

(defun zt--find-id (string)
  (string-match zt--id-regexp string)
  (match-string 0 string))

(defun zt--find-title (formatted-link)
  (string-match (rx (+ (or digit "T"))
                    " "
                    (group (+? any))
                    string-end)
                formatted-link)
  (match-string 1 formatted-link))

(defun zt--all-existing-ids-default-directory ()
  (let* ((all-files (seq-filter 'zt--is-id (directory-files default-directory nil "^[[:digit:]]")))
         (all-ids (mapcar 'zt--find-id all-files)))
    all-ids))

(defun zt--search-id (id)
  "If a note with the given ID exists in the current directory,
   return its full path; if no file exists, or there are multiple
   matching files, return nil."
  (let* ((all-files (directory-files default-directory))
         (has-id (lambda (file) (s-prefix? id file)))
         (id-files (seq-filter has-id all-files)))
    (when (= (length id-files) 1)
      (car id-files))))

(defun zt--file-title (filename)
  "Given the path to a file, return it's 'title' as it would be
   inserted as a new link."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun zt--format-link-id (id)
  (if-let ((file (zt--search-id id)))
      (concat id " " (zt--file-title file))
    id))

(defun zt--available-formatted-links ()
  (mapcar 'zt--format-link-id (zt--all-existing-ids-default-directory)))

(defun zt--completing-read (prompt)
  (completing-read prompt (zt--available-formatted-links)))

(defun zt--new-filename-id (id)
  (concat id ".txt"))

(defun zt-open (link)
  "Given a link to a file, optionally also followed by a title, go
   to the associated file if it exists, and otherwise create the
   file with the optionally given title.

  Finds or create the associated file:
    (zt-open \"20220912T201109\")

  Find or create the associated file; if and only if it does not
  already exist, insert the text \"New file\" on the first line
  of the created file:
    (zt-open \"20220912T201109 New file\")"
  (let* ((id (zt--find-id link))
         (title (zt--find-title link))
         (file (zt--search-id id)))
    (find-file (or file (zt--new-filename-id id)))
    (when (and (not file) title)
      (insert title))
    (zt-minor-mode 1)))

(defun zt-open-at-point ()
  (interactive)
  (if-let ((id (or (zt--id-at-point)
                   (zt--formatted-link-at-point))))
      (zt-open id)
    (message "zt: no link at point")))

(defun zt-find-link ()
  (interactive)
  (insert (zt--completing-read "Insert link: ")))

(defun zt-find-file ()
  (interactive)
  (zt-open (zt--completing-read "Find file: ")))

(defun zt-insert-index ()
  "Insert at point a list of links to each file in the current
   directory, including their titles."
  (interactive)
  (mapc (lambda (link) (insert link "\n"))
        (zt--available-formatted-links)))

(defconst zt--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'zt-open-at-point)
    (define-key map (kbd "C-c C-t") 'zt-insert-new-id)
    (define-key map (kbd "C-c C-l") 'zt-find-link)
    (define-key map (kbd "C-c C-f") 'zt-find-file)
    map))

(define-minor-mode zt-minor-mode "zt"
  :lighter " zt"
  :keymap zt--keymap
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))


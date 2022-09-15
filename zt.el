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
return its full path. If no file exists, or there are multiple
matching files, return `nil'."
  (let* ((all-files (directory-files default-directory))
         (has-id (lambda (file) (s-prefix? id file)))
         (id-files (seq-filter has-id all-files)))
    (when (= (length id-files) 1)
      (car id-files))))

(defun zt--file-title (file)
  "Given the path to a FILE, return it's title as it would be
inserted as a new link."
  (with-temp-buffer
    (when (and (file-exists-p file)
               (not (file-directory-p file)))
      (insert-file-contents file)
      (buffer-substring (line-beginning-position) (line-end-position)))))

(defun zt--format-link-id (id)
  (if-let ((file (zt--search-id id)))
      (concat id " " (zt--file-title file))
    id))

(defun zt--file-has-link (file id)
  (with-temp-buffer
    (insert-file-contents file)
    (re-search-forward id nil t)))

(defun zt--all-linking-ids (id)
  (seq-filter (lambda (other-id)
                (zt--file-has-link (zt--search-id other-id) id))
              (zt--all-existing-ids-default-directory)))

(defun zt--available-formatted-links ()
  (s-split "\n" (s-trim (shell-command-to-string "ztf ."))))

(defun zt--available-linking-files (id)
  (s-split "\n" (s-trim (shell-command-to-string (concat "ztf . " (shell-quote-argument id))))))

(defun zt--completing-read (prompt)
  (completing-read prompt (zt--available-formatted-links)))

(defun zt--completing-read-linking-files (prompt id)
  (completing-read prompt (zt--available-linking-files id)))

(defun zt--new-filename-id (id)
  (concat id ".txt"))

(defun zt--current-id ()
  (zt--find-id (buffer-name)))

(defun zt-open (link)
  "Given a link to a file, optionally also followed by a title, go
to the associated file if it exists, and otherwise create the
file with the optionally given title.

Finds or create the associated file:

    (zt-open \"20220912T201109\")

Find or create the associated file. If and only if it does not
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
  "Open the link at point, according to the same rules as `zt-open'."
  (interactive)
  (if-let ((id (or (zt--id-at-point)
                   (zt--formatted-link-at-point))))
      (zt-open id)
    (message "zt: no link at point")))

(defun zt-insert-link ()
  (interactive)
  (insert (zt--completing-read "Insert link: ")))

(defun zt-find-file ()
  (interactive)
  (zt-open (zt--completing-read "Find file: ")))

(defun zt-find-linking-file ()
  "Interactively prompt for a file from among those that link to
the current file and open it."
  (interactive)
  (zt-open (zt--completing-read-linking-files "Find file: " (zt--current-id))))

(defun zt-insert-linking-file ()
  "Interactively prompt for a file from among those that link to
the current file. Insert a link to the selected file."
  (interactive)
  (insert (zt--completing-read-linking-files "Insert link: " (zt--current-id))))

(defun zt-insert-linking-files ()
  "Insert an index of all files that link to the current file. To
insert only a single link, use `zt-insert-linking-file'. To
insert an index of all files in the system, use
`zt-insert-index'."
  (interactive)
  (mapc (lambda (link) (insert link "\n"))
        (zt--available-linking-files (zt--current-id))))

(defun zt-insert-index ()
  "Insert at point a list of links to each file in the current
directory, including their titles. To include only files that
link to the current file, use `zt-insert-linking-files' instead."
  (interactive)
  (mapc (lambda (link) (insert link "\n"))
        (zt--available-formatted-links)))

(defconst zt--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") 'zt-open-at-point)
    (define-key map (kbd "C-c C-t") 'zt-insert-new-id)
    (define-key map (kbd "C-c C-l") 'zt-insert-link)
    (define-key map (kbd "C-c M-l") 'zt-insert-linking-file)
    (define-key map (kbd "C-c C-f") 'zt-find-file)
    (define-key map (kbd "C-c M-f") 'zt-find-linking-file)
    map))

(define-minor-mode zt-minor-mode "zt"
  :lighter " zt"
  :keymap zt--keymap
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))


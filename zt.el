(require 's)

(defcustom zt-ztf-executable-path "~/.local/share/zt/ztf"
  "The path to the \"ztf\" executable. See the zt package's README
for details on what this is and how to build it."
  :type 'file
  :group 'zt)

(defconst zt--required-ztf-version "0")

(defconst zt--id-regexp (rx (= 8 digit)
                            (? "T")
                            (= 6 digit)))

(defun zt--link-help-echo (window object pos)
  (save-excursion
    (goto-char pos)
    (zt--format-link-id (zt--id-at-point))))

(defconst zt--link-face
  (let ((link-keymap (make-sparse-keymap)))
    (define-key link-keymap (kbd "RET") 'zt--link-enter-key-pressed)
    (define-key link-keymap (kbd "<mouse-1>") 'zt-open-at-point)
    `(face link
           help-echo zt--link-help-echo
           keymap ,link-keymap
           mouse-face highlight)))

(defconst zt--keywords `((,zt--id-regexp 0 ',zt--link-face)))

(defun zt--generate-id ()
  (format-time-string "%Y%m%dT%H%M%S"))

(defun zt--insert-link (link &optional omit-title)
  (insert
   (if (eq major-mode 'org-mode) "zt:" "")
   (if omit-title (zt--find-id link) link)))

(defun zt--update-buffer-name (&rest args)
  (interactive)
  (when zt-minor-mode
    (rename-buffer (zt--format-link-id (zt--current-id)) t)))

(defun zt--enable-minor-mode ()
  (zt--update-buffer-name)
  (font-lock-add-keywords nil zt--keywords)
  (setq-local font-lock-extra-managed-props '(help-echo keymap mouse-face))
  (font-lock-fontify-buffer)
  (advice-add 'save-buffer :after 'zt--update-buffer-name))

(defun zt--disable-minor-mode ()
  (font-lock-remove-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(defun zt--is-id (string)
  (string-match zt--id-regexp string))

(defun zt--id-at-point ()
  (if-let ((word (word-at-point t)))
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
  (zt--insert-link (zt--generate-id)))

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

(defun zt--ztf-command (args)
  (s-join " " (cons zt-ztf-executable-path
                    (mapcar 'shell-quote-argument args))))

(defun zt--ztf-needs-update ()
  (or (not (file-exists-p zt-ztf-executable-path))
      (not (equal zt--required-ztf-version
                  (s-trim (shell-command-to-string (zt--ztf-command '("--version"))))))))

(defun zt--interactively-update-ztf ()
  (if (y-or-n-p "zt: Required executable 'ztf' is not installed or not latest version. Automatically download prebuilt executable for x86-64 Linux?")
      (progn (error "zt: Automatically updating ztf currently isn't supported! See README for instructions on manually installing."))
    (error "zt: You must manually install latest 'ztf' executable into 'zt-ztf-executable-path'. See README for more information.")))

(defun zt--call-ztf (&rest args)
  (when (zt--ztf-needs-update)
    (zt--interactively-update-ztf))
  (s-trim (shell-command-to-string (zt--ztf-command args))))

(defun zt--file-title (file)
  "Given the path to a FILE, return it's title as it would be
inserted as a new link."
  (zt--call-ztf file))

(defun zt--format-link-id (id)
  (if-let ((file (zt--search-id id)))
      (concat id " " (zt--file-title file))
    id))

(defun zt--available-formatted-links ()
  (s-split "\n" (zt--call-ztf ".")))

(defun zt--available-linking-files (id)
  (s-split "\n" (zt--call-ztf "." id)))

(defun zt--completing-read (prompt)
  (let ((ivy-mode-enabled (and (boundp ivy-mode) ivy-mode)))
    (if ivy-mode-enabled
        (ivy-read prompt (zt--available-formatted-links) :preselect (zt--current-id))
      (completing-read prompt (zt--available-formatted-links)))))

(defun zt--completing-read-linking-files (prompt id)
  (completing-read prompt (zt--available-linking-files id)))

(defun zt--new-filename-id (id)
  (concat id ".txt"))

(defun zt--current-id ()
  (let ((id (zt--find-id (buffer-name))))
    (when (zt--is-id id) id)))

(defun zt-change-file-extension (extension)
  (interactive "MFile extension: ")
  (save-buffer)
  (let* ((old-file-name (buffer-file-name))
         (new-file-name (concat (file-name-directory old-file-name)
                                (file-name-base old-file-name)
                                "." extension)))
    (write-file new-file-name)
    (delete-file old-file-name)
    (zt-minor-mode 1)))

(defun zt-kill-current-id ()
  "Add the ID of the current file to the kill ring, so that it
can be yanked again with `yank', or, depending on your Emacs
configuration, is copied to the clipboard."
  (interactive)
  (let ((id (zt--current-id)))
    (kill-new id)
    (message (format "Added current ID '%s' to kill ring." id))))

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

(defun zt-open-at-point-and-insert-linking-files ()
  "Open the link at point, according to the same rules at
`zt-open'. In the newly created file, also insert a list of all
linking files (see also `zt-insert-linking-files')."
  (interactive)
  (if-let ((id (or (zt--id-at-point)
                   (zt--formatted-link-at-point))))
      (progn
        (save-buffer)
        (zt-open id)
        (insert "\n\n")
        (zt-insert-linking-files))
    (message "zt: no link at point")))

(defun zt-open-at-point (&optional prefix)
  "Open the link at point, according to the same rules as
`zt-open'. With a prefix argument, instead do the same as
`zt-create-at-point-and-insert-linking-files'."
  (interactive "P")
  (if prefix
      (zt-open-at-point-and-insert-linking-files)
    (if-let ((id (or (zt--id-at-point)
                     (zt--formatted-link-at-point))))
        (progn
          (zt-open id)
          (when prefix
            (insert "\n\n")
            (zt-insert-linking-files)))
      (message "zt: no link at point"))))

(defun zt--link-enter-key-pressed ()
  (interactive)
  (let ((is-actually-on-link (save-excursion
                               (when (not (= (point) (point-max)))
                                 (forward-char)
                                 (zt--id-at-point)))))
    (if is-actually-on-link (zt-open-at-point) (newline))))

(defun zt-insert-link (&optional prefix)
  "Interactively select a note, and insert a link to the note at
point. By default, also insert the linked file's title (if it
exists). With prefix argument, insert just the link."
  (interactive "P")
  (let ((formatted-link (zt--completing-read "Insert link: ")))
    (zt--insert-link formatted-link prefix)))

(defun zt-find-file ()
  (interactive)
  (zt-open (zt--completing-read "Find file: ")))

(defun zt-find-linking-file ()
  "Interactively prompt for a file from among those that link to
the current file and open it."
  (interactive)
  (zt-open (zt--completing-read-linking-files "Find file: " (zt--current-id))))

(defun zt-insert-linking-file (&optional prefix)
  "Interactively prompt for a file from among those that link to
the current file. Insert a link to the selected file. Prefix
argument has same effect as for `zt-insert-link'."
  (interactive "P")
  (let ((formatted-link (zt--completing-read-linking-files "Insert link: " (zt--current-id))))
    (zt--insert-link formatted-link prefix)))

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

(defconst zt-minor-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'zt-open-at-point)
    (define-key map (kbd "t") 'zt-insert-new-id)
    (define-key map (kbd "l") 'zt-insert-link)
    (define-key map (kbd "L") 'zt-insert-linking-file)
    (define-key map (kbd "M-l") 'zt-insert-linking-files)
    (define-key map (kbd "f") 'zt-find-file)
    (define-key map (kbd "F") 'zt-find-linking-file)
    (define-key map (kbd "M-w") 'zt-kill-current-id)
    (define-key map (kbd ".") 'zt-change-file-extension)
    map))

(defconst zt-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c #") zt-minor-mode-prefix-map)
    map))

(defun zt-enable-recommended-keymap ()
  (interactive)
  (define-key zt-minor-mode-map (kbd "C-z") zt-minor-mode-prefix-map) 
  (define-key zt-minor-mode-map (kbd "C-z C-l") 'zt-insert-link)
  (define-key zt-minor-mode-map (kbd "C-z C-S-l") 'zt-insert-linking-file)
  (define-key zt-minor-mode-map (kbd "C-z C-f") 'zt-find-file))

(define-minor-mode zt-minor-mode "zt"
  :lighter " zt"
  :keymap zt-minor-mode-map
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))

;; ---------------------------------------------------------
;; ORG MODE LINKS

(require 'org)
(require 'ol)

(defun zt--org-handler (link)
  (zt-open link))

(org-link-set-parameters
 "zt"
 :follow 'zt--org-handler
 :help-echo 'zt--link-help-echo)

(provide 'zt)

;;; zt.el --- Simple but opinionated Zettelkasten in Emacs  -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Jonas Hvid

;; Author: Jonas Hvid <mail@johv.dk>
;; URL: https://github.com/c2d7fa/zt
;; Created: 22 Sep 2022
;; Version: 1.0.1
;; Package-Requires: ((emacs "26.0") (s "1.13.1"))

;;; Code:

(require 's)
(require 'url)

(defcustom zt-ztf-executable-path (concat (getenv "HOME") "/.local/share/zt/ztf")
  "The path to the \"ztf\" executable. See the zt package's README
for details on what this is and how to build it."
  :type 'file
  :group 'zt)

(defcustom zt-default-file-extension ".txt"
  "The default file extension used when creating new files. You can
also change this after files have been created by calling
`zt-change-file-extension'."
  :type 'string
  :group 'zt)

(defcustom zt-fontify-plain-text-title t
  "When `zt-minor-mode' is enabled in a `text-mode' buffer, add
fontification for the first line."
  :type 'boolean
  :group 'zt)

(defface zt-plain-text-title '((t (:weight bold)))
  "The face used to highlight titles in plain text files when))))
`zt-fontify-plain-text-title' is enabled.")

(defconst zt--required-ztf-version "1")

(defun zt--system-configuration-has (s)
  (s-contains? s system-configuration))

(defun zt--platform ()
  (let* ((cpu (cond ((zt--system-configuration-has "x86_64") "x86_64")
                    ((zt--system-configuration-has "aarch64") "aarch64")
                    ((zt--system-configuration-has "arm") "aarch64")
                    (t nil)))
         (os (cond ((zt--system-configuration-has "linux") "linux")
                   ((zt--system-configuration-has "win") "windows")
                   ((zt--system-configuration-has "mingw") "windows")
                   ((zt--system-configuration-has "mac") "macos")
                   ((zt--system-configuration-has "darwin") "macos")
                   ((zt--system-configuration-has "apple") "macos")
                   (t nil))))
    (when (and cpu os)
      (concat cpu "-" os))))

(defun zt--download-url ()
  (let ((windows? (s-contains? "windows" (zt--platform))))
    (concat "https://johv.dk/public/ztf-1-" (zt--platform) (if windows? ".exe" ""))))

(defun zt--expected-sha256 ()
  (alist-get (zt--platform)
             '(("aarch64-linux" . "50d34326f6c9b286c56971231491a8babcbfc99e1eb7ac7e4723cff41549f9b9")
               ("aarch64-macos" . "a2f1969297594f3454fc013556dcfb606cde44444761e6184909ef8b88530e4c")
               ("aarch64-windows" . "10f9456412951e66ae6cf8fe51973e28745c07aabbcab0f579dea878a5f302b4")
               ("x86_64-linux" . "ee09b1e740d5c7db5437256ff145905dc6145ffeaaeb30cc6f9c018a1c953e74")
               ("x86_64-macos" . "acd91eaf9be8010119cf136cb86d4990933e171b1373bd05d8b7e216d43e74da")
               ("x86_64-windows" . "9fd756e1a0e45527178fd9aaabc932897a87ab991c991c6179378f60b5d66c98"))
   nil nil 'equal))

(defun zt--fontify-first-line (limit)
  (if (= (point) 1)
      (re-search-forward ".*?\n" limit t)
    nil))

(defconst zt--id-regexp (rx (= 8 digit)
                            (? "T")
                            (= 6 digit)))

(defvar zt--link-help-echo-cache (cons "" ""))
(defun zt--link-help-echo (window object pos)
  (save-excursion
    (goto-char pos)
    (let ((current-id (zt--id-at-point)))
      (if (equal (car zt--link-help-echo-cache) current-id)
          (cdr zt--link-help-echo-cache)
        (setq zt--link-help-echo-cache (cons current-id (zt--format-link-id current-id)))
        (cdr zt--link-help-echo-cache)))))

(defconst zt--link-face
  (let ((link-keymap (make-sparse-keymap)))
    (define-key link-keymap (kbd "RET") 'zt--link-enter-key-pressed)
    (define-key link-keymap (kbd "<mouse-1>") 'zt--link-mouse-pressed)
    `(face link
           help-echo zt--link-help-echo
           keymap ,link-keymap
           mouse-face highlight)))

(defconst zt--keywords `((,zt--id-regexp 0 ',zt--link-face t)))

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
  (when (and zt-fontify-plain-text-title (derived-mode-p 'text-mode))
    (font-lock-add-keywords nil '((zt--fontify-first-line 0 'zt-plain-text-title))))
  (setq-local font-lock-extra-managed-props '(help-echo keymap mouse-face))
  (font-lock-fontify-buffer)
  (advice-add 'save-buffer :after 'zt--update-buffer-name))

(defun zt--disable-minor-mode ()
  (font-lock-remove-keywords nil zt--keywords)
  (font-lock-fontify-buffer))

(defun zt--is-id (string)
  (string-match zt--id-regexp string))

(defun zt--id-at-point ()
  (if-let ((word (thing-at-point 'word t)))
      (when (zt--is-id word) word)))

(defun zt--formatted-link-at-point ()
  (let ((line (thing-at-point 'line t)))
    (string-match (rx (group (regexp zt--id-regexp)
                             (opt " " (+? any) line-end)))
                  line)
    (match-string 1 line)))

;;;###autoload
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

(defun zt--search-id (id)
  "If a note with the given ID exists in the current directory,
return its full path. If no file exists, or there are multiple
matching files, return `nil'."
  (let* ((id-files (directory-files default-directory nil (rx bos (literal id)))))
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
  (if (y-or-n-p (concat "zt: Required companion ztf v. " zt--required-ztf-version
                        " not available. Automatically download prebuilt executable for " (zt--platform) "?"))
      (progn
        (make-directory (file-name-directory zt-ztf-executable-path) t)
        (url-copy-file (zt--download-url) zt-ztf-executable-path t)
        (let ((ztf-executable-buffer (find-file-noselect zt-ztf-executable-path)))
          (when (not (equal (zt--expected-sha256) (secure-hash 'sha256 (find-file-noselect zt-ztf-executable-path))))
            (error "zt: sha256 hash didn't match! Refusing to continue."))
          (kill-buffer ztf-executable-buffer))
        (shell-command (concat "chmod +x " (shell-quote-argument zt-ztf-executable-path)))
        (when (zt--ztf-needs-update)
          (error "zt: Downloading executable didn't work. Make sure you're running the latest version of this package."))
        (message "zt: Sucessfully downloaded prebuilt executable."))
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
  (let ((ivy-mode-enabled (and (boundp 'ivy-mode) ivy-mode)))
    (if ivy-mode-enabled
        (ivy-read prompt (zt--available-formatted-links) :preselect (zt--current-id))
      (completing-read prompt (zt--available-formatted-links)))))

(defun zt--completing-read-linking-files (prompt id)
  (completing-read prompt (zt--available-linking-files id)))

(defun zt--new-filename-id (id)
  (concat id zt-default-file-extension))

(defun zt--current-id ()
  (let ((id (zt--find-id (buffer-name))))
    (when (zt--is-id id) id)))

;;;###autoload
(defun zt-change-file-extension (extension)
  "Change the file extension of the current file."
  (interactive "MFile extension: ")
  (save-buffer)
  (let* ((old-file-name (buffer-file-name))
         (new-file-name (concat (file-name-directory old-file-name)
                                (file-name-base old-file-name)
                                "." extension)))
    (write-file new-file-name)
    (delete-file old-file-name)
    (zt-minor-mode 1)))

;;;###autoload
(defun zt-kill-current-id ()
  "Add the ID of the current file to the kill ring, so that it
can be yanked again with `yank', or, depending on your Emacs
configuration, is copied to the clipboard."
  (interactive)
  (let ((id (zt--current-id)))
    (kill-new id)
    (message (format "Added current ID '%s' to kill ring." id))))

;;;###autoload
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

;;;###autoload
(defun zt-open-follower-at-point ()
  "Open the link at point or before the point, according to the
same rules at `zt-open'. In the newly created file, insert not
only the title but also a list of all linking files (see also
`zt-insert-linking-files').

The \"follower\" refers to a Folgezettel-style link where a
backlink from the newly created follower-note is also inserted."
  (interactive)
  (if-let ((id (or (zt--id-at-point)
                   (zt--formatted-link-at-point))))
      (progn
        (save-buffer)
        (zt-open id)
        (insert "\n\n")
        (zt-insert-linking-files))
    (message "zt: no link at point")))

;;;###autoload
(defun zt-open-at-point (&optional prefix)
  "Open the link at point, according to the same rules as
`zt-open'. With a prefix argument, instead do the same as
`zt-open-follower-at-point'."
  (interactive "P")
  (if prefix
      (zt-open-follower-at-point)
    (if-let ((id (or (zt--id-at-point)
                     (zt--formatted-link-at-point))))
        (progn
          (zt-open id)
          (when prefix
            (insert "\n\n")
            (zt-insert-linking-files)))
      (message "zt: no link at point"))))

(defun zt--is-point-really-on-link ()
  (save-excursion
    (when (not (= (point) (point-max)))
      (forward-char)
      (zt--id-at-point))))

(defun zt--link-mouse-pressed ()
  (interactive)
  (when (zt--is-point-really-on-link) (zt-open-at-point)))

(defun zt--link-enter-key-pressed ()
  (interactive)
  (if (zt--is-point-really-on-link) (zt-open-at-point) (newline)))

;;;###autoload
(defun zt-insert-link (&optional prefix)
  "Interactively select a note, and insert a link to the note at
point. By default, also insert the linked file's title (if it
exists). With prefix argument, insert just the link."
  (interactive "P")
  (let ((formatted-link (zt--completing-read "Insert link: ")))
    (zt--insert-link formatted-link prefix)))

;;;###autoload
(defun zt-find-file ()
  (interactive)
  (zt-open (zt--completing-read "Find file: ")))

;;;###autoload
(defun zt-find-linking-file ()
  "Interactively prompt for a file from among those that link to
the current file and open it."
  (interactive)
  (zt-open (zt--completing-read-linking-files "Find file: " (zt--current-id))))

;;;###autoload
(defun zt-insert-linking-file (&optional prefix)
  "Interactively prompt for a file from among those that link to
the current file. Insert a link to the selected file. Prefix
argument has same effect as for `zt-insert-link'."
  (interactive "P")
  (let ((formatted-link (zt--completing-read-linking-files "Insert link: " (zt--current-id))))
    (zt--insert-link formatted-link prefix)))

;;;###autoload
(defun zt-insert-linking-files ()
  "Insert an index of all files that link to the current file. To
insert only a single link, use `zt-insert-linking-file'. To
insert an index of all files in the system, use
`zt-insert-index'."
  (interactive)
  (mapc (lambda (link) (insert link "\n"))
        (zt--available-linking-files (zt--current-id))))

;;;###autoload
(defun zt-insert-index ()
  "Insert at point a list of links to each file in the current
directory, including their titles. To include only files that
link to the current file, use `zt-insert-linking-files' instead."
  (interactive)
  (mapc (lambda (link) (insert link "\n"))
        (zt--available-formatted-links)))

;;;###autoload
(defun zt-create-in-directory (directory)
  "Create a new note in the given directory."
  (interactive "DDirectory: ")
  (zt-open-in-directory (zt--generate-id) directory))

;;;###autoload
(defun zt-open-in-directory (link directory)
  "Open the whose ID corresponds to LINK in DIRECTORY. For example,
if you have an index note with ID \"20220922T193931\", this may
be a useful binding to define:

  (global-set-key (kbd \"C-c z\")
                  (lambda (interactive)
                    (zt-open-in-directory \"20220922T193931\" \"~/zt\")))"
  (let ((default-directory directory))
    (zt-open link)))

(defconst zt-minor-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'zt-open-at-point)
    (define-key map (kbd "O") 'zt-open-follower-at-point)
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

;;;###autoload
(define-minor-mode zt-minor-mode
  "Toggle zt minor mode.

Keymap:
\\{zt-minor-mode-map}"
  :lighter " zt"
  :keymap zt-minor-mode-map
  (if zt-minor-mode (zt--enable-minor-mode) (zt--disable-minor-mode)))

;; Org-mode links

(eval-after-load 'org
  (progn
    (require 'ol)

    (defun zt--org-handler (link)
      (zt-open link))

    (org-link-set-parameters
     "zt"
     :follow 'zt--org-handler
     :help-echo 'zt--link-help-echo)))

(provide 'zt)
;;; zt.el ends here

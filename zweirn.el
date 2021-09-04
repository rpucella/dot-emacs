
(require 'subr-x)

(define-derived-mode zweirn-mode
  special-mode "Zweirn"
  "Major mode for showing quick notes.")

(define-key zweirn-mode-map (kbd "RET") 'zweirn-read-note)
(define-key zweirn-mode-map (kbd "TAB") 'zweirn-move-next-note)
(define-key zweirn-mode-map (kbd "<backtab>") 'zweirn-move-prev-note)
(define-key zweirn-mode-map (kbd "n") 'zweirn-move-next-note)
(define-key zweirn-mode-map (kbd "p") 'zweirn-move-prev-note)
(define-key zweirn-mode-map (kbd "g") 'zweirn-reload)
(define-key zweirn-mode-map (kbd "q") 'zweirn-kill)
(define-key zweirn-mode-map (kbd "c") 'zweirn-create-note)
(define-key zweirn-mode-map (kbd "d") 'zweirn-delete-note)
(define-key zweirn-mode-map (kbd "D") 'zweirn-open-dired)
(define-key zweirn-mode-map (kbd "e") 'zweirn-export-note)
(define-key zweirn-mode-map (kbd "f") 'zweirn-show-name)
(define-key zweirn-mode-map (kbd "s") 'zweirn-search)
(define-key zweirn-mode-map (kbd "l") 'zweirn-linked-notes)

;; MODE PARAMETERS

(defvar zweirn-folder
  (concat (file-name-as-directory (getenv "HOME")) ".notes"))

(defvar zweirn-export-folder
  (concat (file-name-as-directory (getenv "HOME")) "Desktop"))


(defun zweirn--create-notes-folder-if-needed ()
  "Create notes folder if it doesn't exist."
  (unless (file-exists-p zweirn-folder)
    (make-directory zweirn-folder)))

(defun zweirn--untitled ()
  (format-time-string "%m/%d/%y %H:%M"))

(defun zweirn-create-note ()
  "Create a 'permanent' note in $HOME/.notes"
  (interactive)
  (zweirn--create-notes-folder-if-needed)
  (let* ((uuid (rp-generate-random-uuid))
         (new-file (concat (file-name-as-directory zweirn-folder) (concat uuid ".md")))
         (buff (get-file-buffer new-file)))
    ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
    ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (if (null buff)
        (progn (switch-to-buffer (find-file-noselect new-file))
               (insert "# Note ")
               (insert (zweirn--untitled))
               (newline)
               (newline))
      (switch-to-buffer buff))))

(defun zweirn--read-first-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
             unless (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))

(defun zweirn--read-first-non-empty-line (file)
  "Return first non-empty line of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((result nil))
      (while (and (not result) (not (eobp)))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (unless (string-empty-p line)
            (setq result line)))
        (forward-line 1))
      (if result
          result
        "<empty>"))))

(defun zweirn--current-name ()
  "Return the filename of the note on the current line."
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (note-name-regexp (rx string-start
                               "*[["
                               (group (zero-or-more (or (not (any "]"))
                                                        (seq "]" (not (any "]")))))
                                      (? "]")
                                      (or ".txt" ".md"))
                               "]]")))
    (save-match-data
      (and (string-match note-name-regexp line)
           (match-string 1 line)))))

(defun zweirn-show-name ()
  "Show the name of the file containing the number on the current line."
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (message (concat "Note file: " nt))
      (message "Cursor not over a note"))))

(defun zweirn-read-note ()
  "Load the note pointed to by the point in a *notes* buffer"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (switch-to-buffer
         (find-file-noselect (concat (file-name-as-directory zweirn-folder) nt)))
      (message "Cursor not over a note"))))

(defun zweirn--note-path (nt)
  (concat (file-name-as-directory zweirn-folder) nt))

(defun zweirn--export-path (n)
  (concat (file-name-as-directory zweirn-export-folder) n))

(defun zweirn-export-note ()
  "Export (copy) the note to export folder"
    (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let ((name (read-string "Note export name: ")))
          (copy-file (zweirn--note-path nt) (zweirn--export-path name)))
      (message "Cursor not over a note"))))

(defun zweirn--note-title (nt)
  (let ((line (zweirn--read-first-non-empty-line
               (concat (file-name-as-directory zweirn-folder) nt))))
    (zweirn--strip-header line)))

(defun zweirn-delete-note ()
  "Delete a note"
    (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (prompt (concat "Delete note? " title)))
          (when (yes-or-no-p prompt)
            (delete-file (zweirn--note-path nt))
            (zweirn--show)))
      (message "Cursor not over a note"))))

(defun zweirn-move-next-note ()
  "Find next note marker in the *notes* buffer"
  (interactive)
  ;; Move forward one (if you're on ^* already...).
  (right-char)
  (let ((result (re-search-forward "^*" nil t)))
    ;; Move back to * or back to original char if not found.
    (left-char)))

(defun zweirn-move-prev-note ()
  "Find previous note marker in the *notes* buffer"
  (interactive)
  (let ((result (re-search-forward "^*" nil t -1)))
    result))

(defun zweirn-open-dired ()
  "Open dired in the notes folder"
  (interactive)
  (dired zweirn-folder))

(defun zweirn-kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))

(defun zweirn--strip-header (s)
  (let ((header-regexp (rx string-start
                           (zero-or-more space)
                           (optional
                            (one-or-more "#")
                            (one-or-more space))
                           (group (zero-or-more not-newline))
                           string-end)))
    (and (string-match header-regexp s)
         (match-string 1 s))))

(defun zweirn--notes-by-update-time ()
  (let* ((filter (rx string-start
                     (zero-or-more (or
                                    (not (any "]"))
                                    (seq "]" (not (any "]")))))
                     (? "]")
                     (or ".txt" ".md")
                     string-end))   ;; any file *.txt|md without two ]] in the name
         (notes (directory-files-and-attributes (directory-file-name zweirn-folder) nil filter t))
         (notes (sort notes (lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
    (mapcar #'car notes)))

(defun zweirn-reload ()
  (interactive)
  (zweirn--show))

(defun zweirn (path)
  "Show list of notes in $HOME/.notes"
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Notes directory: ")
                       nil)))
  (let* ((folder (or path zweirn-folder))
         (name (concat "*Zweirn* " (file-name-as-directory folder)))
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zweirn-mode)
    (make-local-variable 'zweirn-folder)
    (setq zweirn-folder folder)
    (zweirn--create-notes-folder-if-needed)
    (zweirn--show)))
    
(defun zweirn--show ()
  (let* ((existing-notes (zweirn--notes-by-update-time))
         (notes existing-notes)
         (inhibit-read-only t))
    (erase-buffer)
    (insert "Notes directory " (file-name-as-directory zweirn-folder))
    (newline)
    (newline)
    (dolist (nt notes)
      (let* ((line (zweirn--read-first-non-empty-line
                    (concat (file-name-as-directory zweirn-folder) nt))))
        (insert "*" (propertize (concat "[[" nt "]]") 'invisible t) "  ")
        (insert (zweirn--strip-header line))
        (newline))))
  (beginning-of-buffer))

(defun zweirn-search (s)
  ;; TODO: We probably need a dedicated mode for this to navigate results.
  (interactive (list (read-string "Search string: ")))
  (let* ((notes (zweirn--notes-by-update-time))
         (name (concat "*Zweirn Search: " s "*"))
         (buff (get-buffer-create name))
         (seen nil)
         (num-notes 0))
    (switch-to-buffer buff)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (nt notes)
        (setq seen nil)
        (with-temp-buffer
          (insert-file-contents-literally (zweirn--note-path nt))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (when (string-match-p (regexp-quote s) line)
                (with-current-buffer buff
                  (when (not seen)
                    (when (> num-notes 0)
                      (newline))
                    (let ((title (zweirn--note-title nt)))
                      (insert title))
                    (newline)
                    (cl-incf num-notes)
                    (setq seen t))
                  (insert " " line)
                  (newline)))
              (forward-line 1))))))
    (beginning-of-buffer)))

(defun zweirn--find-all-links (nt)
  (let ((link-regexp (rx "[["
                         (group (zero-or-more (or (not (any "]"))
                                                        (seq "]" (not (any "]"))))))
                         "]]"))
        (links '()))
  (with-temp-buffer
    (insert-file-contents-literally (zweirn--note-path nt))
    (beginning-of-buffer)
    (while (progn
             (let ((found (re-search-forward link-regexp nil t)))
               (when found
                 (setq links (cons (match-string 1) links))
                 t))))
    links)))

(defun zweirn--find-link (nt link)
  (let ((link-regexp (regexp-quote (concat "[[" link "]]"))))
    (with-temp-buffer
      (insert-file-contents-literally (zweirn--note-path nt))
      (beginning-of-buffer)
      (if (re-search-forward link-regexp nil t) t nil))))

(defun zweirn--find-linking-notes (nt)
  (let* ((notes (zweirn--notes-by-update-time))
         (title (zweirn--note-title nt))
         (name (concat "*Zweirn Linked: " title "*"))
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (nt notes)
        (when (zweirn--find-link nt title)
          (let ((title (zweirn--note-title nt)))
            (insert title))
          (newline))))
    (beginning-of-buffer)))

(defun zweirn-linked-notes ()
  "Find all notes that link to this note"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (zweirn--find-linking-notes nt)
      (message "Cursor not over a note"))))
  

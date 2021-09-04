
(require 'subr-x)

;; TODO:
;;
;; Command to delete notes.
;; Order by time last time updated (and show it possible)
;; - maybe there's a way to expand to see the UUID and the data modified?
;;

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
(define-key zweirn-mode-map (kbd "d") 'zweirn-open-dired)
(define-key zweirn-mode-map (kbd "e") 'zweirn-export-note)
(define-key zweirn-mode-map (kbd "f") 'zweirn-show-name)

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
  "Export (copy) the note to another folder"
    (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let ((name (read-string "Note export name: ")))
          (copy-file (zweirn--note-path nt) (zweirn--export-path name)))
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
  (let ((buff (get-buffer-create "*Zweirn*")))
    (switch-to-buffer buff)
    (zweirn-mode)
    (make-local-variable 'zweirn-folder)
    (when path (setq zweirn-folder path))
    (rename-buffer (concat "*Zweirn " (file-name-as-directory zweirn-folder) "*"))
    (zweirn--create-notes-folder-if-needed)
    (zweirn--show)))
    
(defun zweirn--show ()
  (let* ((existing-notes (zweirn--notes-by-update-time))
         (notes existing-notes)
         (inhibit-read-only t))
    (erase-buffer)
    (insert (concat "Directory: " (file-name-as-directory zweirn-folder)))
    (newline)
    (newline)
    (dolist (nt notes)
      (let* ((line (zweirn--read-first-non-empty-line
                    (concat (file-name-as-directory zweirn-folder) nt))))
        (insert (concat "*" (propertize (concat "[[" nt "]]") 'invisible t) "  "))
        (insert (zweirn--strip-header line))
        (newline))))
  (beginning-of-buffer))


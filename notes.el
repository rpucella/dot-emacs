(require 'subr-x)

;; TODO:
;;
;; Command to delete notes.
;; Command to move notes.
;; Switch to UUIDs and make them invisible to allow for selection
;;   cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Invisible-Text.html
;; Order by time last time updated (and show it possible)
;;   - maybe there's a way to expand to see the UUID and the data modified?
;;

(define-derived-mode rp-notes-mode special-mode "RP Notes"
  "Major mode for showing quick notes.")

(define-key rp-notes-mode-map (kbd "RET") 'rp-notes-read-note)
(define-key rp-notes-mode-map (kbd "TAB") 'rp-notes-move-next-note)
(define-key rp-notes-mode-map (kbd "<backtab>") 'rp-notes-move-prev-note)
(define-key rp-notes-mode-map (kbd "n") 'rp-notes-move-next-note)
(define-key rp-notes-mode-map (kbd "p") 'rp-notes-move-prev-note)
(define-key rp-notes-mode-map (kbd "g") 'rp-notes)
(define-key rp-notes-mode-map (kbd "q") 'rp-notes-kill)
(define-key rp-notes-mode-map (kbd "c") 'rp-new-note)
(define-key rp-notes-mode-map (kbd "d") 'rp-notes-open-dired)
(define-key rp-notes-mode-map (kbd "e") 'rp-notes-export-note)
(define-key rp-notes-mode-map (kbd "f") 'rp-notes-show-name)

(defvar rp-notes-folder (concat (file-name-as-directory (getenv "HOME")) ".notes"))

(defun rp-notes--create-notes-folder-if-needed ()
  "Create notes folder if it doesn't exist."
  (unless (file-exists-p rp-notes-folder)
    (make-directory rp-notes-folder)))

(defun rp-notes--untitled ()
  (format-time-string "%m/%d/%y %H:%M"))

(defun rp-new-note ()
  "Create a 'permanent' note in $HOME/.notes"
  (interactive)
  (rp-notes--create-notes-folder-if-needed)
  (let* ((uuid (rp-generate-random-uuid))
         (new-file (concat (file-name-as-directory rp-notes-folder) (concat uuid ".md")))
         (buff (get-file-buffer new-file)))
    ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
    ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (if (null buff)
        (progn (switch-to-buffer (find-file-noselect new-file))
               (insert "# Note ")
               (insert (rp-notes--untitled))
               (newline)
               (newline))
      (switch-to-buffer buff))))

(defun rp-notes--read-first-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
             unless (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))

(defun rp-notes--read-first-non-empty-line (file)
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

(defun rp-notes--current-name ()
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

(defun rp-notes-show-name ()
  "Show the name of the file containing the number on the current line."
  (interactive)
  (let ((nt (rp-notes--current-name)))
    (if nt
        (message (concat "Note file: " nt))
      (message "Cursor not over a note"))))

(defun rp-notes-read-note ()
  "Load the note pointed to by the point in a *notes* buffer"
  (interactive)
  (let ((nt (rp-notes--current-name)))
    (if nt
        (switch-to-buffer
         (find-file-noselect (concat (file-name-as-directory rp-notes-folder) nt)))
      (message "Cursor not over a note"))))

(defun rp-notes-export-note ()
  "Export (copy) the note to another folder"
    (interactive)
  (let ((nt (rp-notes--current-name)))
    (if nt
        (let ((name (read-file-name "Copy note to: ")))
          (copy-file (concat (file-name-as-directory rp-notes-folder) nt) name))
      (message "Cursor not over a note"))))

(defun rp-notes-move-next-note ()
  "Find next note marker in the *notes* buffer"
  (interactive)
  ;; Move forward one (if you're on ^* already...).
  (right-char)
  (let ((result (re-search-forward "^*" nil t)))
    ;; Move back to * or back to original char if not found.
    (left-char)))

(defun rp-notes-move-prev-note ()
  "Find previous note marker in the *notes* buffer"
  (interactive)
  (let ((result (re-search-forward "^*" nil t -1)))
    result))

(defun rp-notes-open-dired ()
  "Open dired in the notes folder"
  (interactive)
  (dired rp-notes-folder))

(defun rp-notes-kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))

(defun rp-notes--notes-by-update-time ()
  (let* ((filter (rx string-start
                     (zero-or-more (or
                                    (not (any "]"))
                                    (seq "]" (not (any "]")))))
                     (? "]")
                     (or ".txt" ".md")
                     string-end))   ;; any file *.txt|md without two ]] in the name
         (notes (directory-files-and-attributes rp-notes-folder nil filter t))
         (notes (sort notes (lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
    (mapcar #'car notes)))

(defun rp-notes ()
  "Show list of notes in $HOME/.notes"
  (interactive)
  (rp-notes--create-notes-folder-if-needed)
  (let* ((existing-notes (rp-notes--notes-by-update-time))
         (notes existing-notes)
         (buff (get-buffer-create "*Notes*")))
    (switch-to-buffer buff)
    (rp-notes-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (concat "Notes available in " rp-notes-folder))
      (newline)
      (newline)
      (dolist (nt notes)
        (let* ((line (rp-notes--read-first-non-empty-line
                      (concat (file-name-as-directory rp-notes-folder) nt))))
          (insert (concat "*" (propertize (concat "[[" nt "]]") 'invisible t) "  "))
          (insert line)
          (newline))))
    (beginning-of-buffer)))

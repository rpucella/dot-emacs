(require 'subr-x)

;; TODO:
;;
;; Create own major mode.
;; Command to delete notes.
;; Uniformize path manipulation in unix/windows.
;;

(defvar rp-notes-folder (concat (getenv "HOME") "/.notes"))

(defun --create-notes-folder-if-needed ()
  "Create notes folder if it doesn't exist."
  (unless (file-exists-p rp-notes-folder)
    (make-directory rp-notes-folder)))

(defun rp-new-note ()
  "Create a 'permanent' note in $HOME/.notes"
  (interactive)
  (--create-notes-folder-if-needed)
  (let* ((existing-notes (directory-files rp-notes-folder nil (rx "note-" (zero-or-more digit) ".txt")))
         (note-regexp "note-\\([0-9]+\\).txt" )
         (notes (mapcar (lambda (fname) (save-match-data
                                          (and (string-match note-regexp fname)
                                               (string-to-number (match-string 1 fname)))))
                        existing-notes))
         (new-note (+ (seq-reduce 'max notes 0) 1))
         (new-file (concat rp-notes-folder "/note-" (number-to-string new-note) ".txt"))
         (buff (get-file-buffer new-file)))
    ;; TODO: if the file/buffer already exists, don't insert the # Note thing...
    ;; also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (if (null buff)
        (progn (switch-to-buffer (find-file-noselect new-file))
               (insert "# Note " (number-to-string new-note))
               (newline)
               (newline))
      (switch-to-buffer buff))))

(defun --read-first-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
             unless (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))

(defun --read-note ()
  "Load the note pointed to by the point in a *notes* buffer"
  ;; which line are we on?
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (num-regexp "^* \\([0-9]+\\)")
         (nt (save-match-data
               (and (string-match num-regexp line)
                    (match-string 1 line)))))
    (message line)
    (if nt
        (switch-to-buffer
         (find-file-noselect (concat rp-notes-folder "/note-" nt ".txt")))
      (message "Cursor not over a note"))))

(defun --move-next-note ()
  "Find next note marker in the *notes* buffer"
  (interactive)
  ;; move forward one
  (right-char)
  (let ((result (re-search-forward "^* " nil t)))
    ;; move back to * or back to original char if not found
    (when result
      ;; move back an additional character
      (left-char))
    (left-char)))

(defun --move-prev-note ()
  "Find previous note marker in the *notes* buffer"
  (interactive)
  (let ((result (re-search-forward "^* " nil t -1)))
    result))

(defun --open-dired ()
  "Open dired in the notes folder"
  (interactive)
  (dired rp-notes-folder))

(defun --kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))

(defun --width-of-max-num (lst)
  (if lst
      (length (number-to-string (apply 'max lst)))
    0))

(defvar --notes-name-spacing 3)

(defun rp-notes ()
  "Show list of notes in $HOME/.notes"
  (interactive)
  (--create-notes-folder-if-needed)
    (let* ((existing-notes (directory-files rp-notes-folder nil (rx "note-" (zero-or-more digit) ".txt")))
           (note-regexp "^note-\\([0-9]+\\).txt$" )
           (notes (seq-filter 'numberp
                              (mapcar (lambda (fname) (save-match-data
                                                        (and (string-match note-regexp fname)
                                                             (string-to-number (match-string 1 fname)))))
                                      existing-notes)))
           (buff (get-buffer-create "*Notes*")))
      (switch-to-buffer buff)
      (read-only-mode)
      ;; we probably want to define a new major mode instead
      ;; cf https://stackoverflow.com/questions/27321407/how-to-make-a-buffer-local-key-binding-in-emacs
      (use-local-map (copy-keymap text-mode-map))
      (local-set-key (kbd "RET") '--read-note)
      (local-set-key (kbd "TAB") '--move-next-note)
      (local-set-key (kbd "<backtab>") '--move-prev-note)
      (local-set-key (kbd "n") 'rp-new-note)
      (local-set-key (kbd "r") 'rp-notes)
      (local-set-key (kbd "d") '--open-dired)
      (local-set-key (kbd "q") '--kill)
      (let ((inhibit-read-only t)
            ;; How much room do we give the notes number?
            (width (+ (--width-of-max-num notes) --notes-name-spacing))
            (sorted-notes (sort notes '<)))
        (erase-buffer)
        (insert "Notes available in ")
        (insert rp-notes-folder)
        (newline)
        (newline)
        (dolist (nt sorted-notes)
          (let* ((snt (number-to-string nt))
                 (line (string-trim (car (--read-first-lines (concat rp-notes-folder "/note-" snt ".txt") 1)))))
            (insert (concat "* " snt (make-string (- width (length snt)) ?\s) line))
            (newline))))
      (beginning-of-buffer)))

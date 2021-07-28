
(defun rp-full ()
  "Toggle full/maximal screen"
  (interactive)
  (cond
   ;; windows 10
   ((eq window-system 'w32) (toggle-frame-maximized))
   ;; mac os x
   ((eq window-system 'ns) (toggle-frame-fullscreen))
   ;; do nothing otherwise
   ))

(defun rp-edit-init ()
  "Edit emacs initialization file"
  (interactive)
  (find-file (concat (getenv "HOME") "/.emacs.d/init.el")))

(defun rp-embiggen (size)
  "Embiggen the font - optionally give the size as a parameter"
  (interactive (list (if current-prefix-arg ; <=== User provided arg
                         (prefix-numeric-value current-prefix-arg)
                       rp-default-embiggen-size))) ; <=== Default
  (set-face-attribute 'default nil :height (* size 10)))

(defun rp-new-note ()
  "Create a 'permanent' note in $HOME/.notes"
  (interactive)
  (let ((notes-folder (concat (getenv "HOME") "/.notes")))
    ;; create notes folder if it doesn't exist
    (unless (file-exists-p notes-folder)
      (make-directory notes-folder))
    (let* ((existing-notes (directory-files notes-folder nil (rx "note-" (zero-or-more digit) ".txt")))
           (note-regexp "note-\\([0-9]+\\).txt" )
           (notes (mapcar (lambda (fname) (save-match-data
                                            (and (string-match note-regexp fname)
                                                 (string-to-number (match-string 1 fname)))))
                          existing-notes))
           (new-note (+ (seq-reduce 'max notes 0) 1))
           (new-file (concat notes-folder "/note-" (number-to-string new-note) ".txt"))
           (buff (get-file-buffer new-file)))
      ;; TODO: if the file/buffer already exists, don't insert the # Note thing...
      ;; also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
      (if (null buff)
          (progn (switch-to-buffer (find-file-noselect new-file))
                 (insert "# Note " (number-to-string new-note) "\n\n"))
        (switch-to-buffer buff)))))




      
    
    

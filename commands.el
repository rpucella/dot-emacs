(require 'subr-x)


(defun rp-toggle-fullscreen ()
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
  (find-file (concat-emacs-folder "init.el")))


(defun rp-embiggen (size)
  "Embiggen the font - optionally give the size as a parameter"
  (interactive (list (if current-prefix-arg ; <=== User provided arg
                         (prefix-numeric-value current-prefix-arg)
                       rp-default-embiggen-size))) ; <=== Default
  (set-face-attribute 'default nil :height (* size 10)))


(defun rp-random-uuid ()
  (interactive)
  (insert (rp-generate-random-uuid)))


(defun rp-cheat-sheet ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat-emacs-folder "cheat-sheet.org"))))


(defun rp-date ()
  ;; better - maybe `Thu 5/20/2021`?
  (interactive)
  (insert (format-time-string "%a %m/%d/%y")))


(defun rp-deoutlook (str &optional from to)
  "Translate outlook special characters in pasted text.

When called interactively, work on current paragraph or text selection.

When called in lisp code, if STRING is non-nil, returns a changed string.
If STRIN is nil, change the text in the region between positions FROM and TO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (let* ((workOnStringP (if $string t nil))
         (inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to))))
    (let ((outputStr (let ((case-fold-search t))
                       (replace-regexp-in-string "’" "'" inputStr))))
      (if workOnStringP
          outputStr
        (save-excursion
          (delete-region $from $to)
          (goto-char $from)
          (insert outputStr))))))


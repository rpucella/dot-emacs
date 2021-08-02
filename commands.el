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
  ;; better - maybe `Thursday 5/20/2021`?
  (interactive)
  (insert (format-time-string "%A %m/%d/%y")))




(defvar rp-default-embiggen-size
  (cond
   ((eq window-system 'w32) 16)
   ((eq window-system 'ns) 18)
   ((t 12))))

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


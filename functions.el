
(defun rp-fs ()
  "Custom - toggle full screen"
  (interactive)
  (toggle-frame-fullscreen))

(defun rp-edit-init ()
  (interactive)
  (find-file (concat (getenv "HOME") "/git/dot-emacs/init.el")))

(defun rp-embiggen (size)
  (interactive (list (if current-prefix-arg ; <=== User provided arg
                         (prefix-numeric-value current-prefix-arg)
                       rp-default-embiggen-size))) ; <=== Default
  (set-face-attribute 'default nil :height (* size 10)))


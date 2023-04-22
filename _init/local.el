;; Local definitions for only this particular machine.

(when window-system
  (setup-proportional-font))

(setq rp/default-embiggen-size 16)
(rp/embiggen 16)

(setq default-frame-alist '((height . 64) (width . 112) (left . 2575) (top . 32)))

;; Not working?
(setq-default comint-process-echoes t)

;; Add some zweirn subfolders.
(add-to-list 'zweirn-subfolders '(?o "ooze" "(o)oze" nil))
(add-to-list 'zweirn-subfolders '(?3 "b3" "b(3)" nil))
(add-to-list 'zweirn-subfolders '(?f "/Users/riccardo/Dropbox/Apps/filenotes" "(f)ilenotes" t))
(add-to-list 'zweirn-subfolders '(?p "projects" "(p)rojects" nil))
(add-to-list 'zweirn-subfolders '(?c "commonplace" "(c)ommonplace" nil))

;; Make it a bit wider for my vertical monitor.
(setq-default fill-column 100)

(defun rp/website ()
  ;; TODO: Figure out how to stop this!
  (interactive)
  (shell-command "cd ~/git/rpucella.github.io && python3 -m http.server &"))

(defun rp/local-website ()
  (interactive)
  (shell-command "python3 -m http.server &"))

(defun rp/gen-website ()
  (interactive)
  (shell-command "cd ~/git/rpucella.github.io && ./webgen/bin/webgen"))

(defun rp/open-here ()
  (interactive)
  (shell-comment "open ."))

(defun rp/make ()
  ;; Assumes we have a shell open ready to receive make commands...
  (interactive)
  (switch-to-buffer "*shell*")
  (end-of-buffer)
  (insert "make")
  (comint-send-input))

(global-set-key (kbd "C-c m") 'rp/make)

(setq ispell-program-name "/opt/homebrew/bin/aspell")

;;(load-theme 'sanityinc-tomorrow-blue t)
(load-theme 'monokai t)

;; For SLIME

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-to-list 'zweirn-special-prefixes "SCRATCH")

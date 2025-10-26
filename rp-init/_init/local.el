;; Local definitions for only this particular machine.

(when window-system
  (setup-proportional-font))

(setq rp/default-embiggen-size 16)
(rp/embiggen 16)

(setq default-frame-alist '((height . 64) (width . 112) (left . 2575) (top . 32)))

;; Not working?
;; (Causes a processp wrong argument error upon exiting a comint.)
;; (setq-default comint-process-echoes t)

;; Add some zweirn notebooks.
(add-to-list 'zweirn-notebooks '(?b "backlog" "(b)acklog"))
(add-to-list 'zweirn-notebooks '(?o "ooze" "(o)oze"))
(add-to-list 'zweirn-notebooks '(?3 "b3" "b(3)"))
(add-to-list 'zweirn-notebooks '(?f "/Users/riccardo/Dropbox/Apps/filenotes" "(f)ilenotes"))
(add-to-list 'zweirn-notebooks '(?p "projects" "(p)rojects"))
(add-to-list 'zweirn-notebooks '(?c "commonplace" "(c)ommonplace"))
(add-to-list 'zweirn-special-prefixes "SCRATCH")
(add-to-list 'zweirn-special-prefixes "SCHEDULE")

;; (setq zweirn-note-symbol "↳")


;; Make it a bit wider for my vertical monitor.
(setq-default fill-column 100)

(defun rp/website ()
  ;; TODO: Figure out how to stop this!
  (interactive)
  (shell-command "cd ~/for/rpucella.github.io && python3 -m http.server &"))

(defun rp/local-website ()
  (interactive)
  (shell-command "python3 -m http.server &"))

(defun rp/gen-website ()
  (interactive)
  (shell-command "cd ~/for/rpucella.github.io && ./webgen/bin/webgen"))

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

;; For writing

(defun --kill-buffer-frame ()
  (interactive)
  (kill-buffer)
  (delete-frame))


(defvar --writing-frame-mode-map (make-sparse-keymap)
  "Keymap while writing-frame-mode is active.")

(define-minor-mode --writing-frame-mode
  "A temporary minor mode to be activated only specific to a buffer."
  nil
  :lighter " WF"
  --writing-frame-mode-map)

(defun rp/writing-frame (&optional buffer)
  (interactive)
  ;; Fork off new frame
  (let* ((curr-window (selected-window)))
    (make-frame '((left . 32)
                  (top . 32)
                  (height . 64)
                  (width . 200)
                  (fullscreen . nil)
                  (background-color . "white")
                  (foreground-color . "black")
                  (border-color . "white")
                  (cursor-color . "red")))
    (when buffer
      (pop-to-buffer-same-window buffer))
    ;; Ditch the mode line.
    (setq mode-line-format nil)
    ;; Disable line numbers and auto-fill if present.
    (display-line-numbers-mode -1)
    (auto-fill-mode -1)
    ;; Larger font
    (set-frame-font (font-spec :size 24) t (list (selected-frame)))
    ;; Change this in a frame-local manner?
    (set-face-attribute 'fringe (selected-frame) :background "white")
    ;; Writing mode.
    (olivetti-mode)
    ;; Killing the original buffer will kill the frame too.
    ;; Cf: https://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer
    (--writing-frame-mode 1)
    (define-key --writing-frame-mode-map (kbd "C-x k") '--kill-buffer-frame)
  ;;;(local-set-key (kbd "C-x k") '--kill-buffer-frame)
    ;; Not sure if this helps.
    (set-window-dedicated-p nil t)
    ))

;; Overrides!

(defun zweirn-create-writing-frame (buffer)
  (make-frame '((left . 32)
                (top . 32)
                (height . 64)
                (width . 200)
                (fullscreen . nil)
                (background-color . "white")
                (foreground-color . "black")
                (border-color . "white")
                (cursor-color . "red")))
  (when buffer
    (pop-to-buffer-same-window buffer)))

(defun zweirn-setup-writing-frame ()
  (setq mode-line-format nil)
  ;; Disable line numbers and auto-fill if present.
  (display-line-numbers-mode -1)
  (auto-fill-mode -1)
  ;; Larger font
  (set-frame-font (font-spec :size 24 :family "Courier Prime") t (list (selected-frame)))
  ;; Change this in a frame-local manner?
  (set-face-attribute 'fringe (selected-frame) :background "white")
  ;; Writing mode.
  (olivetti-mode)
  ;; Killing the original buffer will kill the frame too.
  ;; Cf: https://emacs.stackexchange.com/questions/519/key-bindings-specific-to-a-buffer
  (--writing-frame-mode 1)
  (define-key --writing-frame-mode-map (kbd "C-x k") '--kill-buffer-frame)
  ;;;(local-set-key (kbd "C-x k") '--kill-buffer-frame)
  ;; Not sure if this helps.
  (set-window-dedicated-p nil t))

;; Replace MD conversion to HTML + Firefox

(setq rp/pdf-convert-markdown "~/git/simple-markdown/bin/md %s > %s.html")
(setq rp/pdf-open "open %s.html")

;; (gptel-make-gemini "Gemini" :key "AIzaSyCUj7mvm1fqq32C-b89e5VaOwx8IXAdqTs" :stream t)

;; To update to newest models: may need to refresh gtpel,
;; and look up the appropriate model name in gptel-gemini.el.
;; cf: https://github.com/karthink/gptel/blob/master/gptel-gemini.el

(use-package gptel
  :config
  (setq
   gptel-model "gemini-2.0-flash-thinking-exp-01-21"
   gptel-backend (gptel-make-gemini "Gemini"
                   :key "AIzaSyCUj7mvm1fqq32C-b89e5VaOwx8IXAdqTs"
                   :stream t))
  (global-set-key (kbd "C-c g") 'gptel-menu))

;; Disable python readline support.

(setq python-shell-completion-native-enable nil)

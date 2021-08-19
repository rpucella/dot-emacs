;; use M-x package-install-selected-packages when installing on a new system

(require 'package)
(require 'display-line-numbers)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))
;; Added by Package.el. Must be before any package configuration.
(package-initialize)

;; identify machines + define defaults

(defvar rp-is-macos
  (eq window-system 'ns))

(defvar rp-is-windows
  (eq window-system 'w32))

(defun concat-emacs-folder (fname)
  (let ((emacs-folder (concat (file-name-as-directory (getenv "HOME")) ".emacs.d")))
    (concat (file-name-as-directory emacs-folder) fname)))

(load-library (concat-emacs-folder "functions.el"))
(load-library (concat-emacs-folder "commands.el"))
(load-library (concat-emacs-folder "notes.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization via M-x customize
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("08765d801b06462a3ce7e414cdb747436ccaf0c073350be201d8f87bd0481435" default)))
 '(package-selected-packages
   (quote
    (ssh magit restclient ws-butler green-phosphor-theme green-is-the-new-black-theme dracula-theme go-mode web-mode markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Screen setup
;;

(when window-system
  ;; WINDOW SYSTEM
  (setq default-frame-alist '((height . 40) (width . 120) (left . 24) (top . 24)))
  (set-frame-font "Hack-12" nil t)
  (load-theme 'green-phosphor t)
  (custom-theme-set-faces 'green-phosphor
                          '(mode-line ((t (:foreground "black" :background "LimeGreen" :box nil)))))
  (enable-theme 'green-phosphor)
  ;;(load-theme 'dracula t)
  ;;(load-theme 'green-is-the-new-black t)
  ;; stop blinking
  (blink-cursor-mode 0)
  )

(when (not window-system)
  ;; TEXT SYSTEM
  (menu-bar-mode 0)
  )
    
(defun --flash-mode-line ()
  (interactive)
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
  
(setq visible-bell nil
      ring-bell-function '--flash-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General settings
;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; enable line numbers
(defun display-line-numbers--turn-on ()
  ;; turn on line number mode for listed modes
  (message (symbol-name major-mode))
  (when (derived-mode-p 'prog-mode 'text-mode)
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always) ;; don't ask when creating new buffer

;; use spaces for indentation in lieu of tabs
(setq-default indent-tabs-mode nil)

;; disable warning about upcase-region
(put 'upcase-region 'disabled nil)

;; shells are login shells
(setq explicit-bash-args '("--noediting" "-i" "-l"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key bindings
;;
;; C-c <letter> is reserved, so use that

(defalias 'custom-notes-keymap (make-sparse-keymap))
(defvar custom-notes-map (symbol-function 'custom-notes-keymap))

(global-set-key (kbd "RET") 'newline-and-indent)  ;; indent automatically
(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key (kbd "C-c f") 'rp-toggle-fullscreen)
(global-set-key (kbd "C-c n") 'custom-notes-keymap)
(global-set-key (kbd "C-c c") 'rp-cheat-sheet)

(define-key custom-notes-map (kbd "l") 'rp-notes)
(define-key custom-notes-map (kbd "n") 'rp-new-note)

(when rp-is-macos 
  (global-set-key (kbd "<s-return>") 'rp-toggle-fullscreen))

;; override with local settings
(if (file-readable-p (concat-emacs-folder "local.el"))
    (load-library (concat-emacs-folder "local.el")))

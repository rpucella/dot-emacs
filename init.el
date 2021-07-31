;; use M-x package-install-selected-packages when installing on a new system

(require 'package)
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

(defvar rp-is-forrester
  ;; only one running windows right now
  (eq system-type 'windows-nt))

(defun file-in-emacs-folder (fname)
  (let ((emacs-folder (concat (file-name-as-directory (getenv "HOME")) ".emacs.d")))
    (concat (file-name-as-directory emacs-folder) fname)))

(load-library (file-in-emacs-folder "functions.el"))
(load-library (file-in-emacs-folder "commands.el"))
(load-library (file-in-emacs-folder "notes.el"))


;; PUT IN local.el
;;
;;
;; (rp-embiggen ...):
;;   (rp-is-maximillian 14)
;;   (rp-is-forrester 14)
;;
;; move to local.el on forrester
;;
;; (when rp-is-forrester
;;   ;; load Forrester-specific functionality when on windows
;;   (load-library (file-in-emacs-folder "forrester.el")))
;;
;; (when rp-is-forrester
;;   ;; start up in a reasonable directory
;;   (cd (getenv "HOME"))
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization via M-x customize
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit restclient ws-butler green-phosphor-theme green-is-the-new-black-theme dracula-theme go-mode web-mode markdown-mode)))
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

(defun --setup-windows ()
  ;; might depend on the machine
  (setq default-frame-alist '((height . 40) (width . 120) (left . 24) (top . 24)))
  (set-frame-font "Hack-12" nil t)
  (load-theme 'green-phosphor t)
  (custom-theme-set-faces 'green-phosphor
                          '(mode-line ((t (:foreground "black" :background "LimeGreen" :box nil)))))
  (enable-theme 'green-phosphor)
  ;;(load-theme 'dracula t)
  ;;(load-theme 'green-is-the-new-black t)
  (when rp-is-macos 
    ;; full screen
    (global-set-key (kbd "<s-return>") 'rp-toggle-fullscreen))
  ;; stop blinking
  (blink-cursor-mode 0)
  )

(defun --setup-text ()
  (menu-bar-mode 0)
  )
    
(if window-system (--setup-windows) (--setup-text))

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
(require 'display-line-numbers)
;; need to put in "don't put lns on some modes" restriction here
;; cf: https://www.emacswiki.org/emacs/LineNumbers
(global-display-line-numbers-mode)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
;; don't ask when creating new buffer
(setq ido-create-new-buffer 'always)

;; use spaces for indentation in lieu of tabs
(setq-default indent-tabs-mode nil)

;; disable warning about upcase-region
(put 'upcase-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; new key bindings
;;
;; C-c <letter> is reserved, so use that

(defalias 'custom-notes-keymap (make-sparse-keymap))
(defvar custom-notes-map (symbol-function 'custom-notes-keymap))

(global-set-key (kbd "RET") 'newline-and-indent)  ;; indent automatically
(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key (kbd "C-c f") 'rp-toggle-fullscreen)
(global-set-key (kbd "C-c n") 'custom-notes-keymap)

(define-key custom-notes-map (kbd "l") 'rp-notes)
(define-key custom-notes-map (kbd "n") 'rp-new-note)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; not processed yet


;; shells are login shells
(setq explicit-bash-args '("--noediting" "-i" "-l"))

;; ocaml mode for emacs on Max
(ignore-errors
    (load "/Users/riccardo/.opam/system/share/emacs/site-lisp/tuareg-site-file")
  )

;; override with local settings

(if (file-readable-p (file-in-emacs-folder "local.el"))
    (load-library (file-in-emacs-folder "local.el")))

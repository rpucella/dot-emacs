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

(load-library (concat (getenv "HOME")"/.emacs.d/functions.el"))



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
   '(ws-butler green-phosphor-theme green-is-the-new-black-theme dracula-theme go-mode web-mode markdown-mode)))
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

(setq rp-default-embiggen-size 12)

(when window-system
  ;; might depend on the machine
  (setq default-frame-alist '((height . 48) (width . 160)))
  (setq rp-default-embiggen-size 18)
  (set-frame-font "Hack-12" nil t)
  (load-theme 'green-phosphor t)
  (custom-theme-set-faces 'green-phosphor
                          '(mode-line ((t (:foreground "black" :background "LimeGreen" :box nil)))))
  (enable-theme 'green-phosphor)
  ;;(load-theme 'dracula t)
  ;;(load-theme 'green-is-the-new-black t)
  (rp-embiggen rp-default-embiggen-size)
  ;; full screen
  (global-set-key (kbd "<s-return>") 'rp-fs)
  ;; stop blinking
  (blink-cursor-mode 0)
  )
  
(when (not window-system)
  ;; Text mode
  ;; get rid of menu bar in text mode
  (menu-bar-mode 0)
  )

(when (eq system-type 'windows-nt)
  ;; start up in a reasonable directory
  (cd (concat "C:/Users/" (getenv "USERNAME")))
  )


;; machine-specific initial window size?
;;(when (string= (getenv "HOSTNAME") "Maximillian") ...)
;; cf https://stackoverflow.com/questions/16481984/get-width-of-current-monitor-in-emacs-lisp/16484107


(defun -flash-mode-line ()
  (interactive)
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
  
(setq visible-bell nil
      ring-bell-function '-flash-mode-line)


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
(global-linum-mode 1)

;; indent automatically
(define-key global-map (kbd "RET") 'newline-and-indent)

;; IDO

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
;; don't ask when creating new buffer
(setq ido-create-new-buffer 'always)

;; use spaces for indentation in lieu of tabs
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; not processed yet


;; shells are login shells

(setq explicit-bash-args '("--noediting" "-i" "-l"))

(put 'upcase-region 'disabled nil)


;; ocaml mode for emacs

(ignore-error
    (load "/Users/riccardo/.opam/system/share/emacs/site-lisp/tuareg-site-file")
  )


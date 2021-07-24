(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Added by Package.el. Must be before any package configuration.
(package-initialize)

(setq inhibit-startup-screen t)

(when (and (eq system-type 'darwin) window-system)
  ;; Mac OS X GUI
  (setq default-frame-alist '((height . 48) (width . 160))))

(when (not window-system)
  ;; Text mode
  ;; get rid of menu bar in text mode
  (menu-bar-mode 0))

;; machine-specific initial window size?
;;(when (string= (getenv "HOSTNAME") "Maximillian") ...)
;; cf https://stackoverflow.com/questions/16481984/get-width-of-current-monitor-in-emacs-lisp/16484107

; remove tabs indentation
(setq-default indent-tabs-mode nil)

(global-font-lock-mode 0)

;; shells are login shells

(setq explicit-bash-args '("--noediting" "-i" "-l"))

(put 'upcase-region 'disabled nil)


; ocaml mode for emacs

(load "/Users/riccardo/.opam/system/share/emacs/site-lisp/tuareg-site-file")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(go-mode web-mode markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun rp-fs ()
  "Custom - toggle full screen"
  (interactive)
  (toggle-frame-fullscreen))

(defun rp-edit-init ()
  (interactive)
  (find-file (concat (getenv "HOME") "/git/dot-emacs/init.el")))

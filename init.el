(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-screen t)

; get rid of menu bar in text mode
(menu-bar-mode 0)

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

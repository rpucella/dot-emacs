
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
        ("MELPA"        . 1)))
;; Added by Package.el. Must be before any package configuration.
(package-initialize)

;; Identify machines + define defaults.

(defvar rp/is-macos
  (eq window-system 'ns))

(defvar rp/is-windows
  (eq window-system 'w32))


(defun concat-emacs-folder (&rest names)
  "Creates a file name from list of subfolders from ~/.emacs.d root."
  (let* ((result user-emacs-directory))
    (dolist (name names)
      (setq result (concat (file-name-as-directory result) name)))
    result))

;; To allow loading of custom themes via load-theme
(add-to-list 'custom-theme-load-path (concat-emacs-folder "_init/"))

(load (concat-emacs-folder "_init" "commands.el") nil)
(load (concat-emacs-folder "_init" "zweirn.el") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customization via M-x customize
;;

(setq custom-file (concat-emacs-folder "_init" "custom.el"))
(load custom-file 'noerror)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Screen setup
;;

(defvar fixed-pitch-modes
  '(comint-mode calendar-mode dired-mode slime-repl-mode))

(defun some-derived-mode-p (lst)
  (let ((result nil)
        (curr lst))
    (while (and curr (not result))
      (when (derived-mode-p (car curr))
        (setq result t))
      (setq curr (cdr curr)))
    result))
    
(defun setup-base-fonts ()
  ;; Fixed pitch font is Hack.
  ;; Variable pitch font is DejaVu Serif.
  ;; TODO: Check to make sure fonts exist.
  ;; TODO: When is 'fixed-pitch-serif' used ?
  ;;  cf https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
  ;;(set-face-attribute 'fixed-pitch nil :font "Hack-12")
  ;;(set-face-attribute 'variable-pitch nil :font "DejaVu Serif-12")
  (set-face-attribute 'fixed-pitch nil :font "Input Mono Narrow-12")
  (set-face-attribute 'variable-pitch nil :font "Input Sans Narrow-12")
  ;; Make sure line numbers are always fixed pitch.
  (set-face-font 'line-number (face-font 'fixed-pitch))
  ;; Force some modes to be fixed-pitch (shell, etc)
  ;;  cf https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (some-derived-mode-p fixed-pitch-modes)
                (fixed-pitch))))
  )

(defun fixed-pitch ()
  "Switch the current buffer to the default fixed pitch font"
  (interactive)
  (setq buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode))

(defun setup-monospace-font ()
  ;; Monospace - easy peasy.
  (set-face-font 'default (face-font 'fixed-pitch))
  ;(set-frame-font (face-font 'default))
  (set-frame-font (face-font 'fixed-pitch))
  )

(defun setup-proportional-font ()
  ;; TODO: make it easier to switch between monospace and proportional space fonts.
  ;;  cf  https://benghancock.github.io/blog/2022/tao-of-acme.html
  (set-face-font 'default (face-font 'variable-pitch))
  ;(set-frame-font (face-font 'default))
  (set-frame-font (face-font 'fixed-pitch))
  ;;(set-frame-font "-*-DejaVu Serif-normal-normal-normal-*-18-*-*-*-p-0-iso10646-1" nil t)
  ;;(set-face-font 'fixed-pitch "Hack")
  (setq markdown-list-item-bullets '("▸" "•" "•" "•" "•" "•" "•"))
  ;; Bar cursors look better with proportional fonts.
  (setq-default cursor-type 'bar)
  )

(defun setup-theme ()
  ;; (load-theme 'green-phosphor t)
  ;; (custom-theme-set-faces 'green-phosphor
  ;;                        '(mode-line ((t (:foreground "black" :background "LimeGreen" :box nil)))))
  ;; (enable-theme 'green-phosphor)
  ;; (load-theme 'calm-forest t)
  (load-theme 'monokai t)
  )



(when window-system
  ;; WINDOW SYSTEM
  (setq default-frame-alist '((height . 40) (width . 120) (left . 24) (top . 24)))

  (setup-base-fonts)
  ;; Choose one, and choose wisely.
  (setup-monospace-font)
  (setup-theme)
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
  
(setq visible-bell nil)
(setq ring-bell-function '--flash-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General settings
;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-input-method 'TeX)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;(setq kill-buffer-query-functions
;      (remq 'process-kill-buffer-query-function
;            kill-buffer-query-functions))

;; enable line numbers
(defun display-line-numbers--turn-on ()
  ;; turn on line number mode for listed modes
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
(setq explicit-zsh-args '("-l"))

;; Cannot put this in markdown-mode-hook, since needs to be set before
;; mode starts.
(require 'markdown-mode)
(setq-default markdown-hide-markup t)
;; Fix markdown mode hrs to shave off some width to allow for line numbers.
;; Basically, wrap a dynamic letf around `markdown-fontify-hrs` which rebinds
;; `window-body-width` to give you back the width of the window minus something.
;; Cf:
;;   https://www.reddit.com/r/emacs/comments/bjgajb/what_is_the_preferred_way_to_dynamically_add_and/
(defvar markdown-width-adjustment 50)
(advice-add 'markdown-fontify-hrs :around
            (lambda (originalf last)
              (let ((curr-width (window-body-width)))
                
                (cl-letf (((symbol-function 'window-body-width)
                           (lambda () (- curr-width markdown-width-adjustment))))
                  (funcall originalf last)))))

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key bindings
;;
;; C-c <letter> is reserved, so use that

(global-set-key (kbd "RET") 'newline-and-indent)  ;; indent automatically
(global-set-key (kbd "C-c x") 'execute-extended-command)
(global-set-key (kbd "C-c f") 'rp/toggle-fullscreen)
(global-set-key (kbd "C-c n") 'zweirn)
(global-set-key (kbd "C-c c") 'zweirn-create-note)
(global-set-key (kbd "C-c j") 'zweirn-jot-note)
(global-set-key (kbd "C-c t") 'zweirn-today-note)
(global-set-key (kbd "C-c /") 'zweirn-nv-search)
(global-set-key (kbd "C-c C") 'rp/cheat-sheet)
(global-set-key (kbd "C-c s") 'shell)

;; I find this one more useful.
;; Original binding: ido-kill-buffer.
;; Another possibility: ido-kill-buffer-at-head.
;; I never used kill-buffer to kill *another* buffer than the current one.
(global-set-key (kbd "C-x k") 'kill-buffer-and-possibly-window)

(defun kill-buffer-and-possibly-window ()
  (interactive)
  ;; If the current window is the sole window in the frame, do a normal kill-buffer.
  ;; Other, kill the buffer and ditch the window.
  (if (= (length (window-list (window-frame))) 1)
      (kill-buffer)
    (kill-buffer-and-window)))

(when rp/is-macos 
  (global-set-key (kbd "<s-return>") 'rp/toggle-fullscreen)
  ;; right option key is used for accents (standard Option)
  (setq mac-right-option-modifier 'none))

;; override with local settings
(load (concat-emacs-folder "_init" "local.el") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Word count minor mode
;;
;; From: https://www.dr-qubit.org/emacs-misc/wc-mode.el

(setq mode-line-position (assq-delete-all 'wc-mode mode-line-position))

(setq mode-line-position
      (append
       mode-line-position
       '((wc-mode
	  (6 (:eval (if (use-region-p)
				(count-words-region (point) (mark))
		      (format " [%dw]"
			      (count-words-region (point-min) (point-max))))))
	  nil))))

(define-minor-mode wc-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of characters, words, and lines is
displayed in the mode-line.")


;; Remove trailing whitespace on lines that have been changed.
;; Cf: https://github.com/lewang/ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

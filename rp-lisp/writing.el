
;; Writing minor mode
;;
;; - Olivetti margins
;; - Black on white
;; - Full frame
;; - No mode line

;; TODO:
;; - way to pop a buffer out into its own frame?
;; - prevent splitting using advice-add locally?
;; - minibuffer in white as well?

(require 'olivetti)
(require 'face-remap)

(defvar writing-mode-map (make-sparse-keymap)
  "Keymap while writing-frame-mode is active.")


(define-minor-mode writing-mode
  "A temporary minor mode to be activated only specific to a buffer."
  :lighter " Wr"
  :keymap writing-mode-map
  (if writing-mode
      ;; disable
      (writing-mode--enable)
    ;; enable
    (writing-mode--disable)))


(defun writing-mode--enable ()
  ;; Remember these so that we can restore them!
  (make-local-variable 'writing-mode--saved-settings)
  (let* ((foregrounds (mapcar (lambda (face)
                                (if (stringp (face-attribute face :foreground))
                                    (cons face (face-attribute face :foreground))
                                  nil)) (face-list))))
    (setq writing-mode--saved-settings
          (list
           :mode-line mode-line-format
           :line-numbers (if display-line-numbers-mode 1 0)
           :olivetti (if olivetti-mode 1 0)
           :fringe (face-attribute 'fringe :background)
           :foregrounds foregrounds
           :buffer-face (if buffer-face-mode 1 0)))
    ;; Ditch the mode line.
    (setq mode-line-format nil)
    ;; Disable line numbers and auto-fill if present.
    (display-line-numbers-mode 0)
    ;; Nice margins.
    (olivetti-mode 1)
    ;; Full frame if we were split.
    (delete-other-windows)
    (face-remap-add-relative 'fringe '(:background "white"))
    (mapc (lambda (fcpair) (when fcpair
                             (face-remap-add-relative (car fcpair) '(:foreground "black")))) foregrounds)
    (setq buffer-face-mode-face '(:family "Courier Prime" :height 160 :background "white" :foreground "black"))
    (buffer-face-mode 1)))


(defun writing-mode--disable ()
  (setq mode-line-format (plist-get writing-mode--saved-settings :mode-line))
  (display-line-numbers-mode (plist-get writing-mode--saved-settings :line-numbers))
  (olivetti-mode (plist-get writing-mode--saved-settings :olivetti))
  (face-remap-add-relative 'fringe `(:background ,(plist-get writing-mode--saved-settings :fringe)))
  (mapc (lambda (fcpair)
          (when fcpair
            (face-remap-add-relative (car fcpair) `(:foreground ,(cdr fcpair)))))
        (plist-get writing-mode--saved-settings :foregrounds))
  (buffer-face-mode (plist-get writing-mode--saved-settings :buffer-face)))


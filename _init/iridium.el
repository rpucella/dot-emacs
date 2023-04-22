
(require 'json)
;; (require 'subr-x)


(defvar iridium-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (key "q") 'iridium-quit)
    map)
  "Keymap for Iridium Reader mode.")


(define-derived-mode iridium-reader-mode
  special-mode "Iridium-Reader"
  "Major mode for playing Iridium games."
  (setq-local revert-buffer-function
              #'iridium--revert-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODE PARAMETERS.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HELPER FUNCTIONS.

(defun iridium--revert-buffer (_ignore-auto _noconfirm)
  ;; do nothing for now
  )


(defun iridium--read-json (file)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file file)))


(defun iridium--show-passage (title subtitle folder passage &optional start)
  (let* ((buff-name (format "*Iridium* %s: %s" title passage)))
    (rename-buffer buff-name)
    (erase-buffer)
    (let ((inhibit-read-only t))
      (when start
        (newline)
        (insert (upcase title))
        (center-line)
        ;; Don't know why this is needed.
        (newline 2) 
        (when subtitle
          (insert subtitle)
          (center-line)
          (newline 1))
        (newline 1))
      (iridium--read-passage folder passage))))


(defun iridium--read-passage (folder passage)
  ;; Probably need to parse the file instead of just dumping
  ;; it into the buffer willy nilly.
  (insert-file-contents-literally (concat (file-name-as-directory folder) "passages/" passage ".txt"))
  (fill-region (point-min) (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXPORTED FUNCTIONS.

(defun iridium--quit ()
  (interactive)
  (when (yes-or-no-p "Quit? ")
    (kill-buffer (current-buffer))))

  
(defun iridium (file)
  (interactive (list (read-file-name "Iridium JSON file: ")))
  (let* ((game-json (iridium--read-json file))
         (game-title (gethash "title" game-json))
         (game-subtitle (gethash "subtitle" game-json))
         (game-start (gethash "init" game-json))
         (game-folder (file-name-directory file))
         (buff-name (format "*Iridium* %s:" game-title))
         (buff (get-buffer-create buff-name)))
    (switch-to-buffer buff)
    (iridium-reader-mode)
    (iridium--show-passage game-title game-subtitle game-folder game-start t)
    ))

(provide 'iridium)

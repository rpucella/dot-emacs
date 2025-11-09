;;; twine.el --- Twine-like stories in Emacs    -*- lexical-binding: t -*-

;; A game is a set of scenes
;; A scene is a tree description of the possible passages
;; Baed on state, the scene resolves into a passage

;; Each passage:
;; - shows an optional header
;; - shows paragraphs of text
;; - shows a series of options
;; - hitting an option navigates to a new scene

;; Wish list:
;; - remove internal so that everything in the game has a source on disk
;; - ability to run the game in "dev" mode
;;     - track state at every point?
;;     - edit scene from within the game
;; - run multiple Twine games
;;     - each buffer tracks its own information

(require 'widget)

;; cf: deft-mode
;; Deriving from special-mode means that you can always invode "twine-mode" even when you're in some other buffer.
;; Comparse to deft-mode, which defines (put 'deft-mode 'mode-class 'special)!

(define-derived-mode twine-mode
  special-mode "Twine"
  "Major mode for running a Twine story.")

(define-key twine-mode-map (kbd "SPC") 'twine-scroll)
(define-key twine-mode-map (kbd "TAB") 'widget-forward)
(define-key twine-mode-map (kbd "<backtab>") 'widget-backward)
(define-key twine-mode-map (kbd "d") 'twine-toggle-dev)

;; State management.
;; Can we make it even more abstract?
;; - state is a :vars :state plist
;; - check var name before read/write
;; Can we automatically trigger re-renders after state changes?
;; Probably not. The best I can think of right now is have a set of update functions
;;   that when called automatically call a render function.
(defmacro twine--make-state (&rest vars)
  ;; vars = (:name init refresh-fn)
  `(let* ((state (make-hash-table))
          (refresh (make-hash-table))
          (vars (list ,@(mapcar (lambda (v) `(list ,(car v) ,(cadr v) ,(if (cddr v) `(quote ,(caddr v)) 'nil))) vars))))
     (make-local-variable 'twine--*state*)
     (dolist (var vars)
       (puthash (car var) (cadr var) state)
       (when (cddr var)
         (puthash (car var) (caddr var) refresh)))
     (setq twine--*state* (list :state state :refresh refresh))))

(defmacro twine--sget (field)
  `(gethash ,field (plist-get twine--*state* :state)))

(defmacro twine--sput (field value)
  `(puthash ,field ,value (plist-get twine--*state* :state)))

(defun twine (game-file-name)
  ;; M-x twine to start a game
  (interactive (list (read-file-name "Game filename: ")))
  (let* ((name (format "Twine: %s" (file-name-nondirectory game-file-name)))
         (buff (get-buffer-create name))
         (game-obj (twine--load-game game-file-name)))
    ;; check if buffer exists?
    (switch-to-buffer buff)
    (twine-mode)
    (twine--make-state
     (:game-file-name game-file-name)
     (:is-dev nil)
     (:game (plist-get game-obj :game))
     (:game-dir (plist-get game-obj :dir))
     (:scenes (plist-get (plist-get game-obj :game) :scenes))
     (:scene-name nil)
     (:state nil))
    (twine--render)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UPDATE FUNCTIONS
;;
;; Use these functions to update the state + re-render
;;
(defun twine--update-game (game-obj)
  (twine--sput :game (plist-get game-obj :game))
  (twine--sput :game-dir (plist-get game-obj :dir))
  (twine--sput :scenes (plist-get (plist-get game-obj :game) :scenes))
  (twine--sput :scene-name nil)
  (twine--sput :state nil)
  (twine--render))

(defun twine--update-scenes (scenes state)
  ;; refresh scenes
  (twine--sput :scenes scenes)
  ;; Reset state to what it was before
  (twine--sput :state state)
  (twine--render))
(defun twine--update-scene (scene-name state)
  (twine--sput :scene-name scene-name)
  (twine--sput :state state)
  (twine--render))

(defun twine--update-dev (is-dev)
  (twine--sput :is-dev is-dev)
  (twine--render))

(defun twine-toggle-dev ()
  (interactive)
  ;; Only available if we have a game directory.
  (when (twine--sget :game-dir)
    (twine--update-dev (not (twine--sget :is-dev)))))

(defun twine--join (dir file)
  (concat (file-name-as-directory dir) file))

;; Walk through a directory
;; and gather all non-hidden files
;; and collect into a scenes object.
(defun twine--gather-scenes (dir)
  ;; Real all files that do not start with _ and don't end with ~
  (let* ((filter (rx string-start
                     (not (or ?_ ?.))
                     (zero-or-more (any print))
                     (not ?~)
                     string-end))
         (files (directory-files-and-attributes dir nil filter))
         (names (mapcar 'car files))
         (scenes nil))
    (dolist (name names)
      (message (format "Reading %s" name))
      (let* ((file (twine--join dir name))
             (content (twine--read-all file)))
        (push (cons (intern name) content) scenes)))
    (message (format "%s scenes read" (length scenes)))
    scenes))

(defun twine--read-all (file)
  (let* ((content nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (ignore-errors (push (read (current-buffer)) content))))
    (nreverse content)))

(defun twine--load-game (file-name)
  (with-temp-buffer
    (let* ((game nil)
           (dir nil))
      (insert-file-contents file-name)
      (goto-char (point-min))
      (setq game (read (current-buffer)))
      (when (stringp (plist-get game :scenes))
        (setq dir (twine--join (file-name-directory file-name) (plist-get game :scenes)))
        (plist-put game :scenes (twine--gather-scenes dir)))
      (list :game game :dir dir))))

(defun twine-scroll ()
  (interactive)
  (scroll-up))

(defun twine--render ()
  (if (twine--sget :scene-name)
      (twine--render-scene-with-info)
    (twine--render-scene-splash)))

(defun twine--render-scene-with-info ()
  (let* ((scene-name (twine--sget :scene-name))
         (state (twine--sget :state))
         (scenes (twine--sget :scenes))
         (scene (cdr (assq scene-name scenes)))
         (error-scene `((text ,(format "ERROR: scene does not exist: %s" scene-name)))))
    (if scene
        (twine--render-scene scene-name scene state)
      (twine--render-scene scene-name error-scene state))))

(defun twine--render-scene-splash ()
  (let* ((game (twine--sget :game))
         (state (make-hash-table))
         (scene-name 'start)
         (scenes (twine--sget :scenes))
         (scene (cdr (assq scene-name scenes)))
         (error-scene `((text ,(format "ERROR: scene does not exist: %s" scene-name))))
         (splash `((title ,(plist-get game :title) ,(plist-get game :author))
                   (eval ,(plist-get game :init))
                   ,@scene)))
    (twine--render-scene 'start splash state)))

(defun twine--clear-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun twine--render-scene (scene-name scene state)
  (let* ((prev-state (copy-hash-table state))
         (passage (twine--extract-passage-from-scene scene state))
         (header (twine--extract-header-blocks passage))
         (title (twine--extract-title-blocks passage))
         (text (twine--extract-text-blocks passage))
         (options (twine--extract-option-blocks passage)))
    (twine--clear-buffer)
    (widget-insert "\n")
    (when title
      (let* ((title-text (upcase (twine--get-title-text (car title))))
             (title-author (twine--get-title-author (car title))))
        (twine--create-banner title-text title-author)))
    (when header
      (let ((htext (upcase (twine--get-header-text (car header)))))
        (widget-insert (format "%s\n" htext))
        (widget-insert (format "%s\n\n" (make-string (length htext) ?=)))))
    (widget-insert (twine--fill-paragraphs text))
    (dolist (opt options)
      (widget-create 'link
                     :button-prefix "* "
                     :button-suffix ""
                     :notify (lambda (&rest ignore)
                               (twine--update-scene (twine--get-option-scene-name opt) state))
                     (format "%s" (twine--get-option-text opt)))
      (widget-insert "\n"))
    ;; Show dev options if we are in dev mode and have a scene name (not on the splash page).
    (when (and scene-name (twine--sget :is-dev) (twine--sget :game-dir))
      (widget-insert "\n\n")
      (dolist (dopt (twine--build-dev-options scene-name prev-state))
        (widget-create 'link
                       :button-prefix "[ "
                       :button-suffix " ]"
                       :notify (lambda (&rest ignore) (funcall (twine--get-doption-action dopt)))
                       (format "%s" (twine--get-doption-text dopt)))
        (widget-insert "\n")))
    (widget-setup)
    (goto-char (point-min))))

(defun twine--create-banner (title author)
  (let* ((author (format "By %s" author))
         (width (+ (max (length title) (length author)) 4)))
    (widget-insert (format "%s\n" (make-string (+ width 2) ?-)))
    (widget-insert (format "|%s|\n" (twine--center-string width title)))
    (widget-insert (format "|%s|\n" (make-string width ? )))
    (widget-insert (format "|%s|\n" (twine--center-string width author)))
    (widget-insert (format "%s\n\n" (make-string (+ width 2) ?-)))))

(defun twine--center-string (width s)
  (format "%s%s%s" (make-string (/ (- width (length s)) 2) ? ) s (make-string  (/ (+ (- width (length s)) 1) 2) ? )))

(defun twine--refresh-game ()
  ;; Reload the game file and restart.
  (let* ((game-obj (twine--load-game (twine--sget :game-file-name))))
    (twine--update-game game-obj)))

(defun twine--refresh-scene (prev-state)
  ;; Reload the scenes and rerun the current scene.
  (let* ((scenes (twine--gather-scenes (twine--sget :game-dir))))
    (twine--update-scenes scenes prev-state)))

(defun twine--build-dev-options (scene-name prev-state)
  (let ((scene-file (twine--join (twine--sget :game-dir) (symbol-name scene-name)))
        (notes-file (twine--join (twine--sget :game-dir) "_notes"))
        (width 20))
    (list
     (list (lambda () (find-file scene-file)) (string-pad "Edit scene" width))
     (list (lambda () (twine--refresh-scene prev-state)) (string-pad "Refresh scene" width))
     (list (lambda () (dired (twine--sget :game-dir))) (string-pad "Open game directory" width))
     (list (lambda () (find-file (twine--sget :game-file-name))) (string-pad "Edit game file" width))
     (list (lambda () (twine--refresh-game)) (string-pad "Refresh game" width))
     (list (lambda () (find-file notes-file)) (string-pad "Game notes" width)))))


(defun twine--get-header-text (header)
  (cadr header))

(defun twine--get-title-text (title)
  (cadr title))

(defun twine--get-title-author (title)
  (caddr title))

(defun twine--get-text (text)
  (cadr text))

(defun twine--get-option-text (option)
  (caddr option))

(defun twine--get-option-scene-name (option)
  (cadr option))

(defun twine--get-doption-action (doption)
  (car doption))

(defun twine--get-doption-text (doption)
  (cadr doption))

;; ??twine--extract-passage: scene name -> passage
;; twine--extract-passage-from-scene: scene -> passage
;; twine--classify-blocks: passage -> blocks in the passage

(defun twine--extract-passage (scene-name state)
  (let* ((scenes (twine--sget :scenes))
         (scene (cdr (assq scene-name scenes))))
    (twine--extract-passage-from-scene scene state)))

(defun twine--normalize-block (block)
  (if (stringp block) `(text ,block) block))

(defun twine--eval (exp state)
  (let ((env `((*state* . ,state))))
    (eval `(cl-flet ((get (sym) (gethash sym *state*))
                     (put (sym val) (puthash sym val *state*)))
             ,exp)
          env)))

;; Go through the body, evaluate everythng that needs to be evaluated, and
;; return the linearization - header, paragraphs of text, options.
(defun twine--extract-passage-from-scene (scene state)
  (let* ((state state)
         (passage nil)
         (block nil)
         (tag nil))
    (catch 'exit
      (while scene
        (setq block (twine--normalize-block (car scene)))
        (setq tag (car block))
        (cl-case tag
          ((text) (let ((params (mapcar (lambda (x) (twine--eval x state)) (cddr block))))
                    (push `(text ,(apply 'format (cons (cadr block) params))) passage)))
          ((title) (push block passage))
          ((header) (let ((params (mapcar (lambda (x) (twine--eval x state)) (cddr block))))
                      (push `(header ,(apply 'format (cons (cadr block) params))) passage)))
          ((option) (let ((params (mapcar (lambda (x) (twine--eval x state)) (cdddr block))))
                      (push `(option ,(cadr block) ,(apply 'format (cons (caddr block) params))) passage)))
          ((when)
           (when (twine--eval (cadr block) state)
             (let ((subpassage (twine--extract-passage-from-scene (cddr block) state)))
               (setq passage (append (nreverse subpassage) passage)))))
          ((jump)
           (let ((subpassage (twine--extract-passage (cadr block) state)))
             (setq passage (append (nreverse subpassage) passage))
             (throw 'exit t)))
          ((eval) (let ((st (twine--eval `(progn ,@(cdr block) *state*) state)))
                    (setq state st)))
          (t (push `(text ,(format "ERROR: unrecognized block tag %s" tag)) passage) (throw 'exit t)))
        (setq scene (cdr scene))))
    (nreverse passage)))

(defun twine--extract-blocks-by-tag (blocks tag)
  (seq-filter (lambda (block) (eq (car block) tag)) blocks))

(defun twine--extract-header-blocks (blocks)
  (twine--extract-blocks-by-tag blocks 'header))

(defun twine--extract-title-blocks (blocks)
  (twine--extract-blocks-by-tag blocks 'title))

(defun twine--extract-text-blocks (blocks)
  (twine--extract-blocks-by-tag blocks 'text))

(defun twine--extract-option-blocks (blocks)
  (twine--extract-blocks-by-tag blocks 'option))

(defun twine--fill-paragraphs (body)
  (with-temp-buffer
    (text-mode)
    (dolist (p body)
      (insert (format "%s\n\n" (twine--get-text p))))
    (fill-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))


(provide 'twine)

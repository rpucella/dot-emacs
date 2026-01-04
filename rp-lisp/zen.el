;;; zen.el --- Note-taking environment     -*- lexical-binding: t -*-



;; Customization variables

(defvar zen-root-directory "~/.notes")
(defvar zen-export-directory "~/Desktop")
(defvar zen-pinned-tags '("PIN"))
(defvar zen-highlighted-tags '("JOT" "SCRATCH"))
(defvar zen-note-symbol "≻")  ; Nice choices: * ⊳  ≻  ►
(defvar zen-max-jot-title 60)
(defvar zen-home-notebook "HOME")
(defvar zen-default-extension "md") ; Default extension for new notes.
(defvar zen-trash-notebook "_trash") ; Trash notebook.
(defvar zen-time-format "%y/%m/%d %A")
(defvar zen-today-note-title "TODAY")


;; Zen mode

(define-derived-mode zen-mode
  special-mode "Zen"
  "Major mode for managing text notes.")

(define-key zen-mode-map (kbd "o") #'zen-open-notebook)
(define-key zen-mode-map (kbd "c") #'zen-create-note)
(define-key zen-mode-map (kbd "s") #'zen-grep)
(define-key zen-mode-map (kbd "/") #'zen-nv)

(defun zen ()
  (interactive)
  (let* ((name "Zen Notes")
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zen-mode)
    (setq-local revert-buffer-function (lambda (&rest ignore) (zen--render)))
    (setq-local zen--default-mode-line-buffer-identification mode-line-buffer-identification)
    (button-mode)
    (defstate **state** (:notebook) #'zen--render)
    (setstate :notebook zen-home-notebook)))

;; Do we have the notion of derived state, where some state properties get
;; automatically recomputed when other properties change?
;; We don't even track which state properties are changed by an update.

(defun zen--render ()
  (let* ((inhibit-read-only t)
         (notebook (getstate :notebook))
         (notes (zen--load-notes notebook))
         (pinned-notes (seq-filter (lambda (note) (eq (plist-get note :class) :pinned)) notes))
         (highlighted-notes (seq-filter (lambda (note) (eq (plist-get note :class) :highlighted)) notes))
         (regular-notes (seq-filter (lambda (note) (eq (plist-get note :class) :regular)) notes)))
  (erase-buffer)
  (setq-local mode-line-buffer-identification
              (append zen--default-mode-line-buffer-identification (list (format "[%s]" notebook))))
  (insert (format "\nNotebook: %s\n" notebook))
  (dolist (sub-notes (list pinned-notes highlighted-notes regular-notes))
    (when sub-notes
      (setq seen-one t)
      (insert "\n")
      (dolist (note sub-notes)
        (zen--render-note note))))
  (goto-char (point-min))
  (when seen-one (forward-button 1))))


(defun zen--render-note (note)
  "Render a note as a text button opening the note."
  (let* ((start-button (point))
         (start-tag nil)
         (end-tag nil))
    (insert (format "%s  " zen-note-symbol))
    (setq start-tag (point))
    (setq end-tag (point))
    (when (and (plist-get note :tag) (not (member (plist-get note :tag) zen-pinned-tags)))
      (insert (plist-get note :tag))
      (setq end-tag (point))
      (insert " - "))
    (insert (plist-get note :title))
    (make-button start-button (point)
                 'type 'zen--note-button
                 'note note)
    (add-face-text-property start-tag end-tag font-lock-function-name-face)
    (insert "\n")))


(define-button-type 'zen--note-button
  'face nil
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map button-map)
            (define-key map (kbd "e") #'zen-export-note)
            (define-key map (kbd "f") #'zen-name-note)
            (define-key map (kbd "k") #'zen-delete-note)
            (define-key map (kbd "m") #'zen-move-note)
            map)
  'action (lambda (btn) (zen-open-note (button-get btn 'note))))


(defun zen-open-note (note)
  (interactive (list (zen--note-at-point)))
  (let ((buff (find-file-noselect (plist-get note :path))))
    (pop-to-buffer buff)
    (when (eq (plist-get note :type) :markdown)
      (markdown-mode))
    (when (eq (plist-get note :type) :org)
      (org-mode))
    (when (fboundp 'wc-mode) (wc-mode))
    (when (fboundp 'auto-fill-mode) (auto-fill-mode))
    ;; Add local hook to possibly rename after saving.
    ;; (add-hook 'after-save-hook 'zen--rename-buffer-file-if-needed nil t)
    ))


(defun zen-create-note (title)
  "Create a note in current notebook (or home notebook if not in a zen buffer)."
  (interactive (list (let ((default (zen--default-title)))
                       (read-string (format "Title (%s): " default) nil nil default))))
  (let* ((fname (zen--fresh-name))
         (notebook (zen--defaulted-notebook))
         (new-file (zen--note-path notebook fname))
         (new-note (list :path new-file))
         (buff (get-file-buffer new-file)))
    ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
    ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (if (null buff)
        (progn
          (zen-open-note new-note)
          (newline)
          ;; This depends on type, no?
          (insert (format "# %s" title))
          (newline)
          (newline))
      (pop-to-buffer buff))))


(defun zen-delete-note ()
  "Delete note under point."
  (interactive)
  (let* ((note (zen--note-at-point))
         (prompt nil)
         (new-path nil))
    (setq prompt (format "Delete note [%s]? " (plist-get note :title)))
    (when (yes-or-no-p prompt)
      ;; Move it to the trash folder.
      (setq new-path (zen--note-path zen-trash-notebook (plist-get note :raw)))
      (rename-file (plist-get note :path) new-path)
      (refstate))))


(defun zen-name-note ()
  "Show the full path of note under point."
  (interactive)
  (let* ((note (zen--note-at-point)))
    (message (plist-get note :path))))


(defun zen-move-note ()
  "Move note under point to notebook."
  (interactive)
  (let* ((note (zen--note-at-point))
         (target-notebook nil)
         (new-path nil))
    (setq target-notebook (zen--query-notebook "Target notebook: "))
    (setq new-path (zen--note-path target-notebook (plist-get note :raw)))
    (rename-file (plist-get note :path) new-path)
    (refstate)))


(defun zen-export-note ()
  "Export (copy) note under point to export folder."
  (interactive)
  (let* ((note (zen--note-at-point))
         (full-title (plist-get note :full-title))
         (default-name (format "%s.%s" full-title (zen--note-extension note)))
         (name (read-string (format "Export name [%s]: " default-name) nil nil default-name)))
    (copy-file (plist-get note :path) (zen--concat-path zen-export-directory name))))


(defun zen-open-notebook (notebook)
  (interactive (list (zen--query-notebook "Open notebook: ")))
  (setstate :notebook notebook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zen NV Search mode

(define-derived-mode zen-nv-mode
  special-mode "Zen NV"
  "Major mode for searching Zen note titles.")

(mapcar (lambda (c)
          (let* ((s (char-to-string c))
                 (f (lambda () (interactive) (zen--add-nv-search s))))
            (define-key zen-nv-mode-map (kbd s) f)))
        "abcdefghijklmnopqrstuvwxyz0123456789.,;:-+=()/")
(define-key zen-nv-mode-map (kbd "<SPC>") (lambda () (interactive) (zen--add-nv-search " ")))
(define-key zen-nv-mode-map (kbd "<backspace>") (lambda () (interactive) (zen--remove-nv-search)))


(defun zen-nv ()
  (interactive)
  (let* ((name "Zen NV")
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zen-nv-mode)
    ;; Maybe setq the initial notes so that we don't have to reload every time?
    ;; Two levels of state? Heavy, light?
    (setq-local revert-buffer-function (lambda (&rest ignore) (zen--nv-render)))
    (button-mode)
    (defstate **state** (:search-string) #'zen--nv-render)
    (setstate :search-string "")))


(defun zen--nv-render ()
  (let* ((inhibit-read-only t)
         (notes-map (zen--load-all-notes))
         (search-string (getstate :search-string))
         (seen-one nil)
         (notebook nil)
         (subnotes nil))
    (erase-buffer)
    (insert (format "\nNV Search => %s" search-string))
    (setq cursor (point))
    (insert "\n")
    (dolist (notebook-content notes-map)
      (setq notebook (car notebook-content))
      (setq subnotes (zen--filter-notes (cdr notebook-content) search-string))
      (when subnotes
        (setq seen-one t)
        (insert (format "\n%s\n\n" notebook))
        (dolist (note subnotes)
          (zen--render-note note))))
    (goto-char cursor)))


(defun zen--filter-notes (notes str)
  (seq-filter (lambda (note)
                (string-match-p (regexp-quote str) (downcase (plist-get note :full-title))))
              notes))


(defun zen--add-nv-search (str)
  (let* ((search-string (getstate :search-string)))
    (setstate :search-string (concat search-string str))))


(defun zen--remove-nv-search ()
  (let* ((search-string (getstate :search-string))
         (len (length search-string)))
    (when (> len 0)
      (setstate :search-string (substring search-string 0 (- len 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zen Grep Search mode

(define-derived-mode zen-grep-mode
  special-mode "Zen Grep"
  "Major mode for searching Zen notes via grep.")


(defun zen-grep (search-string)
  (interactive (list (read-string "Search string: ")))
  (let* ((name (format "Zen Grep: %s" search-string))
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zen-grep-mode)
    ;; Maybe setq the initial notes so that we don't have to reload every time?
    ;; Two levels of state? Heavy, light?
    (setq-local revert-buffer-function (lambda (&rest ignore) (zen--grep-render)))
    (button-mode)
    (defstate **state** (:search-string) #'zen--grep-render)
    (setstate :search-string search-string)))


(defun zen--grep-render ()
  (let* ((inhibit-read-only t)
         (notes-map (zen--load-all-notes))
         (search-string (getstate :search-string))
         (seen-one nil)
         (notebook nil)
         (subnotes nil)
         (start-color nil)
         (end-color nil)
         (buffer (current-buffer)))
    (erase-buffer)
    (insert (format "\nSearch: %s\n" search-string))
    (dolist (notebook-content notes-map)
      (setq notebook (car notebook-content))
      (setq subnotes-with-matches (zen--find-grep-matches search-string notebook (cdr notebook-content)))
      (when subnotes-with-matches
        (insert (format "\n%s\n\n" notebook))
        (dolist (note-with-matches subnotes-with-matches)
          (setq seen-one t)
          (zen--render-note (car note-with-matches))
          (dolist (match (cdr note-with-matches))
            (setq start-color (point))
            (insert (format "   %s" match))
            (add-face-text-property start-color (point) font-lock-comment-face)
            (newline)))))
    (goto-char (point-min))
    (when seen-one (forward-button 1))))


(defun zen--find-grep-matches (search-string notebook notes)
  (let* ((result nil)
         (matches nil)
         (line nil))
    (with-temp-buffer
      (save-match-data
        ;; Note safe if we have special characters!
        (shell-command (format "grep -i %s %s/Z-*.{txt,md,org}(N)"
                               search-string
                               (zen--notebook-path notebook))
                       (current-buffer))
        (dolist (note notes)
          (goto-char (point-min))
          (setq matches nil)
          (while (not (eobp))
            (setq line (setq line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            (when (string-prefix-p (plist-get note :path) line)
              (let ((sz (+ (length (plist-get note :path)) 1)))
                (push (substring line sz (min (length line) (+ sz (window-body-width) -4))) matches)))
            (forward-line 1))
          (when matches
            (push (cons note (nreverse matches)) result)))
        (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions

(defvar zen--note-filter
  (rx string-start
      (or "Z-" "zen-")
      (one-or-more (not (any "-"))) ;; ID
      "-"
      (zero-or-more alnum) ;; "Free" text
      (or ".txt" ".md")
      string-end))


(defun zen--load-notes (notebook)
  (let* ((path (zen--notebook-path notebook))
         (raw-notes (directory-files (directory-file-name path) nil zen--note-filter t))
         (notes (mapcar (zen--process-note notebook) raw-notes)))
    (sort notes (lambda (x y) (string-lessp (plist-get x :sort) (plist-get y :sort))))))


(defun zen--process-note (notebook)
  (lambda (raw-note)
    (let* ((full-title (or (zen--note-title notebook raw-note) raw-note))
           (title-tag (zen--split-title-tag full-title))
           (tag (car title-tag))
           (title (cdr title-tag))
           (path (zen--note-path notebook raw-note))
           (class (zen--note-class tag))
           (type (zen--note-type raw-note)))
      (list :raw raw-note
            :title title
            :tag tag
            :full-title full-title
            :sort (downcase full-title)
            :path path
            :class class
            :type type))))


(defun zen--load-all-notes ()
  (let* ((notebooks (zen--load-notebooks))
         (result nil))
    (message "Loading notes...")
    (dolist (notebook notebooks)
      (push (cons notebook (zen--load-notes notebook)) result))
    (message nil)
    (nreverse result)))

(defun zen--notebook-path (notebook)
  (expand-file-name (zen--concat-path zen-root-directory notebook)))


(defun zen--note-path (notebook raw-note)
  (expand-file-name (zen--concat-path zen-root-directory notebook raw-note)))


(defun zen--note-class (tag)
  (cond ((member tag zen-pinned-tags) :pinned)
        ((member tag zen-highlighted-tags) :highlighted)
        (t :regular)))


(defun zen--split-title-tag (title)
  "Extract tag from TITLE of a note."
  (let* ((case-fold-search nil)
         (tag nil))
    (save-match-data
      (setq tag (and (string-match "^\\([A-Z0-9 ]+\\) - " title)
                     (match-string 1 title)))
      (if tag
          (cons tag (substring title (+ (length tag) 3))) ;; to account for " - "
        (cons nil title)))))


(defvar zen--title-rx (list :markdown (rx string-start "# " (group (zero-or-more not-newline)) string-end)
                            :text (rx string-start (group (zero-or-more not-newline)) string-end)
                            :org (rx string-start "* " (group (zero-or-more not-newline)) string-end)))

(defun zen--note-title (notebook raw-note)
  "Get title of a RAW-NOTE depending on note file type."
  (let* ((path (zen--note-path notebook raw-note))
         (type (zen--note-type raw-note))
         (title (zen--read-first-matching-line path (plist-get zen--title-rx type))))
    (string-trim (or title (format "{%s}" raw-note)))))


(defun zen--note-type (raw-note)
  "Get note file type."
  (cond ((string-suffix-p ".md" raw-note) :markdown)
        ((string-suffix-p ".org" raw-note) :org)
        ;; Eventually, this is different...
        ((string-suffix-p ".txt" raw-note) :markdown)
        (t :text)))


(defun zen--note-extension (note)
  (let* ((type (zen--note-type (plist-get note :raw))))
    (pcase type
      (:markdown "md")
      (:text "txt")
      (:org "org"))))


(defun zen--read-first-matching-line (path regexp)
  "Get first line of a note matching the given regexp."
  (with-temp-buffer
    (save-match-data
      (insert-file-contents-literally path)
      (let ((result nil)
            (line nil))
        (while (and (not result) (not (eobp)))
          (setq line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (and (not (string-empty-p line))
                     (string-match regexp line))
            (setq result (match-string 1 line)))
          (forward-line 1))
        result))))


(defun zen--load-notebooks ()
  "Get the notebooks in the zen root directory, excluding hidden and _ notebooks."
  (let* ((result nil)
         (files (directory-files-and-attributes zen-root-directory nil))
         (name nil)
         (is-dir nil))
    (dolist (file files)
      (setq name (car file))
      (setq is-dir (cadr file))
      (when (and is-dir (not (string-prefix-p "." name)) (not (string-prefix-p "_" name)))
        (push name result)))
    (nreverse result)))


(defun zen--query-notebook (prompt)
  (completing-read prompt (zen--load-notebooks) nil t))


(defun zen--default-title ()
  (format "SCRATCH - %s" (format-time-string zen-time-format)))


(defun zen--in-buffer-p ()
  (eq major-mode 'zen-mode))


(defun zen--fresh-name ()
  (format "Z-%s-.%s" (zen--random-uuid) zen-default-extension))


(defun zen--manual-random-uuid ()
  ;; Code here by Christopher Wellons, 2011-11-18.
  ;; Edited Hideki Saito further to generate all valid variants
  ;; for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
  (let* ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                             (user-uid)
                             (emacs-pid)
                             (system-name)
                             (user-full-name)
                             (current-time)
                             (emacs-uptime)
                             (garbage-collect)
                             (buffer-string)
                             (random)
                             (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring myStr 0 8)
            (substring myStr 8 12)
            (substring myStr 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring myStr 17 20)
            (substring myStr 20 32))))


(defun zen--random-uuid ()
  "Returns a UUID - calls “uuidgen” on MacOS, Linux, and PowelShell on Microsoft Windows."
  (let* ((uuid
          (cond
           ((string-equal system-type "windows-nt")
            (string-trim (shell-command-to-string rp/powershell-random-uuid)))
           ((string-equal system-type "darwin") ; Mac
            (string-trim (shell-command-to-string "uuidgen")))
           ((string-equal system-type "gnu/linux")
            (string-trim (shell-command-to-string "uuidgen")))
           (t (zen--manual-random-uuid)))))
    ;; Remove - and force uppercase.
    (upcase (replace-regexp-in-string (regexp-quote "-") "" uuid))))


(defun zen--defaulted-notebook ()
  "Get current notebook name if in a Zen buffer, default notebook otherwise."
  (if (zen--in-buffer-p)
      (getstate :notebook)
    zen-home-notebook))


(defun zen--note-at-point ()
  "Return the note at point, error out if none."
  (let ((btn (button-at (point))))
    (unless (and btn (button-get btn 'note))
      (error "not over a note button"))
    (button-get btn 'note)))


(defun zen--concat-path (&rest names)
  ;; Recursive, so don't use too deeply...
  (cond ((null names) "")
        ((null (cdr names)) (car names))
        (t (concat (file-name-as-directory (car names)) (apply #'zen--concat-path (cdr names))))))


;;
;; TODO:
;;
;; 1. ~~coloring~~
;; 2. ~~open notebooks~~
;; 3. ~~create note~~
;; 4. nv search (title) "/"
;; 5. grep search (body) "s"
;; 6. jot note "j"
;; 7. coalesce jot "J"
;; 8. open dired
;; 9. today note
;;

;;; zen.el --- Note-taking environment     -*- lexical-binding: t -*-
;;;
;;; This replaces the older Zweirn system.


;; Customization variables

(defvar zen-root-directory "~/.notes")
(defvar zen-export-directory "~/Desktop")
(defvar zen-note-symbol "≻")  ; Nice choices: * ⊳  ≻  ►
(defvar zen-max-jot-title 60)
(defvar zen-home-notebook "HOME")
(defvar zen-default-extension "md") ; Default extension for new notes.
(defvar zen-trash-notebook "_trash") ; Trash notebook.
(defvar zen-time-format "%y/%m/%d %A")
(defvar zen-classes '("pin" "jot" "now"))

;; Path is a list of "bookmarks" of the form (name-symbol . note-path).
;; you can open a bookmark by binding a key to a call to zen-open-bookmark
;; and passing the bookmark name.
(defvar zen-bookmarks nil)


;; Zen mode

(define-derived-mode zen-mode
  special-mode "Zen"
  "Major mode for managing text notes.")

(define-key zen-mode-map (kbd "o") #'zen-open-notebook)
(define-key zen-mode-map (kbd "c") #'zen-create-note)
(define-key zen-mode-map (kbd "j") #'zen-jot-note)
(define-key zen-mode-map (kbd "J") #'zen-coalesce-jots)
(define-key zen-mode-map (kbd "s") #'zen-grep)
(define-key zen-mode-map (kbd "/") #'zen-nv)
(define-key zen-mode-map (kbd "b") #'zen-open-bookmark)
(define-key zen-mode-map (kbd "d") #'zen-open-dired)


(defun zen ()
  (interactive)
  (let* ((name "*Zen*")
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
         (jotted-notes (seq-filter (lambda (note) (eq (plist-get note :class) :jotted)) notes))
         (highlighted-notes (seq-filter (lambda (note) (eq (plist-get note :class) :highlighted)) notes))
         (regular-notes (seq-filter (lambda (note) (eq (plist-get note :class) :regular)) notes)))
  (erase-buffer)
  (setq-local mode-line-buffer-identification
              (append zen--default-mode-line-buffer-identification (list (format "[%s]" notebook))))
  (insert (format "\nNotebook: %s\n" notebook))
  (dolist (sub-notes (list pinned-notes jotted-notes highlighted-notes regular-notes))
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
    (when (plist-get note :tag)
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
         (new-note (list :path new-file)))
    ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (zen-open-note new-note)
    (newline)
    ;; This depends on type, no?
    (insert (format "# %s" title))
    (newline)
    (newline)))


(defun zen-jot-note ()
  "Create a jot note in the home notebook taking initial text from minibuffer."
  (interactive)
  (let* ((fname (zen--fresh-name))
         (notebook (zen--defaulted-notebook))
         (new-file (zen--note-path notebook fname))
         (content (read-string "Jot note: "))
         (title (substring content 0 (min zen-max-jot-title (length content)))))
    ;; See https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect ?
    (with-current-buffer (find-file-noselect new-file)
      (newline)
      ;; This depends on type, no?
      (insert (format "# jot: %s" title))
      (newline)
      (newline)
      (insert (format-time-string zen-time-format))
      (newline)
      (newline)
      (insert content)
      (newline)
      (save-buffer)
      (kill-buffer))
    (when (zen--in-buffer-p)
      (refstate))))


(defun zen-coalesce-jots ()
  "If there are any jotted notes, coalesce them all into a new note and trash originals."
  (interactive)
  (let* ((notebook (getstate :notebook))
         (notes (zen--load-notes notebook))
         (jotted-notes (seq-filter (lambda (note) (eq (plist-get note :class) :jotted)) notes))
         (fname (zen--fresh-name))
         (notebook (zen--defaulted-notebook))
         (new-file (zen--note-path notebook fname)))
    (when (and (not (null jotted-notes))
               (null (get-file-buffer new-file)))
      ;; Only bother if we have jotted notes and the created buffer is not a clash.
      (with-current-buffer (find-file-noselect new-file)
        (insert (format "\n# %s\n\n" (zen--default-title)))
        (dolist (note jotted-notes)
          (insert-file (plist-get note :path))
          ;; What follows depends on the EXACT format of what we put in a jot note!
          ;; Get rid of everything up to the newline before the line with the actual content.
          (kill-line)
          ;; Turn the title into a 2nd degree title preceded by an hrule.
          (insert "***\n\n#")
          (goto-char (point-max))
          (newline)
          (zen--trash-note note))
        (save-buffer)
        (kill-buffer))
      (refstate))))


(defun zen-open-bookmark (bookmark-sym to-bottom)
  "Open a bookmarked note and navigate to the bottom if to-bottom is t."
  (interactive (list (zen--query-bookmark "Bookmark: ") nil))
  (let* ((path (alist-get bookmark-sym zen-bookmarks))
         (buff nil))
    (when (and path (file-exists-p path))
      (setf buff (find-file-noselect path))
      (pop-to-buffer buff)
      ;; Rely on extension/emacs to get the mode right for now.
      ;; TODO: put :type in the bookmark definition?
      (when (fboundp 'wc-mode) (wc-mode))
      (when (fboundp 'auto-fill-mode) (auto-fill-mode))
      ;; Add local hook to possibly rename after saving.
      ;; (add-hook 'after-save-hook 'zen--rename-buffer-file-if-needed nil t)
      (when to-bottom
        (goto-char (point-max))))
    ))


(defun zen--query-bookmark (prompt)
  (let* ((bookmarks (mapcar #'car zen-bookmarks)))
    ;; Return a symbol.
    (intern (completing-read prompt bookmarks nil t))))


(defun zen-delete-note ()
  "Delete note under point."
  (interactive)
  (let* ((note (zen--note-at-point))
         (prompt nil)
         (new-path nil))
    (setq prompt (format "Delete note [%s]? " (plist-get note :title)))
    (when (yes-or-no-p prompt)
      (zen--trash-note note)
      (refstate))))


(defun zen--trash-note (note)
  ;; Move it to the trash folder.
  (setq new-path (zen--note-path zen-trash-notebook (plist-get note :raw)))
  (rename-file (plist-get note :path) new-path))


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
(define-key zen-nv-mode-map (kbd "C-g") #'zen-nv-kill) ;; Override usual quit

(defun zen-nv ()
  (interactive)
  (let* ((name "*Zen NV*")
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

(defun zen-nv-kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Zen Grep Search mode

(define-derived-mode zen-grep-mode
  special-mode "Zen Grep"
  "Major mode for searching Zen notes via grep.")


(defun zen-grep (search-string)
  (interactive (list (read-string "Search string: ")))
  (let* ((name (format "*Zen Grep: %s*" search-string))
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
           (class (car title-tag))
           (tag (cadr title-tag))
           (title (caddr title-tag))
           (path (zen--note-path notebook raw-note))
           (class (zen--note-class class))
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



(defun zen--note-class (class)
  ;; TODO: fix this so that we have a sequence of classes specified in zen-classes,
  ;; and just show the buckets in order instead of pre-defining them.
  (cond ((string= class (car zen-classes)) :pinned)          ;; pin:
        ((string= class (cadr zen-classes)) :jotted)         ;; jot:
        ((string= class (caddr zen-classes)) :highlighted)   ;; now:
        (t :regular)))


(defun zen--split-title-tag (title)
  "Extract class and tag from TITLE of a note."
  (let* ((case-fold-search nil)
         (tag nil)
         (class nil))
    (save-match-data
      (setq class (and (string-match "^\\([A-Za-z]+\\): " title)
                       (match-string 1 title)))
      (when (and class (member class zen-classes ))
        (setq title (substring title (+ (length class) 2))))
      (setq tag (and (string-match "^\\([A-Z0-9 ]+\\) - " title)
                     (match-string 1 title)))
      (when tag
        (setq title (substring title (+ (length tag) 3))))
      (list class tag title))))


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


(defun zen-open-dired ()
  "Open dired in the current notebook."
  (interactive)
  (dired (zen--notebook-path (getstate :notebook))))


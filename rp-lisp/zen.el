;;; zen.el --- Note-taking environment     -*- lexical-binding: t -*-


(define-derived-mode zen-mode
  special-mode "Zen"
  "Major mode for managing text notes.")

(defvar zen-root-directory
  (concat (file-name-as-directory (getenv "HOME")) ".notes"))

(defvar zen-export-directory
  (concat (file-name-as-directory (getenv "HOME")) "Desktop"))

(defvar zen-highlighted-tags '("JOT" "SCRATCH" "SCHEDULE"))

(defvar zen-note-symbol "≻")  ; Nice choices: * ⊳  ≻  ►

(defvar zen-max-jot-title 60)

(defvar zen-home-notebook "HOME")

(defvar zen-default-extension "org") ; Default extension for markdown files.

(defvar zen-trash-notebook "_trash") ; Trash notebook.

(defvar zen-time-format "%y/%m/%d %A")


(define-key zen-mode-map (kbd "o") #'zen-open-notebook)
(define-key zen-mode-map (kbd "c") #'zen-create-note)


;; Color buttons - gemini:
;; can font-lock-mode be used to change the color of the text inside buttons defined using the button.el package?
;; what if I want to change the color of only _part_ of the text of a button?

(defun zen ()
  (interactive)
  (let* ((name "*Zen*")
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zen-mode)
    (setq-local revert-buffer-function (lambda (&rest ignore) (zen--render)))
    (setq-local zen--default-mode-line-buffer-identification mode-line-buffer-identification)
    (button-mode)
    ;; Font lock.
    (setq font-lock-defaults '(zen--highlights t))
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
  (insert (format "Notebook: %s\n\n" notebook))
  (dolist (sub-notes (list pinned-notes highlighted-notes regular-notes))
    (when sub-notes
      (zen--render-notes sub-notes)
      (insert "\n")))
  (goto-char (point-min))))


(defun zen--render-notes (notes)
  "Render a list of notes where each note is a button opening the note."
  (dolist (note notes)
    (insert-text-button (format "%s  %s" zen-note-symbol (zen--strip-tag (plist-get note :title) '("PIN")))
                        'face '(:underline nil :inherit default)
                        'action (lambda (btn) (zen--open-note note)))
    (insert "\n")))

(defun zen--open-note (note)
  (let ((buff (find-file-noselect (plist-get note :path))))
    (pop-to-buffer buff)
    (when (eq (plist-get note :type) :markdown)
      (markdown-mode))
    (when (eq (plist-get note :type) :org)
      (org-mode))
    (when (fboundp 'wc-mode) (wc-mode))
    (when (fboundp 'auto-fill-mode) (auto-fill-mode))
    ;; Add local hook to possibly rename after saving.
    (add-hook 'after-save-hook 'zen--rename-buffer-file-if-needed nil t)))


(defun zen--rename-buffer-file-if-needed ()
  "Rename the current buffer file to account for a possible new title."
  ;; TODO: Write me!
  nil)


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
    (sort notes (lambda (x y) (string-lessp (plist-get x :title) (plist-get y :title))))))

(defun zen--process-note (notebook)
  (lambda (note)
    (let* ((title (or (zen--note-title notebook note) note))
           (path (zen--note-path notebook note))
           (class (zen--note-class title))
           (type (zen--note-type note)))
      (list :raw note
            :title title
            :path path
            :class class
            :type type))))

(defun zen--notebook-path (notebook)
  (concat (file-name-as-directory zen-root-directory) notebook))

(defun zen--note-path (notebook note)
  (concat (file-name-as-directory zen-root-directory) (file-name-as-directory notebook) note))

(defun zen--note-class (title)
  (let ((tag (zen--title-tag title)))
    (cond ((member tag '("PIN")) :pinned)
          ((member tag zen-highlighted-tags) :highlighted)
          (t :regular))))

(defun zen--title-tag (title)
  "Extract tag from TITLE of a note."
  (let ((case-fold-search nil))
    ;; Search case insensitively.
    (save-match-data
      (and (string-match "^\\([A-Z0-9 ]+\\) - " title)
           (match-string 1 title)))))

(defun zen--strip-tag (title tags)
  "Strip any leading tag (including the tailing dash) appearing in TAGS from TITLE."
  (let* ((tag (zen--title-tag title)))
    (if (member tag tags)
        (substring title (+ (length tag) 3))
      title)))

(defun zen--note-title (notebook note)
  "Get title of a NOTE (depending on note file type)."
  (let* ((path (zen--note-path notebook note))
         (type (zen--note-type note))
         (title nil))
    (pcase type
      (:markdown
       (setq title (zen--read-first-matching-line path (rx string-start "# " (group (zero-or-more anychar)) string-end))))
      (:text
       (setq title (zen--read-first-matching-line path (rx string-start (group (zero-or-more anychar)) string-end))))
      (:org
       (setq title "org")))
    (string-trim (or title (format "{%s}" note)))))

(defun zen--note-type (note)
  "Get note file type."
  (cond ((string-suffix-p ".md" note) :markdown)
        ((string-suffix-p ".org" note) :org)
        ;; Eventually, this is different...
        ((string-suffix-p ".txt" note) :markdown)
        (t :text)))

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


(defun zen-open-notebook (notebook)
  (interactive (list (completing-read "Open notebook: " (zen--load-notebooks) nil t)))
  (setstate :notebook notebook))


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


(defun zen-create-note (title)
  "Create a note in current notebook (or home notebook if not in a zen buffer)."
  (interactive (list (let ((default (zen--default-title)))
                       (read-string (format "Title (%s): " default) nil nil default))))
  (let* ((fname (zen--fresh-name))
         (notebook (zen--defaulted-notebook))
         (new-file (zen--note-path notebook fname))
         (new-note (list :raw fname
                         :title title
                         :path new-file
                         :class (zen--note-class title)
                         :type (zen--note-type fname)))
         (buff (get-file-buffer new-file)))
    ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
    ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
    (if (null buff)
        (progn
          (zen--open-note new-note)
          (newline)
          ;; This depends on type, no?
          (insert (format "# %s" title))
          (newline)
          (newline))
      (pop-to-buffer buff))))

(defun zen--defaulted-notebook ()
  "Get current notebook name if in a Zen buffer, default notebook otherwise."
  (if (zen--in-buffer-p)
      (getstate :notebook)
    zen-home-notebook))

;;
;; TODO:
;;
;; 1. coloring
;; 2. ~~open notebooks~~
;; 3. ~~create note~~
;; 4. nv search (title)
;; 5. grep search (body)
;; 6. other note actions
;; 7. today note
;;

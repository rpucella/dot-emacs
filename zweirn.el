
(require 'subr-x)
(require 'markdown-mode)
(require 'cl-lib)

;; Zweirn for Zweites Gehirn aka Second Brain


;; TODO: Add cl-defstruct for reasonable things.
;;  cf:  https://nullprogram.com/blog/2018/02/14/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ZWEIRN MODE.

(define-derived-mode zweirn-mode
  special-mode "Zweirn"
  "Major mode for showing quick notes.")

(define-key zweirn-mode-map (kbd "RET") 'zweirn-read-note)
(define-key zweirn-mode-map (kbd "TAB") 'zweirn-move-next-note)
(define-key zweirn-mode-map (kbd "<backtab>") 'zweirn-move-prev-note)
(define-key zweirn-mode-map (kbd "c") 'zweirn-create-note)
(define-key zweirn-mode-map (kbd "d") 'zweirn-delete-note)
(define-key zweirn-mode-map (kbd "e") 'zweirn-export-note)
(define-key zweirn-mode-map (kbd "f") 'zweirn-show-name)
(define-key zweirn-mode-map (kbd "g") 'zweirn-reload)
(define-key zweirn-mode-map (kbd "i") 'zweirn-move-to-inbox)
(define-key zweirn-mode-map (kbd "j") 'zweirn-jot-note)
;;(define-key zweirn-mode-map (kbd "l") 'zweirn-linked-notes)
(define-key zweirn-mode-map (kbd "m") 'zweirn-move-note)
(define-key zweirn-mode-map (kbd "n") 'zweirn-move-next-note)
(define-key zweirn-mode-map (kbd "o") 'zweirn-open-subfolder)
(define-key zweirn-mode-map (kbd "p") 'zweirn-move-prev-note)
(define-key zweirn-mode-map (kbd "q") 'zweirn-kill)
(define-key zweirn-mode-map (kbd "s") 'zweirn-search)
(define-key zweirn-mode-map (kbd "/") 'zweirn-nv-search)

(define-key zweirn-mode-map (kbd "D") 'zweirn-open-dired)
(define-key zweirn-mode-map (kbd "P") 'zweirn-pdf-note)
(define-key zweirn-mode-map (kbd "J") 'zweirn-coalesce-jots)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ZWEIRN-NV MODE.

(define-derived-mode zweirn-nv-mode
  special-mode "Zweirn-NV"
  "Major mode for searching Zweirn notes.")

(define-key zweirn-nv-mode-map (kbd "RET") 'zweirn-nv-read-note)
(define-key zweirn-nv-mode-map (kbd "TAB") 'zweirn-move-next-note)
(define-key zweirn-nv-mode-map (kbd "<backtab>") 'zweirn-move-prev-note)

(define-key zweirn-nv-mode-map (kbd "a") (lambda () (interactive) (zweirn--add-to-search "a")))
(define-key zweirn-nv-mode-map (kbd "b") (lambda () (interactive) (zweirn--add-to-search "b")))
(define-key zweirn-nv-mode-map (kbd "c") (lambda () (interactive) (zweirn--add-to-search "c")))
(define-key zweirn-nv-mode-map (kbd "d") (lambda () (interactive) (zweirn--add-to-search "d")))
(define-key zweirn-nv-mode-map (kbd "e") (lambda () (interactive) (zweirn--add-to-search "e")))
(define-key zweirn-nv-mode-map (kbd "f") (lambda () (interactive) (zweirn--add-to-search "f")))
(define-key zweirn-nv-mode-map (kbd "g") (lambda () (interactive) (zweirn--add-to-search "g")))
(define-key zweirn-nv-mode-map (kbd "h") (lambda () (interactive) (zweirn--add-to-search "h")))
(define-key zweirn-nv-mode-map (kbd "i") (lambda () (interactive) (zweirn--add-to-search "i")))
(define-key zweirn-nv-mode-map (kbd "j") (lambda () (interactive) (zweirn--add-to-search "j")))
(define-key zweirn-nv-mode-map (kbd "k") (lambda () (interactive) (zweirn--add-to-search "k")))
(define-key zweirn-nv-mode-map (kbd "l") (lambda () (interactive) (zweirn--add-to-search "l")))
(define-key zweirn-nv-mode-map (kbd "m") (lambda () (interactive) (zweirn--add-to-search "m")))
(define-key zweirn-nv-mode-map (kbd "n") (lambda () (interactive) (zweirn--add-to-search "n")))
(define-key zweirn-nv-mode-map (kbd "o") (lambda () (interactive) (zweirn--add-to-search "o")))
(define-key zweirn-nv-mode-map (kbd "p") (lambda () (interactive) (zweirn--add-to-search "p")))
(define-key zweirn-nv-mode-map (kbd "q") (lambda () (interactive) (zweirn--add-to-search "q")))
(define-key zweirn-nv-mode-map (kbd "r") (lambda () (interactive) (zweirn--add-to-search "r")))
(define-key zweirn-nv-mode-map (kbd "s") (lambda () (interactive) (zweirn--add-to-search "s")))
(define-key zweirn-nv-mode-map (kbd "t") (lambda () (interactive) (zweirn--add-to-search "t")))
(define-key zweirn-nv-mode-map (kbd "u") (lambda () (interactive) (zweirn--add-to-search "u")))
(define-key zweirn-nv-mode-map (kbd "v") (lambda () (interactive) (zweirn--add-to-search "v")))
(define-key zweirn-nv-mode-map (kbd "w") (lambda () (interactive) (zweirn--add-to-search "w")))
(define-key zweirn-nv-mode-map (kbd "x") (lambda () (interactive) (zweirn--add-to-search "x")))
(define-key zweirn-nv-mode-map (kbd "y") (lambda () (interactive) (zweirn--add-to-search "y")))
(define-key zweirn-nv-mode-map (kbd "z") (lambda () (interactive) (zweirn--add-to-search "z")))
(define-key zweirn-nv-mode-map (kbd "0") (lambda () (interactive) (zweirn--add-to-search "0")))
(define-key zweirn-nv-mode-map (kbd "1") (lambda () (interactive) (zweirn--add-to-search "1")))
(define-key zweirn-nv-mode-map (kbd "2") (lambda () (interactive) (zweirn--add-to-search "2")))
(define-key zweirn-nv-mode-map (kbd "3") (lambda () (interactive) (zweirn--add-to-search "3")))
(define-key zweirn-nv-mode-map (kbd "4") (lambda () (interactive) (zweirn--add-to-search "4")))
(define-key zweirn-nv-mode-map (kbd "5") (lambda () (interactive) (zweirn--add-to-search "5")))
(define-key zweirn-nv-mode-map (kbd "6") (lambda () (interactive) (zweirn--add-to-search "6")))
(define-key zweirn-nv-mode-map (kbd "7") (lambda () (interactive) (zweirn--add-to-search "7")))
(define-key zweirn-nv-mode-map (kbd "8") (lambda () (interactive) (zweirn--add-to-search "8")))
(define-key zweirn-nv-mode-map (kbd "9") (lambda () (interactive) (zweirn--add-to-search "9")))
(define-key zweirn-nv-mode-map (kbd "<SPC>") (lambda () (interactive) (zweirn--add-to-search " ")))
(define-key zweirn-nv-mode-map (kbd ".") (lambda () (interactive) (zweirn--add-to-search ".")))
(define-key zweirn-nv-mode-map (kbd ",") (lambda () (interactive) (zweirn--add-to-search ",")))
(define-key zweirn-nv-mode-map (kbd ";") (lambda () (interactive) (zweirn--add-to-search ";")))
(define-key zweirn-nv-mode-map (kbd ":") (lambda () (interactive) (zweirn--add-to-search ":")))
(define-key zweirn-nv-mode-map (kbd "-") (lambda () (interactive) (zweirn--add-to-search "-")))
(define-key zweirn-nv-mode-map (kbd "+") (lambda () (interactive) (zweirn--add-to-search "+")))
(define-key zweirn-nv-mode-map (kbd "=") (lambda () (interactive) (zweirn--add-to-search "=")))
(define-key zweirn-nv-mode-map (kbd "(") (lambda () (interactive) (zweirn--add-to-search "(")))
(define-key zweirn-nv-mode-map (kbd ")") (lambda () (interactive) (zweirn--add-to-search ")")))
(define-key zweirn-nv-mode-map (kbd "/") (lambda () (interactive) (zweirn--add-to-search "/")))
(define-key zweirn-nv-mode-map (kbd "<backspace>") (lambda () (interactive) (zweirn--remove-last-from-search)))
(define-key zweirn-nv-mode-map (kbd "C-g") 'zweirn-kill)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODE PARAMETERS.

(defvar zweirn-root-folder
  (concat (file-name-as-directory (getenv "HOME")) ".notes"))

 (defvar zweirn-export-folder
  (concat (file-name-as-directory (getenv "HOME")) "Desktop"))

(defvar zweirn--folder zweirn-root-folder)

(defvar zweirn-special-prefixes '("JOT" "NOTES"))

(defvar zweirn-pin-format "%s")

;;(defvar zweirn-note-symbol "*")
(defvar zweirn-note-symbol "\u227b")
;; Other nice choices:
;;  22b3: ⊳
;;  227b: ≻
;;  25ba: ►

(defvar zweirn-max-jot-title 60)

;; Add to this list to add a new subfolder.
;; Each entry takes the form:
;;    (character-for-subfolder subfolder-name prompt-string true-names)
(defvar zweirn-subfolders
  '((?a "archive" "(a)rchive" nil)
    (?r "reference" "(r)eference" nil)))


;; Default extension for markdown files.
(defvar zweirn-default-extension "txt")


;; Trash folder
(defvar zweirn-trash-folder
  (concat (file-name-as-directory zweirn-root-folder) ".trash"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HELPER FUNCTIONS.

(defun zweirn--create-notes-folder-if-needed ()
  "Create notes folder if it doesn't exist."
  (unless zweirn--is-nonroot
    ;; Only run this is we're in the root folder.
    (progn
      (unless (file-exists-p zweirn-root-folder)
        (make-directory zweirn-root-folder))
      (unless (file-exists-p zweirn-trash-folder)
        (make-directory zweirn-trash-folder))
      (dolist (elt zweirn-subfolders)
        (when (zweirn--is-subfolder (cadr elt))
              (let ((path (concat (file-name-as-directory zweirn-root-folder) (cadr elt))))
                (unless (file-exists-p path)
                  (make-directory path))))))))


(defun zweirn--is-subfolder (path)
  "Check if a path is a subfolder of .notes or a standalone folder."
  (not (string-prefix-p "/" path)))


(defun zweirn--untitled ()
  (format "NOTES - %s" (zweirn--date-tag)))

(defun zweirn--date-tag ()
  (format-time-string "%m/%d/%y %H:%M %A"))

(defun zweirn--open-note-in-markdown (fname)
  (switch-to-buffer (find-file-noselect fname))
  (when (fboundp 'markdown-mode) (markdown-mode))
  (when (fboundp 'wc-mode) (wc-mode))
  (when (fboundp 'auto-fill-mode) (auto-fill-mode)))


(defun zweirn--read-first-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
             unless (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))


(defun zweirn--read-first-non-empty-line (file)
  "Return first non-empty line of FILE." 
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((result nil))
      (while (and (not result) (not (eobp)))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (unless (string-empty-p line)
            (setq result line)))
        (forward-line 1))
      (or result ""))))


(defun zweirn--current-name ()
  "Return the filename of the note on the current line."
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (note-name-regexp (rx string-start
                               (eval zweirn-note-symbol)
                               "[["
                               (group (zero-or-more (or (not (any "]"))
                                                        (seq "]" (not (any "]")))))
                                      (? "]")
                                      (or ".txt" ".md"))
                               "]]")))
    (save-match-data
      (and (string-match note-name-regexp line)
           (match-string 1 line)))))


(defun zweirn--note-path (nt)
  (concat (file-name-as-directory zweirn--folder) nt))

(defun zweirn--export-path (n)
  (concat (file-name-as-directory zweirn-export-folder) n))

(defun zweirn--subfolder-note-path (subfolder n)
  (concat (file-name-as-directory subfolder) n))

(defun zweirn--subfolder-path (name)
  (if (zweirn--is-subfolder name)
      (concat (file-name-as-directory zweirn-root-folder) name)
    name))


(defun zweirn--clean-title (str)
  ;; mostly from https://github.com/kaushalmodi/ox-hugo/blob/e42a824c3253e127fc8b86a5370c8d5b96a45166/ox-hugo.el#L1816-L1886
  (let* (;; Convert to lowercase.
         (str (downcase str))
         ;; Replace any non-alpha char by space.
         (str (replace-regexp-in-string "[^[:alnum:]]" " " str))
         ;; On emacs 24.5, multibyte punctuation characters like "："
         ;; are considered as alphanumeric characters! Below evals to
         ;; non-nil on emacs 24.5:
         ;;   (string-match-p "[[:alnum:]]+" "：")
         ;; So replace them with space manually.
         (str (if (version< emacs-version "25.0")
                  (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                    (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
                str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str)))
    str))


(defun zweirn--query-subfolder (prompt-string)
  (let* ((allowed-inputs (mapcar #'car zweirn-subfolders))
         (prompt (mapconcat #'identity (mapcar #'caddr zweirn-subfolders) " "))
         (subfolder-key (read-char-choice (format "%s %s? " prompt-string prompt) allowed-inputs))
         (target (assoc subfolder-key zweirn-subfolders)))
    target))


(defun zweirn--note-title (nt)
  (let ((line (zweirn--read-first-non-empty-line
               (concat (file-name-as-directory zweirn--folder) nt))))
    (if (string-prefix-p "# " line)
        (zweirn--strip-header line)
      (format "[ %s ]" nt))))


(defun zweirn--notes-by-update-time (path)
  (let* ((filter (rx string-start
                     alnum
                     (zero-or-more (or
                                    (not (any "]"))
                                    (seq "]" (not (any "]")))))
                     (? "]")
                     (or ".txt" ".md")
                     string-end))   ;; any file *.txt|md without two ]] in the name
         (notes (directory-files-and-attributes (directory-file-name path) nil filter t))
         (notes (sort notes (lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
    (mapcar #'car notes)))

;; Can probably rationalize how to pull notes with titles.
;;   Represent as a vector [note title]?
;; More generally, make this kind of processing cleaner. This code is a mess.

(defun zweirn--sort-by-title (notes)
  (let* ((notes-with-titles (mapcar (lambda (nt) (cons nt (zweirn--note-title nt))) notes))
         (sorted (sort notes-with-titles (lambda (x y) (string-lessp (cdr x) (cdr y))))))
    (mapcar #'car sorted)))

(defun zweirn--is-pin (title)
  ;; (and (string-prefix-p ">> " title) (string-suffix-p " <<" title)))
  (string-prefix-p "PIN - " title))

(defun zweirn--is-jot (title)
  (string-prefix-p "JOT - " title))

(defun zweirn--is-special (title)
  (let ((case-fold-search nil))
    ;; Search case insensitively.
    (and (string-match "^\\([A-Z0-9 ]+\\) - " title)
         (member (match-string 1 title) zweirn-special-prefixes))))

(defun zweirn--special-name (title)
  (let* ((case-fold-search nil))
    ;; Search case insensitively.
    (save-match-data
      (and (string-match "^\\([A-Z0-9 ]+\\) - " title)
           (match-string 1 title)))))

(defun zweirn--strip-pin (title)
  (string-trim (substring title 5)))

(defun zweirn--strip-jot (title)
  (string-trim (substring title 5)))

(defun zweirn--pin-notes (notes)
  ;; split notes into pinned/jotted/rest with the first two sorted alphabetically
  (let* ((notes-with-titles (mapcar (lambda (nt) (cons nt (zweirn--note-title nt))) notes))
         (classified-notes (cl-loop for ntt in notes-with-titles
                                    if (zweirn--is-pin (cdr ntt))
                                      collect ntt into pinned
                                    else
                                      if (zweirn--is-special (cdr ntt))
                                        collect ntt into specialed
                                      else 
                                      collect ntt into other
                                      end 
                                    finally return (vector pinned specialed other)))
         (pinned (sort (aref classified-notes 0) (lambda (x y) (string-lessp (cdr x) (cdr y)))))
         (special (sort (aref classified-notes 1) (lambda (x y) (string-lessp (cdr x) (cdr y)))))
         (other (aref classified-notes 2)))
    (vector
     (mapcar #'car pinned)
     (mapcar #'car special)
     (mapcar #'car other))))

(defun zweirn--filter-jotted-notes (notes)
  ;;(message (format "jotted notes = %d" (length notes)))
  (cl-remove-if (lambda (nt) (not (zweirn--is-jot (zweirn--note-title nt)))) notes))

(defun zweirn--pad-right (str width)
  (let* ((fstring (format "%%%ds" (- width))))
    (format fstring str)))

(defun zweirn--show ()
  (let* ((existing-notes (zweirn--notes-by-update-time zweirn--folder))
         ;; Sort alphabetically in the "nonroot folder" case.
         (existing-notes (if zweirn--is-nonroot
                             (vector '() '() (zweirn--sort-by-title existing-notes))
                           (zweirn--pin-notes existing-notes)))
         (notes existing-notes)
         (inhibit-read-only t))
    (erase-buffer)
    (insert "Notes directory " (file-name-as-directory zweirn--folder))
    (newline)
    (newline)
    (when (aref notes 0)
      (let* ((notes (aref notes 0))
             (pinned-notes (mapcar (lambda (nt) (vector nt (zweirn--strip-pin (zweirn--note-title nt)))) notes))
             (width 0))
        (dolist (ntt pinned-notes)
          (let* ((w (length (aref ntt 1))))
            (if (> w width)
                (setq width w))))
        (dolist (ntt pinned-notes)
          (let* ((nt (aref ntt 0))
                 (title (format zweirn-pin-format (zweirn--pad-right (aref ntt 1) width))))
            (insert zweirn-note-symbol (propertize (concat "[[" nt "]]") 'invisible t) "  ")
            (insert title)
            (newline)))
        (newline)))
    (when (aref notes 1)
      (let ((ht (zweirn--classify-specialed-notes (aref notes 1))))
        (maphash (lambda (key nts) (zweirn--show-notes nts)) ht)
        (newline)))
    (dolist (nt (aref notes 2))
      (let ((title (zweirn--note-title nt)))
        (insert zweirn-note-symbol (propertize (concat "[[" nt "]]") 'invisible t) "  ")
        (insert title)
        (newline))))
  (goto-char (point-min))
  (zweirn-move-next-note))

(defun zweirn--show-notes (notes)
   (dolist (nt notes)
    (let* ((title (zweirn--note-title nt)))
      (insert zweirn-note-symbol (propertize (concat "[[" nt "]]") 'invisible t) "  ")
      (insert title)
      (newline))))

(defun zweirn--classify-specialed-notes (notes)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (nt notes)
      (let* ((title (zweirn--note-title nt))
             (name (zweirn--special-name title))
             (curr (gethash name ht)))
        (puthash name (cons nt curr) ht)))
    ht))

(defun zweirn--strip-header (s)
  (let ((header-regexp (rx string-start
                           (zero-or-more space)
                           (optional
                            (one-or-more "#")
                            (one-or-more space))
                           (group (zero-or-more not-newline))
                           string-end)))
    (and (string-match header-regexp s)
         (match-string 1 s))))


(defun zweirn-refresh-name (nt)
  "Rename file `nt` in the current zweirn--folder to a new UUID"
  (let* ((name (zweirn--fresh-name)))
    (rename-file (zweirn--note-path nt) (zweirn--note-path name))))

(defun zweirn-refresh-current-name ()
  "Rename file `nt` in the current zweirn--folder to a new UUID"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (prompt (concat "Refresh note name? " title)))
          (when (yes-or-no-p prompt)
            (zweirn-refresh-name nt)
            (zweirn--show)))
      (message "Cursor not over a note"))))

(defun zweirn--fresh-name ()
  (format "Z-%s.%s" (zweirn--random-uuid) zweirn-default-extension))

(defun zweirn--random-uuid ()
  "Returns a UUID - calls “uuidgen” on MacOS, Linux, and PowelShell on Microsoft Windows."
  ;; TODO: add Z- in front of it,and use this new name - the Z is not part
  (cond
   ((string-equal system-type "windows-nt")
    (string-trim (shell-command-to-string rp/powershell-random-uuid)))
   ((string-equal system-type "darwin") ; Mac
    (string-trim (shell-command-to-string "uuidgen")))
   ((string-equal system-type "gnu/linux")
    (string-trim (shell-command-to-string "uuidgen")))
   (t
    ;; Code here by Christopher Wellons, 2011-11-18.
    ;; Editted Hideki Saito further to generate all valid variants
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
              (substring myStr 20 32))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXPORTED FUNCTIONS.

;; Most of those functions must be called from within a Zweirn folder.
;; Maybe we should protect them with ensure-zweirn-mode which might use
;;   (buffer-local-value 'major-mode (get-buffer "*scratch*"))

(defun zweirn-zweirn-buffer-p ()
  (eq major-mode 'zweirn-mode))

(defun zweirn-create-note ()
  "Create a 'permanent' note in $HOME/.notes"
  (interactive)
  (if (zweirn-zweirn-buffer-p)
      (unless zweirn--is-nonroot
        ;; TODO: The main zweirn function should create the folders as an invariant.
        (zweirn--create-notes-folder-if-needed)
        (let* ((fname (zweirn--fresh-name))
               (new-file (zweirn--note-path fname))
               (buff (get-file-buffer new-file)))
          ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
          ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
          (if (null buff)
              (let* ((default-title (zweirn--untitled))
                     (title (read-string (format "Title (%s): " default-title) nil nil default-title)))
                (zweirn--open-note-in-markdown new-file)
                (newline)
                (insert (format "# %s" title))
                (newline)
                (newline))
            (switch-to-buffer buff))))
    (let ((zweirn--is-nonroot t)
          (zweirn--folder zweirn-root-folder))
      (zweirn--create-notes-folder-if-needed)
      (let* ((fname (zweirn--fresh-name))
             (new-file (zweirn--note-path fname))
             (buff (get-file-buffer new-file)))
        ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
        ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
        (if (null buff)
              (let* ((default-title (zweirn--untitled))
                     (title (read-string (format "Title (%s): " default-title) nil nil default-title)))
                (zweirn--open-note-in-markdown new-file)
                (newline)
                (insert (format "# %s" title))
                (newline)
                (newline))
          (switch-to-buffer buff))))))

(defun zweirn-jot-note ()
  "Create a jot note in the inbox from a minibuffer text input."
  (interactive)
  (let ((zweirn--is-nonroot t)
        (zweirn--folder zweirn-root-folder))
    (zweirn--create-notes-folder-if-needed)
    (let* ((fname (zweirn--fresh-name))
           (new-file (zweirn--note-path fname))
           (content (read-string "Jot note: "))
           (title (substring content 0 (min zweirn-max-jot-title (length content))))
           (buff (get-file-buffer new-file)))
      ;; TODO: if the file/buffer already exists, don't insert the # Note thing.
      ;; Also, see https://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect
      (if (null buff)
          (progn 
            (set-buffer (find-file-noselect new-file))
            (newline)
            (insert (format "# JOT - %s" title))
            (newline)
            (newline)
            (insert (zweirn--date-tag))
            (newline)
            (newline)
            (insert content)
            (newline)
            (save-buffer)
            (kill-buffer))
        (message "Wait... buffer already exists?")))))

(defun zweirn-coalesce-jots ()
  "If there are any jotted notes, coalesce them all into a new note and trash originals."
  (interactive)
  (unless zweirn--is-nonroot
    ;; Only allow this in the root folder.
    (let* ((existing-notes (zweirn--notes-by-update-time zweirn--folder))
           (jotted-notes (zweirn--filter-jotted-notes (aref (zweirn--pin-notes existing-notes) 1))))
      (unless (null jotted-notes)
        ;; Only bother if we have jotted notes.
        (let* ((fname (zweirn--fresh-name))
               (new-file (zweirn--note-path fname))
               (buff (get-file-buffer new-file)))
          (if (null buff)
              (let* ((root zweirn--folder))
                (with-current-buffer (find-file-noselect new-file)
                   ;;(insert (format "\n# Note %s\n\n" (zweirn--date-tag)))
                 (insert (format "\n# %s\n\n" (zweirn--untitled)))
                  (dolist (nt jotted-notes)
                    (let* ((original-file (concat (file-name-as-directory root) nt))
                           (fname (zweirn--fresh-name))
                           (new-file (concat (file-name-as-directory zweirn-trash-folder) fname)))
                      (insert-file original-file)
                      ;; What follows depends on the EXACT format of what we put in a jot note!
                      ;; Get rid of everything up to the newline before the line with the actual content.
                      (kill-line)
                      (kill-line)
                      (kill-line)
                      (kill-line)
                      (kill-line)
                      (kill-line)
                      (goto-char (point-max))
                      (rename-file original-file new-file)))
                  (save-buffer)
                  (kill-buffer))
                (zweirn--show))))))))
  
(defun zweirn-show-name ()
  "Show the name of the file containing the number on the current line."
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (message (concat "Note file: " nt))
      (message "Cursor not over a note"))))


(defun zweirn-read-note ()
  "Load the note pointed to by the point in a *notes* buffer"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (progn 
          (zweirn--open-note-in-markdown
           (concat (file-name-as-directory zweirn--folder) nt)))
      (message "Cursor not over a note"))))

(defun zweirn-nv-read-note ()
  "Load the note pointed to by the point in a zweirn-nv buffer, killing the buffer in the process. Handle direct access notes as well."
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let ((file (if (zweirn--is-subfolder nt)
                        (concat (file-name-as-directory zweirn--folder) nt)
                      nt)))
          (kill-buffer (current-buffer))
          (zweirn--open-note-in-markdown file))
      (message "Cursor not over a note"))))

(defun zweirn-pdf-note ()
  "Convert the note pointed to by the point into PDF using function rp/pdf-markdown"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (progn 
          (rp/pdf-markdown
           (concat (file-name-as-directory zweirn--folder) nt)))
      (message "Cursor not over a note"))))


(defun zweirn-export-note ()
  "Export (copy) the note to export folder"
    (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (name (format "%s.%s" (zweirn--clean-title title) zweirn-default-extension))
               (name (read-string (format "Export name (%s): " name) nil nil name)))
          (copy-file (zweirn--note-path nt) (zweirn--export-path name)))
      (message "Cursor not over a note"))))



(defun zweirn-move-note ()
  "Move the note to subfolder"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (name (format "%s.%s" (zweirn--clean-title title) zweirn-default-extension))
               (target (zweirn--query-subfolder "Move to"))
               (target-subfolder (caddr target))
               (target-true-name (if (> (length target) 3) (cadddr target) nil))
               (target-path (zweirn--subfolder-path (cadr target)))
               ;; If target folder has true names, ask for a name.
               ;; If source folder has true names, we need to create a fresh name.
               (name (if target-true-name
                         (read-string (format "Name (%s): " name target-path) nil nil name)
                       (if zweirn--has-true-name
                           (zweirn--fresh-name)
                         nt))))
          (rename-file (zweirn--note-path nt) (zweirn--subfolder-note-path target-path name))
          (zweirn--show))
      (message "Cursor not over a note"))))


(defun zweirn-move-to-inbox ()
  "Move the note to the inbox, assigning is a new UUID"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (prompt (concat "Move note to inbox? " title)))
          (when (yes-or-no-p prompt)
            (if zweirn--has-true-name
                (let* ((fname (zweirn--fresh-name))
                       (new-file (concat (file-name-as-directory zweirn-root-folder) fname)))
                  (rename-file (zweirn--note-path nt) new-file))
              (rename-file (zweirn--note-path nt) (concat (file-name-as-directory zweirn-root-folder) nt)))
            (zweirn--show)))
      (message "Cursor not over a note"))))


(defun zweirn-delete-note ()
  "Delete a note"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (prompt (concat "Delete note? " title)))
          (when (yes-or-no-p prompt)
            ;; Move it to the trash folder.
            (let* ((fname (zweirn--fresh-name))
                   (new-file (concat (file-name-as-directory zweirn-trash-folder) fname)))
              (rename-file (zweirn--note-path nt) new-file)
              (zweirn--show))))
      (message "Cursor not over a note"))))


(defun zweirn-move-next-note ()
  "Find next note marker in the *notes* buffer"
  (interactive)
  ;; Move forward one (if you're on ^* already...).
  (right-char)
  (let ((r (rx line-start (eval zweirn-note-symbol))))
    (re-search-forward r nil t)
    ;; Move back to * or back to original char if not found.
    (left-char)
    ;; (zweirn-show-name)
    ))


(defun zweirn-move-prev-note ()
  "Find previous note marker in the *notes* buffer"
  (interactive)
  (let ((r (rx line-start (eval zweirn-note-symbol))))
    (re-search-forward r nil t -1)
    ;;;(zweirn-show-name)))
    ))


(defun zweirn-open-dired ()
  "Open dired in the notes folder"
  (interactive)
  (dired zweirn--folder))


(defun zweirn-open-subfolder ()
  "Open subfolder"
  (interactive)
  (let* ((target (zweirn--query-subfolder "Open"))
         (target-path (zweirn--subfolder-path (cadr target)))
         (true-name (if (> (length target) 3) (cadddr target) nil)))
    (zweirn target-path t true-name)))


(defun zweirn-kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))


(defun zweirn-reload ()
  (interactive)
  (zweirn--show))


(defun zweirn (path &optional nonroot true-name)
  "Show list of notes in $HOME/.notes"
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Notes directory: ")
                       nil)))
  ;; A nonroot folder is a non-root notes folder (archive, reference, etc).
  ;; It does not support creating new notes, and sorts alphabetically.
  (let* ((folder (or path zweirn-root-folder))
         (name (concat "*Zweirn* " (file-name-as-directory folder)))
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zweirn-mode)
    (make-local-variable 'zweirn--folder)
    (setq zweirn--folder folder)
    (make-local-variable 'zweirn--is-nonroot)
    (setq zweirn--is-nonroot nonroot)
    (make-local-variable 'zweirn--has-true-name)
    (setq zweirn--has-true-name true-name)
    (zweirn--create-notes-folder-if-needed)
    (zweirn--show)))

(add-hook 'markdown-mode-hook
          ;; When we enter markdown-mode, modify it if it we haven't already.
          (lambda ()
            (unless (member "zweirn" markdown-uri-types)
              ;; Add "zweirn:" as a URI type.
              ;; Annoyingly, looks like we need to reload markdown-mode to enable this change. 
              (add-to-list 'markdown-uri-types "zweirn")
              (load-library "markdown-mode")
              ;; Intercept markdown--browse-url to allow opening a zweirn URI.
              (advice-add 'markdown--browse-url :around
                          (lambda (originalf url)
                            (if (string-prefix-p "zweirn:" url)
                                (message (concat "Not yet implemented: open " url))
                              (funcall originalf url)))))))
              

(defun zweirn-nv-search ()
  (interactive)
  ;; We need to do this work from the root folder.
  (let ((zweirn--folder zweirn-root-folder))
    (let* ((notes (zweirn--notes-by-update-time zweirn-root-folder))
           (notes (zweirn--sort-by-title notes))
           (name "*Zweirn-NV*")
           (buff (get-buffer-create name)))
      (switch-to-buffer buff)
      (zweirn-nv-mode)
      ;; Switching major mode clears the local variables.
      (make-local-variable 'zweirn-nv--search-string)
      (setq zweirn-nv--search-string "")
      (make-local-variable 'zweirn-nv--notes)
      (setq zweirn-nv--notes notes)
      (make-local-variable 'zweirn-nv--subfolder-notes)
      (setq zweirn-nv--subfolder-notes '())
      (dolist (target zweirn-subfolders)
        (let ((zweirn--folder (zweirn--subfolder-path (cadr target))))
          (let* ((notes (zweirn--notes-by-update-time zweirn--folder))
                 (notes (zweirn--sort-by-title notes)))
            (setq zweirn-nv--subfolder-notes (cons (list (cadr target) notes) zweirn-nv--subfolder-notes)))))
      (zweirn--show-nv-search))))

(defun zweirn--show-nv-search ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "==> ")
    (insert zweirn-nv--search-string)
    (newline)
    (newline)
    (let ((seen-one nil))
      (dolist (nt zweirn-nv--notes)
        (let ((title (zweirn--note-title nt)))
          (when (string-match-p (regexp-quote zweirn-nv--search-string) (downcase title))
            (insert zweirn-note-symbol (propertize (concat "[[" nt "]]") 'invisible t) "  ")
            (insert title)
            (newline)
            (setq seen-one t))))
      (dolist (subf-notes zweirn-nv--subfolder-notes)
        (let ((zweirn--folder (zweirn--subfolder-path (car subf-notes))))
          (let ((seen-one-here (zweirn-nv--show-subfolder-notes (car subf-notes) (cadr subf-notes))))
            (setq seen-one (or seen-one seen-one-here)))))
      (goto-char (point-min))
      (if seen-one
          (zweirn-move-next-note)
        (goto-char (point-max))))))

(defun zweirn-nv--show-subfolder-notes (name notes)
  (let ((seen-one nil))
    (dolist (nt notes)
      (let ((title (zweirn--note-title nt))
            ; Put nt as subfolder/nt so that when we open the note it picks it up from the subfolder.
            (path-nt (concat (file-name-as-directory name) nt)))
        (when (string-match-p (regexp-quote zweirn-nv--search-string) (downcase title))
          (unless seen-one
            (newline)
            (insert name)
            (newline)
            (newline))
          (insert zweirn-note-symbol (propertize (concat "[[" path-nt "]]") 'invisible t) "  ")
          (insert title)
          (newline)
          (setq seen-one t))))
    seen-one))
      
(defun zweirn--add-to-search (str)
  (setq zweirn-nv--search-string (concat zweirn-nv--search-string str))
  (zweirn--show-nv-search))

(defun zweirn--remove-last-from-search ()
  (let ((len (length zweirn-nv--search-string)))
    (when (> len 0)
      (setq zweirn-nv--search-string (substring zweirn-nv--search-string 0 (- len 1)))
      (zweirn--show-nv-search))))

(defun zweirn-search (s)
  ;; TODO: We probably need a dedicated mode for this to navigate results.
  (interactive (list (read-string "Search string: ")))
  (let* ((notes (zweirn--notes-by-update-time zweirn--folder))
         (name (concat "*Zweirn-search* " s))
         (buff (get-buffer-create name))
         (seen nil)
         (num-notes 0))
    ;; This rigmarole is needed because zweirn--folder is buffer local,
    ;; so when we switch buffers, we need to save it and use dynamic binding!
    (let ((temp-zweirn-folder zweirn--folder))
      (switch-to-buffer buff)
      (let ((zweirn--folder temp-zweirn-folder))
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (nt notes)
            (setq seen nil)
            (with-temp-buffer
              (insert-file-contents-literally (zweirn--note-path nt))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
                  (when (string-match-p (regexp-quote s) line)
                    (with-current-buffer buff
                      (when (not seen)
                        (when (> num-notes 0)
                          (newline))
                        (let ((title (zweirn--note-title nt)))
                          (insert title))
                        (newline)
                        (cl-incf num-notes)
                        (setq seen t))
                      (insert " " line)
                      (newline)))
                  (forward-line 1))))))))
    (goto-char (point-min))))




;; (defun zweirn--find-all-links (nt)
;;   (let ((link-regexp (rx "[["
;;                          (group (zero-or-more (or (not (any "]"))
;;                                                         (seq "]" (not (any "]"))))))
;;                          "]]"))
;;         (links '()))
;;   (with-temp-buffer
;;     (insert-file-contents-literally (zweirn--note-path nt))
;;     (goto-char (point-min))
;;     (while (progn
;;              (let ((found (re-search-forward link-regexp nil t)))
;;                (when found
;;                  (setq links (cons (match-string 1) links))
;;                  t))))
;;     links)))


;; (defun zweirn--find-link (nt link)
;;   (let ((link-regexp (regexp-quote (concat "[[" link "]]"))))
;;     (with-temp-buffer
;;       (insert-file-contents-literally (zweirn--note-path nt))
;;       (goto-char (point-min))
;;       (if (re-search-forward link-regexp nil t) t nil))))


;; (defun zweirn--find-linking-notes (nt)
;;   (let* ((notes (zweirn--notes-by-update-time zweirn--folder))
;;          (title (zweirn--note-title nt))
;;          (name (concat "*Zweirn Linked: " title "*"))
;;          (buff (get-buffer-create name)))
;;     ;; TODO: Zweirn Linked should probably be a zweirn-mode.
;;     (switch-to-buffer buff)
;;     (setq buffer-read-only t)
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (insert "Notes linking to [[" title "]]")
;;       (newline)
;;       (newline)
;;       (dolist (nt notes)
;;         (when (zweirn--find-link nt title)
;;           (let ((title (zweirn--note-title nt)))
;;             (insert title))
;;           (newline))))
;;     (goto-char (point-min))))


;; (defun zweirn-linked-notes ()
;;   "Find all notes that link to this note"
;;   (interactive)
;;   (let ((nt (zweirn--current-name)))
;;     (if nt
;;         (zweirn--find-linking-notes nt)
;;       (message "Cursor not over a note"))))


(provide 'zweirn)

;;; zweirn.el --- Zweites Gehirn (Second Brain)

;; Copyright (C) 2022  Riccardo Pucella

;; Author: Riccardo Pucella <riccardo@acm.org>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'subr-x)
(require 'markdown-mode)
(require 'cl-lib)

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
;;(define-key zweirn-mode-map (kbd "i") 'zweirn-move-to-inbox)
(define-key zweirn-mode-map (kbd "j") 'zweirn-jot-note)
;;(define-key zweirn-mode-map (kbd "l") 'zweirn-linked-notes)
(define-key zweirn-mode-map (kbd "m") 'zweirn-move-note)
(define-key zweirn-mode-map (kbd "n") 'zweirn-move-next-note)
(define-key zweirn-mode-map (kbd "o") 'zweirn-open-notebook)
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

(defvar zweirn-special-prefixes '("JOT"))

(defvar zweirn-pin-format "%s")

;;(defvar zweirn-note-symbol "*")
(defvar zweirn-note-symbol "\u227b")
;; Other nice choices:
;;  22b3: ⊳
;;  227b: ≻
;;  25ba: ►

(defvar zweirn-max-jot-title 60)

;; Add to this list to add a new notebook.
;; Each entry takes the form:
;;    (character-for-notebook notebook-name prompt-string)

(defvar zweirn-notebooks
  '((?i "inbox" "(i)nbox")
    (?a "archive" "(a)rchive")
    (?r "reference" "(r)eference")))

(defvar zweirn-inbox-notebook "inbox")

;; Default extension for markdown files.
(defvar zweirn-default-extension "txt")


;; Trash folder. Technically a notebook.
(defvar zweirn-trash-notebook "_trash")

;; Assets folder. Technically a notebook, but cannot really be treated as such.
(defvar zweirn-assets-notebook "_assets")

(defvar zweirn-max-image-width 1000)

;; Set this to true to sort inbox by name instad of by last updated.
(defvar zweirn-sort-inbox t)

;; Syntax highlighting.
;;
;; Cf http://xahlee.info/emacs/emacs/elisp_syntax_coloring.html
;;
;; Also inspired by markdown-mode:
;;   https://github.com/jrblevin/markdown-mode/blob/master/markdown-mode.el

(defgroup zweirn-faces nil
  "Faces used in Zweirn Mode"
  :group 'zweirn
  :group 'faces)

(defface zweirn-prefix-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for title prefixes"
  :group 'zweirn-faces)

(defface zweirn-pin-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "Face for pinned notes title"
  :group 'zweirn-faces)

(defun zweirn--highlights ()
  (rx-let ((note-start (seq line-start
                            (eval zweirn-note-symbol)
                            "[["
                            (zero-or-more (or (not (any "]"))
                                              (seq "]" (not (any "]")))))
                            (? "]")
                            (or ".txt" ".md")
                            "]]  ")))
    `((,(rx note-start "PIN" (group (zero-or-more print))) (1 'zweirn-pin-face))
      (,(rx note-start (group (zero-or-more (or upper-case digit))) " - ") (1 'zweirn-prefix-face)))))

(defvar zweirn--buffer-prefix-zweirn "zweirn")
(defvar zweirn--buffer-prefix-nv "zweirn-nv")
(defvar zweirn--buffer-prefix-search "zweirn-search")



;; MAIN ENTRY POINT

(defun zweirn--is-inbox ()
  (equal zweirn--notebook zweirn-inbox-notebook))


(defun zweirn (notebook)
  "Show list of notes in $HOME/.notes"
  ;; Presumably we could point to an arbitrary folder.
  ;; Right now, only allow notebooks = subfolders of the root folder.
  (interactive (list zweirn-inbox-notebook))
  (let* ((folder (zweirn--notebook-path notebook))
         (name (format "%s: %s" zweirn--buffer-prefix-zweirn notebook))
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zweirn-mode)
    ;; Font lock.
    (setq font-lock-defaults '(zweirn--highlights t))
    ;; Buffer local variables.
    (make-local-variable 'zweirn--notebook)
    (make-local-variable 'zweirn--external)
    (setq zweirn--notebook notebook)
    (setq zweirn--external (zweirn--is-external notebook))
    (zweirn--create-notes-folder-if-needed)
    (zweirn--show)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HELPER FUNCTIONS.

(defun zweirn--create-notes-folder-if-needed ()
  "Create notes folder if it doesn't exist."
  (let* ((extra `((nil ,zweirn-trash-notebook nil)
                  (nil ,zweirn-assets-notebook nil)))
         (notebooks (append extra zweirn-notebooks))
         path)
    (when (not (file-exists-p zweirn-root-folder))
      (make-directory zweirn-root-folder))
    (dolist (elt notebooks)
      (when (zweirn--is-internal (cadr elt))
        (setq path (zweirn--notebook-path (cadr elt)))
        (when (not (file-exists-p path))
          (make-directory path))))))


(defun zweirn--is-external (path)
  "Check if a path is an external notebook or under .notes."
  (string-prefix-p "/" path))

(defun zweirn--is-internal (path)
  "Check if a path is an internal notebook under .notes."
  (not (zweirn--is-external path)))


(defun zweirn--untitled ()
  (format "SCRATCH - %s" (zweirn--date-tag)))

(defun zweirn--date-tag ()
  (format-time-string "%y/%m/%d %H:%M %A"))

(defun zweirn--rename-buffer-file-if-needed ()
  "Rename the current buffer file to account for a possible new title.
   Added to after-save-hook in a note buffer"
  ;; TODO: Write me!
  nil)

(defun zweirn--open-note-in-markdown (fname)
  (pop-to-buffer (find-file-noselect fname))
  (when (fboundp 'markdown-mode)
    ;; Enable markdown-mode and add functionality.
    (markdown-mode)
    (when (display-images-p)
      (markdown-display-inline-images))
    (when (not (member "zweirn" markdown-uri-types))
      ;; Add "zweirn:" as a URI type.
      ;; Annoyingly, looks like we need to reload markdown-mode to enable this change. 
      (add-to-list 'markdown-uri-types "zweirn")
      (load-library "markdown-mode")
      ;; Intercept markdown--browse-url to allow opening a zweirn URI.
      (advice-add 'markdown--browse-url :around
                  (lambda (originalf url)
                    (if (string-prefix-p "zweirn:" url)
                        (message (concat "Not yet implemented: open " url))
                      (funcall originalf url))))
      (local-set-key [drag-n-drop] 'zweirn-drag-n-drop-image)))
  (when (fboundp 'wc-mode) (wc-mode))
  (when (fboundp 'auto-fill-mode) (auto-fill-mode))
  ;; Add local hook to possibly rename after saving.
  (add-hook 'after-save-hook 'zweirn--rename-buffer-file-if-needed nil t))



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
          (when (not (string-empty-p line))
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
  (concat (file-name-as-directory (zweirn--notebook-path zweirn--notebook)) nt))

(defun zweirn--note-uuid (nt)
  "Extract the UUID from a note name. Return nil if note has no UUID."
  (let* ((uuid (rx string-start
                   "Z-"
                   (group (one-or-more (any "A-Z" "0-9")))
                   "-"
                   (zero-or-more (not (any ".")))
                   (or ".txt" ".md")
                   string-end)))
    (save-match-data
      (and (string-match uuid nt)
           (match-string 1 nt)))))

(defun zweirn--export-path (n)
  (concat (file-name-as-directory zweirn-export-folder) n))

(defun zweirn--notebook-note-path (notebook n)
  (concat (file-name-as-directory notebook) n))

(defun zweirn--notebook-path (name)
  (if (zweirn--is-internal name)
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


(defun zweirn--query-notebook (prompt-string)
  (let* ((allowed-inputs (mapcar #'car zweirn-notebooks))
         (prompt (mapconcat #'identity (mapcar #'caddr zweirn-notebooks) " "))
         (notebook-key (read-char-choice (format "%s %s? " prompt-string prompt) allowed-inputs))
         (target (assq notebook-key zweirn-notebooks)))
    (message nil)
    target))


(defun zweirn--note-title (nt)
  (let ((line (zweirn--read-first-non-empty-line (zweirn--note-path nt))))
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
    (and (zweirn--is-tagged title)
         (member (match-string 1 title) zweirn-special-prefixes))))

(defun zweirn--is-tagged (title)
  (let ((case-fold-search nil))
    ;; Search case insensitively.
    (string-match "^\\([A-Z0-9 ]+\\) - " title)))

(defun zweirn--special-name (title)
  (let* ((case-fold-search nil))
    ;; Search case insensitively.
    (save-match-data
      (and (zweirn--is-tagged title)
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
                                    else if (zweirn--is-special (cdr ntt))
                                      collect ntt into specialed
                                    else if (zweirn--is-tagged (cdr ntt))
                                      collect ntt into tagged
                                    else
                                      collect ntt into other
                                    end
                                    finally return (vector pinned specialed tagged other)))
         (pinned (sort (aref classified-notes 0) (lambda (x y) (string-lessp (cdr x) (cdr y)))))
         (special (sort (aref classified-notes 1) (lambda (x y) (string-lessp (cdr x) (cdr y)))))
         (tagged (sort (aref classified-notes 2) (lambda (x y) (string-lessp (cdr x) (cdr y)))))
         (other (if zweirn-sort-inbox
                    (sort (aref classified-notes 3) (lambda (x y) (string-lessp (cdr x) (cdr y))))
                  (aref classified-notes 3))))
    (vector
     (mapcar #'car pinned)
     (mapcar #'car special)
     (mapcar #'car tagged)
     (mapcar #'car other))))

(defun zweirn--filter-jotted-notes (notes)
  ;;(message (format "jotted notes = %d" (length notes)))
  (cl-remove-if (lambda (nt) (not (zweirn--is-jot (zweirn--note-title nt)))) notes))

(defun zweirn--pad-right (str width)
  (let* ((fstring (format "%%%ds" (- width))))
    (format fstring str)))

(defun zweirn--show (&optional point)
  (let* ((existing-notes (zweirn--notes-by-update-time (zweirn--notebook-path zweirn--notebook)))
         ;; Sort alphabetically in the "nonroot folder" case.
         (existing-notes (if (not (zweirn--is-inbox))
                             (vector '() '() (zweirn--sort-by-title existing-notes))
                           (zweirn--pin-notes existing-notes)))
         (notes existing-notes)
         (inhibit-read-only t))
    (erase-buffer)
    (insert "Notebook: " zweirn--notebook)
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
            ;; For syntax highlighting!
            (insert (propertize "PIN - " 'invisible t))
            (insert title)
            (newline)))
        (newline)))
    (when (aref notes 1)
      (let ((ht (zweirn--classify-specialed-notes (aref notes 1))))
        ;; Note that -classify-specialed-notes returns reversed lists!
        (maphash (lambda (key nts) (zweirn--show-notes (nreverse nts))) ht)
        (newline)))
    (when (aref notes 2)
      (let ((ht (zweirn--classify-specialed-notes (aref notes 2))))
        ;; Note that -classify-specialed-notes returns reversed lists!
        (maphash (lambda (key nts) (zweirn--show-notes (nreverse nts))) ht)
        (newline)))
    (dolist (nt (aref notes 3))
      (let ((title (zweirn--note-title nt)))
        (insert zweirn-note-symbol (propertize (concat "[[" nt "]]") 'invisible t) "  ")
        (insert title)
        (newline))))
  (font-lock-fontify-buffer)
  (if point
      (goto-char point)
    (goto-char (point-min)))
  (zweirn--snap-to-note))

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
  "Rename file `nt` in the current notebook to a new UUID"
  (let* ((name (zweirn--fresh-name)))
    (rename-file (zweirn--note-path nt) (zweirn--note-path name))))

(defun zweirn-refresh-current-name ()
  "Rename file `nt` in the current notebook to a new UUID"
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
  (format "Z-%s-.%s" (zweirn--random-uuid) zweirn-default-extension))

(defun zweirn--manual-random-uuid ()
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

(defun zweirn--random-uuid ()
  "Returns a UUID - calls “uuidgen” on MacOS, Linux, and PowelShell on Microsoft Windows."
  (let* ((uuid
          (cond
           ((string-equal system-type "windows-nt")
            (string-trim (shell-command-to-string rp/powershell-random-uuid)))
           ((string-equal system-type "darwin") ; Mac
            (string-trim (shell-command-to-string "uuidgen")))
           ((string-equal system-type "gnu/linux")
            (string-trim (shell-command-to-string "uuidgen")))
           (t (zweirn--manual-random-uuid)))))
    ;; Remove - and force uppercase.
    (upcase (replace-regexp-in-string (regexp-quote "-") "" uuid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXPORTED FUNCTIONS.

;; Most of those functions must be called from within a Zweirn folder.
;; Maybe we should protect them with ensure-zweirn-mode which might use
;;   (buffer-local-value 'major-mode (get-buffer "*scratch*"))

(defun zweirn-zweirn-buffer-p ()
  (eq major-mode 'zweirn-mode))

(defun zweirn-create-note ()
  "Create a 'permanent' note in inbox folder"
  (interactive)
  (if (zweirn-zweirn-buffer-p)
      (when (zweirn--is-inbox)
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
            (pop-to-buffer buff))))
    (let ((zweirn--notebook zweirn-inbox-notebook))
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
          (pop-to-buffer buff))))))

(defun zweirn-jot-note ()
  "Create a jot note in the inbox from a minibuffer text input."
  (interactive)
  (let ((zweirn--notebook zweirn-inbox-notebook))
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
  (when (zweirn--is-inbox)
    ;; Only allow this in the root folder.
    (let* ((existing-notes (zweirn--notes-by-update-time (zweirn--notebook-path zweirn--notebook)))
           (jotted-notes (zweirn--filter-jotted-notes (aref (zweirn--pin-notes existing-notes) 1))))
      (when (not (null jotted-notes))
        ;; Only bother if we have jotted notes.
        (let* ((fname (zweirn--fresh-name))
               (new-file (zweirn--note-path fname))
               (buff (get-file-buffer new-file)))
          (if (null buff)
              (let* ((root (zweirn--notebook-path zweirn-inbox-notebook)))
                (with-current-buffer (find-file-noselect new-file)
                   ;;(insert (format "\n# Note %s\n\n" (zweirn--date-tag)))
                 (insert (format "\n# %s\n\n" (zweirn--untitled)))
                  (dolist (nt jotted-notes)
                    (let* ((original-file (concat (file-name-as-directory root) nt))
                           (fname (zweirn--fresh-name))
                           (trash-path (zweirn--notebook-path zweirn-trash-notebook))
                           (new-file (concat (file-name-as-directory trash-path) fname)))
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
        (zweirn--open-note-in-markdown (zweirn--note-path nt))
      (message "Cursor not over a note"))))

(defun zweirn-nv-read-note ()
  "Load the note pointed to by the point in a zweirn-nv buffer, killing the buffer in the process. Handle direct access notes as well."
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let ((file (if (zweirn--is-internal nt)
                        (concat (file-name-as-directory zweirn-root-folder) nt)
                      nt)))
          (kill-buffer (current-buffer))
          (zweirn--open-note-in-markdown file))
      (message "Cursor not over a note"))))

(defun zweirn-pdf-note ()
  "Convert the note pointed to by the point into PDF using function rp/pdf-markdown"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
          (rp/pdf-markdown (zweirn--note-path nt))
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
  "Move the note to notebook"
  (interactive)
  (let ((nt (zweirn--current-name)))
    (if nt
        (let* ((title (zweirn--note-title nt))
               (name (format "%s.%s" (zweirn--clean-title title) zweirn-default-extension))
               (target (zweirn--query-notebook "Move to"))
               (target-notebook (cadr target))
               (target-external (zweirn--is-external target-notebook))
               (target-path (zweirn--notebook-path target-notebook))
               ;; If target folder is external, ask for a name.
               ;; If source folder is external, we need to create a fresh name.
               (name (if target-external
                         (read-string (format "Name (%s): " name target-path) nil nil name)
                       (if zweirn--external
                           (zweirn--fresh-name)
                         nt))))
          (rename-file (zweirn--note-path nt) (zweirn--notebook-note-path target-path name))
          (zweirn--show (point)))
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
            (let* (;; Assign fresh name if deleting from an external notebook.
                   (fname (if zweirn--external (zweirn--fresh-name) nt))
                   (trash-path (zweirn--notebook-path zweirn-trash-notebook))
                   (new-file (concat (file-name-as-directory trash-path) fname)))
              (rename-file (zweirn--note-path nt) new-file)
              (zweirn--show (point)))))
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

(defun zweirn--snap-to-note ()
  "Go to note marker for the current line if it exists, otherwise forward to the next one or backward to previous"
  (let ((r (rx line-start (eval zweirn-note-symbol))))
    (beginning-of-line)
    (if (re-search-forward r nil t)
        ;; Move back to *.
        (left-char)
      ;; Search failed, search backward.
      (when (not (re-search-backward r nil t))
        ;; Nothing. Just to the end.
        (goto-char (point-min))))))

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
  (dired (zweirn--notebook-path zweirn--notebook)))


(defun zweirn-open-notebook ()
  "Open notebook"
  (interactive)
  (let* ((target (zweirn--query-notebook "Open"))
         (target-notebook (cadr target))
         (target-path (zweirn--notebook-path target-notebook)))
    (zweirn target-notebook)))


(defun zweirn-kill ()
  "Kill current buffer without asking anything"
  (interactive)
  (kill-buffer (current-buffer)))


(defun zweirn-reload ()
  (interactive)
  ;; TODO: Ideally, we want to stay on the same note we were on.
  ;; This requires tracking which note we're on and passing it through instead of the point.
  (zweirn--show (point)))


(defun zweirn-nv-search ()
  (interactive)
  ;; We need to do this work from the root folder.
  (let* ((name zweirn--buffer-prefix-nv)
         (buff (get-buffer-create name)))
    (switch-to-buffer buff)
    (zweirn-nv-mode)
    ;; Switching major mode clears the local variables.
    (make-local-variable 'zweirn-nv--search-string)
    (setq zweirn-nv--search-string "")
    (make-local-variable 'zweirn-nv--notebook-notes)
    (setq zweirn-nv--notebook-notes '())
    (dolist (target zweirn-notebooks)
      (let ((zweirn--notebook (cadr target)))
        (let* ((notes (zweirn--notes-by-update-time (zweirn--notebook-path zweirn--notebook)))
               (notes (zweirn--sort-by-title notes)))
          (setq zweirn-nv--notebook-notes (cons (list (cadr target) notes) zweirn-nv--notebook-notes)))))
    (zweirn--show-nv-search)))

(defun zweirn--show-nv-search ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "==> ")
    (insert zweirn-nv--search-string)
    (newline)
    (let ((seen-one nil))
      (dolist (subf-notes zweirn-nv--notebook-notes)
        (let ((zweirn--notebook (car subf-notes)))
          (let ((seen-one-here (zweirn-nv--show-notebook-notes (car subf-notes) (cadr subf-notes))))
            (setq seen-one (or seen-one seen-one-here)))))
      (goto-char (point-min))
      (if seen-one
          (zweirn-move-next-note)
        (goto-char (point-max))))))

(defun zweirn-nv--show-notebook-notes (name notes)
  (let ((seen-one nil))
    (dolist (nt notes)
      (let ((title (zweirn--note-title nt))
            ; Put nt as notebook/nt so that when we open the note it picks it up from the notebook.
            (path-nt (concat (file-name-as-directory name) nt)))
        (when (string-match-p (regexp-quote zweirn-nv--search-string) (downcase title))
          (when (not seen-one)
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
  (let* ((notes (zweirn--notes-by-update-time (zweirn--notebook-path zweirn--notebook)))
         (name (format "%s: %s" zweirn--buffer-prefix-search s))
         (buff (get-buffer-create name))
         (seen nil)
         (num-notes 0))
    ;; This rigmarole is needed because zweirn--folder is buffer local,
    ;; so when we switch buffers, we need to save it and use dynamic binding!
    (let ((temp-zweirn-notebook zweirn--notebook))
      (switch-to-buffer buff)
      (let ((zweirn--notebook temp-zweirn-notebook))
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Images

(defun zweirn--make-asset-directory-name (uuid)
  (format "F-%s" uuid))

(defun zweirn--create-asset-directory (uuid)
  (let* ((name (zweirn--make-asset-directory-name uuid))
         (path (zweirn--notebook-path zweirn-assets-notebook))
         (path (concat (file-name-as-directory path) name)))
    (when (not (file-exists-p path))
      ;; Create if it does not already exists.
      (make-directory path))))


(defun zweirn--delete-asset-directory (uuid)
  (let* ((name (zweirn--make-asset-directory-name uuid))
         (start-path (zweirn--notebook-path zweirn-assets-notebook))
         (start-path (concat (file-name-as-directory start-path) name))
         (target-path (zweirn--notebook-path zweirn-trash-notebook))
         (target-path (concat (file-name-as-directory target-path) name)))
    (when (not (file-exists-p start-path))
      (error (format "No path %s" start-path)))
    (when (file-exists-p target-path)
      (error (format "Path %s already exists?" target-path)))
    (rename-file start-path target-path)))


(defvar zweirn-convert-program "/opt/homebrew/bin/convert")

(defun zweirn--copy-image (src uuid)
  (let* ((uuid-path (zweirn--make-asset-directory-name uuid))
         (assets-path (zweirn--notebook-path zweirn-assets-notebook))
         (path (concat (file-name-as-directory assets-path) (file-name-as-directory uuid-path)))
         (name (file-name-nondirectory src)))
    (zweirn--create-asset-directory uuid)
    ;; Last argument indicates to confirm in case of overwrite.
    (copy-file src path 1)
    ;; Resize if needed. Copy again so that previous can be used to confirm overwrite if needed.
    (when (> (zweirn--get-image-width src) zweirn-max-image-width)
      (zweirn--resize-image src (concat (file-name-as-directory path) name)))
    (concat path (file-name-nondirectory src))))

(defun zweirn--resize-image (src tgt)
  (let* ((cmd (format "%s -resize %dx '%s' '%s'" zweirn-convert-program zweirn-max-image-width src tgt))
         result)
    (setq result (shell-command cmd))
    (when (> result 0)
      (error "cannot run convert command to resize"))))

(defun zweirn--get-image-width (src)
  (string-to-number
   (shell-command-to-string
    (format "%s '%s' -ping -format \"%%w\" info:" zweirn-convert-program src))))

(defun zweirn--current-under-zweirn-control ()
  (let* ((full-file-name (buffer-file-name (current-buffer)))
         (file-name (and full-file-name (file-name-nondirectory full-file-name))))
    (and file-name
         ;; TODO: Also check that file is under a notebook under zweirn-root-folder
         (derived-mode-p 'markdown-mode)
         (zweirn--note-uuid file-name))))

(defun zweirn-markdown-insert-image (src)
  "Insert image into a markdown file under Zweirn."
  (interactive (list (read-file-name "Image: ")))
  (let* ((uuid (zweirn--current-under-zweirn-control))
         name)
    (when uuid
      (setq path (zweirn--copy-image src uuid))
      (setq name (file-name-nondirectory path))
      (markdown-insert-inline-image name path)
      (when (display-images-p)
        (markdown-display-inline-images)))))

(defun zweirn-drag-n-drop-image (evt)
  "Invoked by [drag-n-drop] in Markdown notes opened by Zweirn."
  (interactive "e")
  (let* (file-info)
    (when (eq (car evt) 'drag-n-drop)
      (setq file-info (caddr evt))
      (when (and (eq (car file-info) 'file)
                 (> (length file-info) 2))
        (zweirn-markdown-insert-image (caddr file-info))))))

(defun zweirn-markdown-open-assets ()
  "Open the assets folder for the current note."
  (interactive)
  (let* ((uuid (zweirn--current-under-zweirn-control))
         (assets-folder (zweirn--notebook-path zweirn-assets-notebook))
         name
         path)
    (when uuid
      (setq name (zweirn--make-asset-directory-name uuid))
      (setq path (concat (file-name-as-directory assets-folder) name))
      (shell-command (format "open '%s'" path)))))


;; Helper function to rename all notes in a notebookd - unsafe!
(defun zweirn--refresh-note-names ()
  (let* ((path (zweirn--notebook-path zweirn--notebook))
         (notes (zweirn--notes-by-update-time path))
         name)
    (dolist (nt notes)
      (setq name (zweirn--fresh-name))
      (rename-file (zweirn--note-path nt) (zweirn--note-path name)))
  ))

(defun zweirn--refresh-notebook-note-names (notebook)
  ;; Dynamic binding!
  (let ((zweirn--notebook notebook))
    (zweirn--refresh-note-names)))

(defun zweirn--index-notes ()
  ;; Go through all notes and get a map from uuid to notebook
  (let* ((notebooks (cons "_trash" (mapcar (lambda (n) (nth 1 n)) zweirn-notebooks)))
         notes
         result)
    (dolist (nb notebooks)
      (when (zweirn--is-internal nb)
        (setq notes (zweirn--notes-by-update-time (zweirn--notebook-path nb)))
        (setq result (append result (mapcar (lambda (nt) (cons (zweirn--note-uuid nt) nb)) notes)))))
    result))

(defun zweirn-gc-assets ()
  "Find all folders that do not connect to an existing note. We may want to do something special for _trash?"
  (interactive)
  (let* ((name "zweirn-gc-assets")
         (buff (get-buffer-create name))
         (index (zweirn--index-notes))
         (assets-folder (zweirn--notebook-path zweirn-assets-notebook))
         (filter-regexp (rx string-start
                            "F-"
                            (group (one-or-more alnum))
                            string-end))
         (folders (directory-files assets-folder nil filter-regexp t))
         uuid)
    (switch-to-buffer buff)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Unlinked asset folders in %s:\n\n" assets-folder))
      (dolist (f folders)
        (save-match-data
          (setq uuid (and (string-match filter-regexp f)
                          (match-string 1 f)))
          (when (not (assoc uuid index))
            (insert (format "%s\n" f))))))
    (goto-char (point-min))))


(provide 'zweirn)
;;; zweirn.el ends here

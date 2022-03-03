(require 'subr-x)

(defvar rp/default-embiggen-size 12)


(defun rp/toggle-fullscreen ()
  "Toggle full/maximal screen"
  (interactive)
  (cond
   ;; windows 10
   ((eq window-system 'w32) (toggle-frame-maximized))
   ;; mac os x
   ((eq window-system 'ns) (toggle-frame-fullscreen))
   ;; do nothing otherwise
   ))


(defun rp/edit-init ()
  "Edit emacs initialization file"
  (interactive)
  (find-file (concat-emacs-folder "init.el")))


(defun rp/embiggen (size)
  "Embiggen the font - optionally give the size as a parameter"
  (interactive (list (if current-prefix-arg ; <=== User provided arg
                         (prefix-numeric-value current-prefix-arg)
                       rp/default-embiggen-size))) ; <=== Default
  (set-face-attribute 'default nil :height (* size 10)))


(defun rp/cheat-sheet ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat-emacs-folder "cheat-sheet.org"))))


(defun rp/date ()
  ;; better - maybe `Thu 5/20/2021`?
  (interactive)
  (insert (format-time-string "%a %m/%d/%y")))


(defun rp/deoutlook (str &optional from to)
  "Translate outlook special characters in pasted text.

When called interactively, work on current paragraph or text selection.

When called in lisp code, if STRING is non-nil, returns a changed string.
If STRING is nil, change the text in the region between positions FROM and TO."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (let* ((workOnStringP (if $string t nil))
         (inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to))))
    (let ((outputStr (let ((case-fold-search t))
                       (replace-regexp-in-string "’" "'" inputStr))))
      (if workOnStringP
          outputStr
        (save-excursion
          (delete-region $from $to)
          (goto-char $from)
          (insert outputStr))))))


(defun rp/copy-file-path ()
  "Copy buffer's file path to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))


(defvar rp/pdf-convert-markdown "curl http://c.docverter.com/convert -s -F from=markdown -F to=pdf -F 'input_files[]=@%s' > %s")

(defvar rp/pdf-open "open %s")

(defun rp/pdf-markdown (input-file)
  (interactive (list (file-truename buffer-file-name)))
  (let* ((pdf (make-temp-file "output" nil ".pdf")))
    (shell-command (format rp/pdf-convert-markdown input-file pdf))
    (shell-command (format rp/pdf-open pdf))))


(defun rp/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


(defun rp/clean-emacs ()
  "Kill all buffers except *scratch*."
  (interactive)
  ;; TODO: Implement an exclusion list.
  (dolist (buff (buffer-list))
    (let ((name (buffer-name buff)))
      (unless (or (equal ?\s (aref name 0))
                  (equal name "*scratch*"))
        (kill-buffer buff))))
  (delete-other-windows))


(defun rp/python3-cli (program path buf-name)
  "Run command in a comint buffer"
  (interactive (let* ((program (read-file-name "Command: "))
                      (path (read-directory-name "Working directory: "))
                      (buf-name program))
                 (list program path buf-name)))
  (require 'comint)
  (let* ((buffer (get-buffer-create (format "*%s*" buf-name))))
    (pop-to-buffer-same-window buffer)
    ;; create the comint process if there is no process in buffer
    (unless (comint-check-proc buf-name)
      (setenv "PYTHONIOENCODING" "utf-8")
      (setenv "PYTHONUNBUFFERED" "true")
      (cd path)
      ;; TODO: allow passing parameters to program
      (make-comint-in-buffer buf-name buffer program)
      (set-buffer-process-coding-system 'utf-8 'utf-8))))

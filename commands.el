(require 'subr-x)

(defvar rp/powershell-random-uuid "powershell.exe -Command [guid]::NewGuid().toString()")

(defvar rp/default-embiggen-size 12)

;; From http://ergoemacs.org/emacs/elisp_generate_uuid.html
(defun rp/random-uuid ()
  "Insert a UUID - calls “uuidgen” on MacOS, Linux, and PowelShell on Microsoft Windows."
  (interactive)
  (insert
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
               (substring myStr 20 32)))))))


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
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

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
      (make-comint-in-buffer "muck" buffer program)
      (set-buffer-process-coding-system 'utf-8 'utf-8))))


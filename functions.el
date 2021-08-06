(require 'subr-x)

(defvar rp-powershell-command "powershell.exe")

;; From http://ergoemacs.org/emacs/elisp_generate_uuid.html
(defun rp-generate-random-uuid ()
  "Insert a UUID - calls “uuidgen” on MacOS, Linux, and PowelShell on Microsoft Windows."
  (cond
   ((string-equal system-type "windows-nt")
    (string-trim (shell-command-to-string (concat rp-powershell-command " -Command [guid]::NewGuid().toString()"))))
   ((string-equal system-type "darwin") ; Mac
    (string-trim (shell-command-to-string "uuidgen")))
   ((string-equal system-type "gnu/linux")
    (string-trim (shell-command-to-string "uuidgen")))
   (t
    ;; Code here by Christopher Wellons, 2011-11-18.
    ;; Editted Hideki Saito further to generate all valid variants
    ;; for "N" in xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx format.
    (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
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

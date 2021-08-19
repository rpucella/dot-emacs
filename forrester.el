    
;; fbncc

(defun fbncc ()
  "Run FBNCC in a comint buffer"
  (interactive)
  (require 'comint)
  (let* ((program "~/git/knowledge-transfer/fbncc/fbncc.bat")
         (path "~/git/knowledge-transfer/fbncc")
         (buffer (get-buffer-create "*fbncc*")))
    (pop-to-buffer-same-window buffer)
    ;; create the comint process if there is no process in buffer
    (unless (comint-check-proc "*fbncc*")
      (setenv "PYTHONIOENCODING" "utf-8")
      (setenv "PYTHONUNBUFFERED" "true")
      (cd path)
      (make-comint-in-buffer "fbncc" buffer program)
      (set-buffer-process-coding-system 'utf-8 'utf-8))))



;; muck

(defun muck ()
  "Run Muck in a comint buffer"
  (interactive)
  (require 'comint)
  (let* ((muck-program "~/git/dwh-manager/muck.bat")
         (muck-path "~/git/dwh-manager")
         (buffer (get-buffer-create "*muck*")))
    (pop-to-buffer-same-window buffer)
    ;; create the comint process if there is no process in buffer
    (unless (comint-check-proc "*muck*")
      (setenv "PYTHONIOENCODING" "utf-8")
      (setenv "PYTHONUNBUFFERED" "true")
      (cd muck-path)
      (make-comint-in-buffer "muck" buffer muck-program)
      (set-buffer-process-coding-system 'utf-8 'utf-8))))


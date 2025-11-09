;;; portal.el --- Simple functionality portals (lists of links)   -*- lexical-binding: t -*-


(require 'widget)


(define-derived-mode portal-mode
  special-mode "Portal"
  "Major mode for simple portals.")

(define-key portal-mode-map (kbd "TAB") 'widget-forward)
(define-key portal-mode-map (kbd "<backtab>") 'widget-backward)
(define-key portal-mode-map (kbd "b") 'scroll-down)


(defun portal (title description links)
  (let* ((name (format "portal: %s" title))
         (buff (get-buffer-create name)))
    ;; check if buffer exists?
    (switch-to-buffer buff)
    (portal-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (widget-insert (propertize (upcase title) 'face 'bold))
    (widget-insert "\n\n")
    (widget-insert description)
    (widget-insert "\n\n")
    (dolist (link links)
      (widget-create 'link
                     :button-prefix "* "
                     :button-suffix "\n"
                     :notify (lambda (&rest ignore)
                               (funcall (cdr link)))
                     (format "%s" (car link))))
    (widget-setup)
    (widget-forward 1)))


(defun portal--test ()
  (interactive)
  (portal "Test" "This is a test portal" `(("First link" . ,(lambda () (message "hello!"))))))


(defmacro defportal (name title description &rest links)
  (declare (indent 1))
  (let ((links (mapcar (lambda (link) `(cons ,(car link) (lambda () ,@(cdr link)))) links)))
    `(defun ,name ()
       (interactive)
       (portal ,title ,description (list ,@links)))))


(provide 'portal)

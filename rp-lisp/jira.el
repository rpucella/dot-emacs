;; jira.el --- Jira access for Emacs     -*- lexical-binding: t -*-

(require 'request) ;; https://tkf.github.io/emacs-request/manual.html
(require 'json)
(require 'cl-lib)  ;; cl-function [&key, &allow-other-keys arguments)


(defvar jira-api-v3 "https://forrtrak.atlassian.net/rest/api/3")
(defvar jira-api-agile "https://forrtrak.atlassian.net/rest/agile/1.0")

(defvar jira-api "https://forrtrak.atlassian.net/rest")
(defvar jira-board-id 168) ;; Data Experience
(defvar jira-project-id "10117") ;; Data Experience
(defvar jira-project-key "DE")
(defvar jira-reporter-id "5b6dc505fd0dbd29f4e003ce") ;; Riccardo

(defvar jira-auth-token nil) ;; set to something before using!
(defvar jira-board-url "https://forrtrak.atlassian.net/jira/software/c/projects/DE/boards")
(defvar jira-browse-url "https://forrtrak.atlassian.net/browse")

(defun jira--make-url (endpoint)
  (format "%s/%s" jira-api endpoint))

(defun jira--basic-auth ()
  (format "Basic %s" jira-auth-token))

(defun jira--get-item (endpoint K)
  (let ((url (jira--make-url endpoint))
        (auth (jira--basic-auth)))
    ;; Need to loop until done when returning an array?
    (request url
             :headers `(("Authorization" . ,auth)
                        ("Content-Type" . "application/json"))
             :parser 'json-read
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (message "Error: %S" error-thrown)))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (funcall K data))))))

(defun jira--post-item (endpoint body K)
  (let* ((url (jira--make-url endpoint))
         (auth (jira--basic-auth))
         (body (json-encode body)))
    ;; Need to loop until done when returning an array?
    (request url
             :type "POST"
             :data body
             :headers `(("Authorization" . ,auth)
                        ("Content-Type" . "application/json"))
             :parser 'json-read
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (message "Error: %S" error-thrown)))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (funcall K data))))))

(defun jira--get-items (endpoint items-field K)
  (let* ((reporter (make-progress-reporter (format "Reading items from %s..." endpoint) 0 100)))
    (jira--get-items-rec endpoint K items-field 0 nil reporter)))

(defun jira--is-false (v)
  (or (not v) (eq v :json-false)))

(defun jira--is-done (total count is-last)
  (if total
      (= count total)
    (not (jira--is-false is-last))))

(defun jira--get-items-rec (endpoint K items-field start vresult reporter)
  (let* ((url (jira--make-url endpoint))
         (auth (jira--basic-auth)))
    (request url
             :headers `(("Authorization" . ,auth)
                        ("Content-Type" . "application/json"))
             :parser 'json-read
             :params `(("startAt" . ,start))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (message "Error: %S" error-thrown)))
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (let* ((vdata (alist-get items-field data))
                                            (is-last (alist-get 'isLast data))
                                            (total (alist-get 'total data))
                                            (vresultNew (vconcat vresult vdata)))
                                       (when (and total (> total 0))
                                         (progress-reporter-update reporter (/ (* 100 (length vresultNew)) total)))
                                       (if (jira--is-done total (length vresultNew) is-last)
                                           (progn (progress-reporter-done reporter)
                                                  (funcall K vresultNew))
                                         (jira--get-items-rec endpoint K items-field (+ start (length vdata)) vresultNew reporter))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing functions.

(defun jira--test-endpoint (endpoint)
  (jira--get-item endpoint 'jira--output-data))

(defun jira--test ()
  ;; Pull ticket DE-2633.
  (jira--fetch-ticket "DE-2633"))

(defun jira--output-data (data)
  (with-output-to-temp-buffer "*jira output*"
    (pp data)))

(defun jira--output-data-list (data-list)
  (with-output-to-temp-buffer "*jira output*"
    (princ (format "%d items\n\n" (length data-list)))
    (seq-doseq (data data-list)
      (pp data)
      (princ "\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fetch items from Jira and dump them (for now).

(defun jira--fetch-ticket (name K)
  (async-let* ((issue 'jira--get-item (format "api/3/issue/%s" name)))
              (funcall K issue)))

(defun jira--process-sprints (sprints K)
  (let* (;;(active nil)
         (future nil))
    (seq-doseq (spr sprints)
      (cond
       ((string= (alist-get 'state spr) "active") (push spr future))
       ((string= (alist-get 'state spr) "future") (push spr future))))
    (funcall K future)))

(defun jira--extract-active-sprint (sprints K)
  (let* (;;(active nil)
         (active nil))
    (seq-doseq (spr sprints)
      (when (string= (alist-get 'state spr) "active")
        (setq active spr)))
    (funcall K active)))

(defun jira--kont (f)
  (lambda (K)
    (lambda (&rest args)
      (apply f (append args (list K))))))

(defun jira--fetch-sprints (K)
  (async-let* ((raw 'jira--get-items (format "agile/1.0/board/%d/sprint" jira-board-id) 'values)
               (sprints 'jira--process-sprints raw))
              (funcall K sprints)))

(defun jira--fetch-active-sprint (K)
  (async-let* ((raw 'jira--get-items (format "agile/1.0/board/%d/sprint" jira-board-id) 'values)
               (sprints 'jira--process-sprints raw)
               (active 'jira--extract-active-sprint sprints))
              (funcall K active)))

(defun jira--process-epics (epics K)
  (let* ((result (seq-filter (lambda (epic) (eq (alist-get 'done epic) :json-false)) epics)))
    (funcall K result)))

(defun jira--fetch-epics (K)
  (async-let* ((epics 'jira--get-items (format "agile/1.0/board/%d/epic" jira-board-id) 'values)
               (epics 'jira--process-epics epics))
              (funcall K epics)))

(defun jira--fetch-issues-backlog (K)
  (async-let* ((issues 'jira--get-items (format "agile/1.0/board/%d/backlog" jira-board-id) 'issues))
              (funcall K issues)))

(defun jira--fetch-issues-sprint (sprint-id K)
  (async-let* ((issues 'jira--get-items (format "agile/1.0/board/%d/sprint/%d/issue" jira-board-id sprint-id) 'issues))
              (funcall K issues)))

(defun jira--apply (f vs K)
  (funcall K (apply f vs)))

(defun jira--process-tickets (raw-tickets K)
  (let* ((tickets (mapcar 'jira--clean-ticket raw-tickets)))
    (funcall K tickets)))

(defun jira--fetch-tickets-backlog (K)
  (async-let* ((issues 'jira--get-items (format "agile/1.0/board/%s/backlog" jira-board-id) 'issues)
               (issues 'jira--process-tickets issues))
              (funcall K issues)))

(defun jira--fetch-tickets-sprint (sprint-id K)
  (if sprint-id
      (async-let* ((issues 'jira--get-items (format "agile/1.0/board/%s/sprint/%s/issue" jira-board-id sprint-id) 'issues)
                   (issues 'jira--process-tickets issues))
                  (funcall K issues))))

(defun jira--transform-sprints (sprints)
  (mapcar (lambda (sprint)
            (list (alist-get 'name sprint)
                  (format "agile/1.0/board/%s/sprint/%s/issue" jira-board-id (alist-get 'id sprint))))
          sprints))

(defun jira--fetch-tickets-sprints (sprints K)
  (async-let* ((tickets 'jira--fetch-tickets-multi (jira--transform-sprints sprints))
               (tickets 'jira--keep-main tickets))
              (funcall K tickets)))

(defun jira--keep-main (tickets K)
  (let* ((result (seq-filter (lambda (tick) (memq (plist-get tick :type) '(story task bug spike))) tickets)))
    (funcall K result)))

(defun jira--fetch-tickets-active (K)
  (async-let* ((active 'jira--fetch-active-sprint)
               (tickets 'jira--fetch-tickets-multi (jira--transform-sprints (list active)))
               (tickets 'jira--keep-main tickets))
              (funcall K tickets)))

(defun jira--attach-source (raw-source vec K)
  (let* ((source (jira--classify-source raw-source))
         (new-vec (seq-map (lambda (pl)
                             (plist-put pl :source source)
                             (plist-put pl :raw-source raw-source)
                             pl)
                           vec)
                  ))
    (funcall K new-vec)))

(defun jira--fetch-tickets-multi (endpoints K)
  (if endpoints
      (let* ((first (car endpoints))
             (source (car first))
             (endpoint (cadr first)))
        (async-let* ((data 'jira--get-items endpoint 'issues)
                     (data 'jira--process-tickets data)
                     (data 'jira--attach-source source data)
                     (data2 'jira--fetch-tickets-multi (cdr endpoints)))
                    (funcall K (vconcat data data2))))
    (funcall K nil)))

(defmacro async-let* (bindings &rest body)
  (if bindings
      (let* ((binding (car bindings))
             (res-var (car binding))
             (fn-name (cadr binding))
             (fn-args (cddr binding)))
        `(funcall ,fn-name
                  ,@fn-args
                  (lambda (,res-var) (async-let* ,(cdr bindings) ,@body))))
    `(progn ,@body)))

(defun jira--fetch-tickets-all (K)
  (async-let* ((sprints 'jira--fetch-sprints)
               (tickets1 'jira--fetch-tickets-sprints sprints)
               (tickets2 'jira--fetch-tickets-backlog)
               (tickets2 'jira--attach-source "Backlog" tickets2))
              (funcall K (vconcat tickets1 tickets2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nicer interface.

(defvar jira--issue-types
  '((epic . "10000")
   (story . "10001")
   (bug . "10004")
   (task . "10028")
   (spike . "10039")
   (subtask . "10003")
   (subbug . "10071")))

(defun jira--classify-issue (issue-obj)
  (if issue-obj
      (let* ((id (assoc-default 'id issue-obj))
             (type (car (rassoc id jira--issue-types))))
        (or type '-)
        ;; (cond
        ;;  ((equal id "10000") 'epic)
        ;;  ((equal id "10001") 'story)
        ;;  ((equal id "10004") 'bug)
        ;;  ((equal id "10028") 'task)
        ;;  ((equal id "10039") 'spike)
        ;;  ((equal id "10003") 'subtask)
        ;;  ((equal id "10071") 'subbug)
        ;;  (t '-))
        )
    nil))

(defun jira--is-sub (ticket)
  (let ((s (plist-get ticket :type)))
    (memq s '(subtask subbug))))

(defun jira--classify-status (status-obj)
  (if status-obj
      (let ((id (assoc-default 'id status-obj)))
        (cond
         ((equal id "10000") 'todo)
         ;;((equal id "10001") 'done)
         ((equal id "10078") 'done)
         ((equal id "10062") 'qa)
         ;;((equal id "10085") 'hold)
         ((equal id "10076") 'hold)
         ((equal id "3") 'prog)
         (t '-)))
    nil))

(defun jira--classify-source (source-obj)
  (if source-obj
      (cond ((string-prefix-p "Backlog" source-obj) "bck")
            ((string-prefix-p "DE Sprint " source-obj) (format "%3s" (substring source-obj 10)))
            (t "?  "))
    nil))

(defun jira--classify-name (assignee-obj)
  (if assignee-obj
      ;;;(or (car (split-string (alist-get 'displayName assignee-obj))) "")
      (mapconcat (lambda (s) (substring s 0 1)) (split-string (alist-get 'displayName assignee-obj)) "")
    "  "))

(defun jira--clean-ticket (ticket)
  (let* ((key (assoc-default 'key ticket))
         (fields-obj (alist-get 'fields ticket))
         (type-obj (alist-get 'issuetype fields-obj))
         (summary (alist-get 'summary fields-obj ""))
         (status-obj (alist-get 'status fields-obj))
         (assignee-obj (alist-get 'assignee fields-obj))
         (epic-obj (alist-get 'fields (alist-get 'parent fields-obj)))
         (epic (alist-get 'summary epic-obj)))
    (;; jira-ticket-create
     list :name key
     :raw-type type-obj
     :type (jira--classify-issue type-obj)
     :summary summary
     :raw-status status-obj
     :status (jira--classify-status status-obj)
     :raw-assignee assignee-obj
     :assignee (jira--classify-name assignee-obj)
     :epic epic
     :raw ticket
     :kind :jira
     :description (alist-get 'description fields-obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'widget)

(define-derived-mode jira-mode
  special-mode "Jira"
  "Major mode for showing list of Jira tickets.")

(define-key jira-mode-map (kbd "TAB") 'widget-forward)
(define-key jira-mode-map (kbd "<backtab>") 'widget-backward)
(define-key jira-mode-map (kbd "b") 'scroll-down)
(define-key jira-mode-map (kbd "J") 'jira-open-jira)
(define-key jira-mode-map (kbd "c") 'jira-create-local-ticket)
(define-key jira-mode-map (kbd "g") 'jira-soft-refresh)
(define-key jira-mode-map (kbd "G") 'jira-hard-refresh)
(define-key jira-mode-map (kbd "n") 'widget-forward)
(define-key jira-mode-map (kbd "p") 'widget-backward)
(define-key jira-mode-map (kbd "N") 'jira-epic-forward)
(define-key jira-mode-map (kbd "P") 'jira-epic-backward)

(defmacro jira--make-state (&rest vars)
  ;; vars = (:name init refresh-fn)
  `(progn (make-local-variable 'jira--*state*)
          (setq jira--*state* (list ,@vars))))

(defmacro jira--sget (field)
  `(plist-get jira--*state* ,field))

(defmacro jira--sput (field value)
  `(plist-put jira--*state* ,field ,value))

(defun jira (sprint)
  (interactive (list (read-string "Sprint name, active, backlog, all (DEFAULT all): " nil nil "all")))
  (let* ((name (format "*jira*"))
         (buff (get-buffer-create name))
         (local-tickets (jira--read-local-tickets)))
    ;; check if buffer exists?
    (switch-to-buffer buff)
    (jira-mode)
    (jira--make-state :tickets 'loading
                      :next-id (plist-get local-tickets :next-id)
                      :local (plist-get local-tickets :tickets)
                      :bucket-by :epic
                      :sort-by :source
                      :sprint sprint
                      :sprints nil
                      :epics nil
                      :sources nil)
    (jira--load-epics-sprints-and-tickets sprint)
    (make-local-variable 'jira--cache-epic-positions)
    (setq jira--cache-epic-positions '())
    (jira--render)))

(defun jira--load-sprint-tickets (sprint)
  (pcase sprint
    ;; TODO: set up the callback so that it select the jira buffer!
    ("all" (jira--fetch-tickets-all 'jira--update-tickets))
    ("backlog" (jira--fetch-tickets-backlog 'jira--update-tickets))
    ("active" (jira--fetch-tickets-active 'jira--update-tickets))
    (_ (message "Unsupported"))))

(defun jira--clean-epics (epics)
  (mapcar (lambda (epic) (cons (alist-get 'key epic) (alist-get 'summary epic))) epics))

(defun jira--clean-sprints (sprints)
  (print  sprints)
  (mapcar (lambda (sprint) (cons (alist-get 'name sprint) (alist-get 'id sprint))) sprints))

(defun jira--load-epics-sprints-and-tickets (sprint)
  (jira--fetch-epics (lambda (epics)
                       (jira--sput :epics (jira--clean-epics epics))
                       (jira--load-sprints-and-tickets sprint))))

(defun jira--load-sprints-and-tickets (sprint)
  (jira--fetch-sprints (lambda (sprints)
                         (jira--sput :sprints (jira--clean-sprints sprints))
                         (jira--load-sprint-tickets sprint))))

(defun jira--update-tickets (tickets)
  ;; Force *jira* buffer
  (with-current-buffer "*jira*"
    (let* ((sources nil))
      (seq-doseq (ticket tickets)
        (unless (member (plist-get ticket :raw-source) sources)
          (push (plist-get ticket :raw-source) sources)))
      (jira--sput :tickets tickets)
      (jira--sput :sources sources)
      (jira--render))))

(defun jira--update-local (local)
  (jira--sput :local (plist-get local :tickets))
  (jira--sput :next-id (plist-get local :next-id))
  (jira--render))

(defun jira--render ()
  (jira--clear-buffer)
  (if (eq (jira--sget :tickets) 'loading)
      (jira--render-loading)
    (jira--render-tickets))
  (widget-setup)
  (goto-char (point-min)))

(defun jira--clear-buffer ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun jira--render-loading ()
  (widget-insert "Loading epics and tickets"))

(defun jira--render-tickets ()
  (let* ((tickets (jira--sget :tickets))
         ;; Actually destructive update.
         (local (jira--resolve-epics (jira--sget :local)))
         (tickets (vconcat tickets local))
         (bucket-by (jira--sget :bucket-by))
         (sort-by (jira--sget :sort-by))
         (previous-bucket nil)
         (curr-bucket nil)
         (epic-positions nil))
    (widget-insert (format "Count: %s\n\n" (length tickets)))
    (jira--sort-by tickets bucket-by sort-by)
    (seq-doseq (ticket tickets)
      (when (jira--show-ticket-p ticket)
        (setq curr-bucket (or (plist-get ticket bucket-by) ""))
        (when (and (not (null previous-bucket))
                   (not (string= previous-bucket curr-bucket)))
          (widget-insert "                 |\n"))
        (when (or (null previous-bucket)
                  (not (string= previous-bucket curr-bucket)))
          (push (point) epic-positions))
        (jira--render-ticket ticket)
        (setq previous-bucket curr-bucket)))
    (setq jira--cache-epic-positions (nreverse epic-positions))))

(defun jira--show-ticket-p (ticket)
  ;; Check if we need to show this ticket.
  (memq (plist-get ticket :kind) '(:jira :local)))

(defun jira--render-ticket (ticket)
  (pcase (plist-get ticket :kind)
    (:jira (jira--render-ticket-jira ticket))
    (:local (jira--render-ticket-local ticket))))

;; "✓" "■"
(defun jira--render-ticket-jira (ticket)
  (let* ((width (- (window-body-width) 10))
         (type (alist-get (plist-get ticket :type)
                          '((story . "S") (task . "T") (bug . "B") (spike . "K")) " "))
         (status (alist-get (plist-get ticket :status)
                            '((todo . " ") (hold . "x") (prog . ">") (qa . "?") (done . "✓")) "!"))
         (color (alist-get (plist-get ticket :status)
                           '((todo . "white") (hold . "red") (prog . "yellow") (qa . "yellow") (done . "green"))))
         (qa-prefix (alist-get (plist-get ticket :status) '((qa . "?? ")) ""))
         (assignee (downcase (truncate-string-to-width (or (plist-get ticket :assignee) "  ") 2)))
         (summary (or (plist-get ticket :summary) ""))
         (epic (or (plist-get ticket :epic) ""))
         (source (or (plist-get ticket :source) "   "))
         ;; Override, but still sort normally!
         (source (if (string= source "bck") "   " source))
         (summary (if (eq (plist-get ticket :status) 'qa)
                      (propertize summary 'face `(:foreground ,color :slant italic))
                    (propertize summary 'face `(:foreground ,color))))
         (text1 (format "%s%s %s | %s" type source assignee summary))
         (text1 (if (>= (+ (length text1) (length epic)) width)
                    (truncate-string-to-width text1 (- width (length epic) 1))
                  text1))
         (text2 (string-pad epic (- width (length text1)) ? t))
         (name (plist-get ticket :name))
         (epics (jira--sget :epics))
         (sprints (jira--sget :sprints)))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :notify (lambda (&rest ignore) (jira-ticket name ticket epics sprints))
                   :custom-jira-ticket ticket
                   (format "%-8s" (plist-get ticket :name)))
    (widget-insert (format " %s %s\n" text1 text2))))

(defun jira--render-ticket-local (ticket)
  (let* ((width (- (window-body-width) 10))
         (type (alist-get (plist-get ticket :type)
                          '((story . "S") (task . "T") (bug . "B") (spike . "K")) " "))
         (summary (or (plist-get ticket :summary) ""))
         (epic (or (plist-get ticket :epic) ""))
         (text1 (format "%s       |  ► %s" type summary))
         (text1 (if (>= (+ (length text1) (length epic)) width)
                    (truncate-string-to-width text1 (- width (length epic) 1))
                  text1))
         (text2 (string-pad epic (- width (length text1)) ? t))
         (name (plist-get ticket :name))
         (epics (jira--sget :epics))
         (sprints (jira--sget :sprints)))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :notify (lambda (&rest ignore) (jira-ticket name ticket epics sprints))
                   :custom-jira-ticket ticket
                   (format " ► %-5s" (plist-get ticket :name)))
    (widget-insert (format " %s %s\n" text1 text2))))


(defun jira--resolve-epics (tickets)
  (let* ((epics (jira--sget :epics)))
    (seq-doseq (tick tickets)
      (plist-put tick :epic (alist-get (plist-get tick :epic-key) epics nil nil 'equal)))
    tickets))

(defun jira--string<= (a b)
  ;; Eventually, can make it more clever, dependent on type?
  (or (string= a b)
      (string< a b)))

(defun jira--sort-by (tickets field1 field2)
  (sort tickets (lambda (a b)
                  (let ((a1 (format "%s" (plist-get a field1)))
                        (b1 (format "%s" (plist-get b field1)))
                        (a2 (format "%s" (plist-get a field2)))
                        (b2 (format "%s" (plist-get b field2))))
                    (if (string= a1 b1)
                        (jira--string<= a2 b2)
                      (jira--string<= a1 b1))))))

(defun jira--temp-show-ticket (name)
  (jira--fetch-ticket name 'jira--output-data))

(defun jira-open-jira ()
  (interactive)
  (let* ((url (format "%s/%d/backlog" jira-board-url jira-board-id)))
    (shell-command (format "open \"%s\"" url))))

(defun jira-soft-refresh ()
  (interactive)
  (when (eq major-mode 'jira-mode)
    (jira--update-local (jira--read-local-tickets))))

(defun jira-hard-refresh ()
  (interactive)
  (when (eq major-mode 'jira-mode)
    (let* ((sprint (jira--sget :sprint)))
      (jira--load-sprint-tickets sprint))))

(defun jira--find-next-position-in-list (p positions)
  (catch 'exit
    (while (not (null positions))
      (if (< p (car positions))
          (throw 'exit (car positions))
        (setq positions (cdr positions))))
    nil))

(defun jira-epic-forward ()
  (interactive)
  (let* ((p (point))
         (positions jira--cache-epic-positions))
    (catch 'exit
      (while (not (null positions))
        (when (< p (car positions))
          (goto-char (car positions))
          (throw 'exit nil))
        (setq positions (cdr positions)))
      (message "Past the last epic position"))))

(defun jira-epic-backward ()
  (interactive)
  (let* ((p (point))
         (positions (reverse jira--cache-epic-positions)))
    (catch 'exit
      (while (not (null positions))
        (when (> p (car positions))
          (goto-char (car positions))
          (throw 'exit nil))
        (setq positions (cdr positions)))
      (message "Before the first epic position"))))

(defun jira-create-local-ticket ()
  ;; Callable from outside?
  (interactive)
  (if (eq major-mode 'jira-mode)
      (let* ((widget (jira--find-closest-widget))
             (ticket (widget-get widget :custom-jira-ticket))
             (epic (plist-get ticket :epic))
             (epics (jira--sget :epics))
             (epic-key (car (rassoc (plist-get ticket :epic) epics))))
        (jira-edit-ticket nil epic-key epic))
    (jira-edit-ticket nil nil nil)))

(defun jira--find-closest-widget ()
  (save-excursion
    (beginning-of-line)
    (unless (eq (point) (point-min))
      (backward-char))
    (widget-forward 1)
    (widget-at)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode jira-ticket-mode
  special-mode "Jira Ticket"
  "Major mode for showing a Jira ticket.")

(define-key jira-ticket-mode-map (kbd "TAB") 'widget-forward)
(define-key jira-ticket-mode-map (kbd "<backtab>") 'widget-backward)
(define-key jira-ticket-mode-map (kbd "b") 'scroll-down)
(define-key jira-ticket-mode-map (kbd "j") 'jira-ticket-open-jira)
(define-key jira-ticket-mode-map (kbd "e") 'jira-ticket-edit)
(define-key jira-ticket-mode-map (kbd "k") 'jira-ticket-kill)
(define-key jira-ticket-mode-map (kbd "p") 'jira-ticket-push)
(define-key jira-ticket-mode-map (kbd "m") 'jira-ticket-move)

(defmacro jira-ticket--make-state (&rest vars)
  ;; vars = (:name init refresh-fn)
  `(let* ((state (make-hash-table))
          (refresh (make-hash-table))
          (vars (list ,@(mapcar (lambda (v) `(list ,(car v) ,(cadr v) ,(if (cddr v) `(quote ,(caddr v)) 'nil))) vars))))
     (make-local-variable 'jira-ticket--*state*)
     (dolist (var vars)
       (puthash (car var) (cadr var) state)
       (when (cddr var)
         (puthash (car var) (caddr var) refresh)))
     (setq jira-ticket--*state* (list :state state :refresh refresh))))

(defmacro jira-ticket--sget (field)
  `(gethash ,field (plist-get jira-ticket--*state* :state)))

(defmacro jira-ticket--sput (field value)
  `(puthash ,field ,value (plist-get jira-ticket--*state* :state)))

(defun jira-ticket (ticket-name ticket epics sprints)
  ;; (interactive (list (read-string "Ticket name: ") nil))
  (let* ((name (format "*jira: %s*" ticket-name))
         (buff (get-buffer-create name)))
    ;; check if buffer exists?
    (switch-to-buffer buff)
    (jira-ticket-mode)
    (jira-ticket--make-state (:name ticket-name)
                             (:ticket ticket)
                             (:epics epics)
                             (:sprints sprints))
    ;; (when (null ticket)
    ;;   (jira--fetch-ticket ticket-name 'jira-ticket--update-ticket))
    (jira-ticket--render)))

(defun jira-ticket--update-ticket (ticket)
  (jira-ticket--sput :ticket ticket)
  (jira-ticket--render))

(defun jira-ticket-open-jira ()
  (interactive)
  (when (eq major-mode 'jira-ticket-mode)
    (let* ((ticket (jira-ticket--sget :ticket))
           (url (format "%s/%s" jira-browse-url (plist-get ticket :name))))
      (when (eq (plist-get ticket :kind) :jira)
        (shell-command (format "open \"%s\"" url))))))

(defun jira-ticket-edit ()
  (interactive)
  (when (and (eq major-mode 'jira-ticket-mode)
             (eq (plist-get (jira-ticket--sget :ticket) :kind) :local))
    (jira-edit-ticket (jira-ticket--sget :ticket))))

(defun jira-ticket-kill ()
  (interactive)
  (let* ((ticket (jira-ticket--sget :ticket))
         (kind (plist-get ticket :kind))
         (prompt "Delete this local ticket?"))
    (when (and (eq major-mode 'jira-ticket-mode)
               (eq kind :local)
               (yes-or-no-p prompt))
      (jira--delete-local-ticket ticket)
      (kill-buffer (current-buffer)))))

(defun jira-ticket-push ()
  (interactive)
  (when (jira--is-enabled-local)
    (let* ((ticket (jira-ticket--sget :ticket))
           (types '(story task bug spike))
           (sprints (jira-ticket--sget :sprints))
           (type (intern (completing-read "Story type [story/task/bug/spike]: " types nil t)))
           (sprint (completing-read "Sprint: " (mapcar 'car sprints) nil t))
           ;; Note: hand created stories do not have a type! Ask for one by way of confirmation?
           (body (jira--create-jira-ticket ticket type sprint)))
      (jira--post-item "/api/3/issue" body (lambda (result)
                                             ;; Delete = mark as pushed!
                                             (jira--delete-local-ticket ticket)
                                             (kill-buffer (current-buffer)))))))

(defun jira--is-enabled-local ()
  (let* ((ticket (jira-ticket--sget :ticket))
         (kind (plist-get ticket :kind)))
    (and (eq major-mode 'jira-ticket-mode)
         (eq kind :local))))


(defun jira-ticket-move ()
  (interactive)
  (when (jira--is-enabled-local)
    (let* ((ticket (jira-ticket--sget :ticket))
           (epics (jira-ticket--sget :epics))
           (epic-names (mapcar 'cdr epics))
           (epic (completing-read "Epic name: " epic-names nil t))
           (epic-key (car (rassoc epic epics))))
      (plist-put ticket :epic epic)
      (plist-put ticket :epic-key epic-key)
      (jira--save-local-ticket ticket)
      (kill-buffer (current-buffer)))))


(defun jira--create-jira-ticket (ticket type sprint)
  (let* ((issuetype `(("id" . ,(alist-get type jira--issue-types))))
         (parent `(("key" . ,(plist-get ticket :epic-key))))
         (project `(("key" . ,jira-project-key)))
         (reporter `(("id" . ,jira-reporter-id)))
         (summary (plist-get ticket :summary))
         (sprints (jira-ticket--sget :sprints))
         (sprint-id (alist-get sprint sprints nil nil 'equal))
         (description `(("type" . "doc")
                        ("version" . 1)
                        ("content" (("type" . "paragraph")
                                    ("content" (("type" . "text")
                                                ("text" . ,(plist-get ticket :description)))))))))
    `(("fields" . (("issuetype" . ,issuetype)
                   ("project" . ,project)
                   ("parent" . ,parent)
                   ("summary" . ,summary)
                   ;; Sprint
                   ("customfield_10010" . ,sprint-id)
                   ;; Sarbox Requirement
                   ("customfield_10069" . (("id" . "10082")))
                   ("labels" . ("draft"))
                   ("description" . ,description))))))


(defun jira-ticket--render ()
  (let ((ticket (jira-ticket--sget :ticket)))
    (jira--clear-buffer)
    (widget-insert (format ";; Ticket:    %s\n" (jira-ticket--sget :name)))
    (cond ((null ticket) (jira-ticket--render-loading))
          ((eq (plist-get ticket :kind) :jira) (jira-ticket--render-jira))
          ((eq (plist-get ticket :kind) :local) (jira-ticket--render-local))
          (t (widget-insert (format "Unknown kind %s\n" (plist-get ticket :kind)))
             (widget-setup)
             (goto-char (point-min))))))

(defun jira-ticket--render-loading ()
  (widget-insert "Loading...\n")
  (widget-setup)
  (goto-char (point-min)))

(defun jira-ticket--render-jira ()
  (let* ((ticket (jira-ticket--sget :ticket))
         (source (plist-get ticket :raw-source))
         (status (alist-get 'name (plist-get ticket :raw-status)))
         (assignee (alist-get 'displayName (plist-get ticket :raw-assignee)))
         (epic (plist-get ticket :epic))
         (summary (plist-get ticket :summary))
         (description (plist-get ticket :description)))
    (when source
      (widget-insert (format ";; Source:    %s\n" source)))
    (when status
      (widget-insert (format ";; Status:    %s\n" status)))
    (when assignee
      (widget-insert (format ";; Assignee:  %s\n" assignee)))
    (when epic
      (widget-insert (format ";; Epic:      %s\n" epic)))
    (widget-insert "\n")
    (widget-insert (format "%s\n\n\n" (or summary "(empty summary)")))
    (widget-insert (format "%s\n" description))
    (widget-setup)
    (goto-char (point-min))))

(defun jira-ticket--render-local ()
  (let* ((ticket (jira-ticket--sget :ticket))
         (epic (plist-get ticket :epic))
         (summary (plist-get ticket :summary))
         (description (plist-get ticket :description)))
    (when epic
      (widget-insert (format ";; Epic:      %s\n" epic)))
    (widget-insert "\n")
    (widget-insert (format "%s\n\n\n" (or summary "(empty summary)")))
    (widget-insert (format "%s\n" description))
    (widget-setup)
    (goto-char (point-min))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actually, try to reproduce the commit mode from magit?

(define-derived-mode jira-edit-mode
  text-mode "Jira Ticket Create/Edit"
  "Major mode for creating/editing a Jira ticket.")

(defun jira-edit-ticket (&optional ticket epic-key epic)
  (interactive)
  ;;(print (cons epic-key epic))
  (let* ((ticket-name (plist-get ticket :name)) ;; nil if doesn't exist
         (name (format "*jira: %s*" (or ticket-name "NEW")))
         (buff (get-buffer-create name))
         (point-summary nil)
         (epic-key (or epic-key (plist-get ticket :epic-key)))
         (epic (or epic (plist-get ticket :epic))))
    ;; check if buffer exists?
    (switch-to-buffer buff)
    (jira-edit-mode)
    (setq buffer-read-only nil)
    (when (null ticket)
      (setq ticket (list :name nil
                         :epic epic
                         :epic-key epic-key
                         :summary "PROVIDE ONE-LINE SUMMARY"
                         :description "PROVIDE DESCRIPTION"
                         :kind :local)))
    ;; Needed?
    (make-local-variable 'jira-edit--ticket)
    (setq jira-edit--ticket ticket)
    (erase-buffer)
    (insert ";; Use C-c C-c to commit changes; C-c C-k to abort\n")
    (insert ";; The first non-empty non-comment line is the summary\n")
    (insert ";; Everything following the summary is the description, including comments\n;;\n")
    (insert (format ";; Ticket:  %s\n" (or ticket-name "NEW")))
    (when epic
      (insert (format ";; Epic:    %s\n" epic)))
    (insert "\n")
    (setq point-summary (point))
    (insert (format "%s\n\n\n" (plist-get ticket :summary)))
    (insert (format "%s\n" (plist-get ticket :description)))
    (local-set-key (kbd "C-c C-k") 'jira-edit-abort)
    (local-set-key (kbd "C-c C-c") 'jira-edit-commit)
    (goto-char point-summary)))

(defun jira-edit-abort ()
  (interactive)
  (when (eq major-mode 'jira-edit-mode)
    (message "Aborting")
    (kill-buffer (current-buffer))))

(defun jira-edit-commit ()
  (interactive)
  (when (eq major-mode 'jira-edit-mode)
    (let* ((summ-desc (jira--find-summary-and-description))
           (summary (car summ-desc))
           (description (cdr summ-desc))
           (ticket (copy-sequence jira-edit--ticket)))
      (message (format "Committing"))
      (plist-put ticket :summary summary)
      (plist-put ticket :description description)
      (jira--save-local-ticket ticket)
      (kill-buffer (current-buffer))))
  )

(defun jira--find-summary-and-description()
  (let* ((summary nil)
         (description nil)
         (line nil))
    (save-excursion
      ;; Top of buffer.
      (goto-char (point-min))
      ;; Search through every line to find the first non-empty non-comment.
      (while (and (null summary) (not (eobp)))
        (setq line (string-trim (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
        (when (and (not (string-empty-p line))
                   (not (string= (substring line 0 1) ";")))
          (setq summary line))
        (forward-line 1))
      ;; At this point, if first-line is set, then the
      ;; point is on the line _after_ the first line.
      (setq description (string-trim (buffer-substring-no-properties
                                      (point)
                                      (point-max))))
      (if (null summary)
          (cons "" "")
        (cons summary description)))))

(defun jira-edit--render-loading ()
  (widget-insert "Loading...\n")
  (widget-setup)
  (goto-char (point-min)))

(defun jira-edit--render-jira ()
  (let* ((ticket (jira-edit--sget :ticket)))
    (widget-insert (format ";; Source:    %s\n" (plist-get ticket :raw-source)))
    (widget-insert (format ";; Status:    %s\n" (alist-get 'name (plist-get ticket :raw-status))))
    (widget-insert (format ";; Assignee:  %s\n" (alist-get 'displayName (plist-get ticket :raw-assignee))))
    (widget-insert (format ";; Epic:      %s\n\n" (plist-get ticket :epic)))
    (widget-insert (format "%s\n\n\n" (plist-get ticket :summary)))
    (widget-insert (format "%s\n" (plist-get ticket :description)))
    (widget-setup)
    (goto-char (point-min))))

(defun jira-edit--render-local ()
  (let* ((ticket (jira-edity--sget :ticket)))
    (widget-insert (format ";; Epic:      %s\n\n" (plist-get ticket :epic)))
    (widget-insert (format "%s\n\n\n" (plist-get ticket :summary)))
    (widget-insert (format "%s\n" (plist-get ticket :description)))
    (widget-setup)
    (goto-char (point-min))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local tickets

(defvar jira-local-tickets "jira-local-tickets.data")

(defun jira--local-tickets-file () (concat (file-name-as-directory user-emacs-directory) jira-local-tickets))

(defun jira--read-local-tickets ()
  (let* ((filepath (jira--local-tickets-file)))
    (if (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (goto-char (point-min))
          (read (current-buffer)))
      '())))

(defun jira--write-local-tickets (tickets)
  (let* ((filepath (jira--local-tickets-file)))
    (with-temp-file filepath
      (prin1 tickets (current-buffer)))))

(defun jira--save-local-ticket (ticket)
  (if (null (plist-get ticket :name))
      (jira--save-new-local-ticket ticket)
    (jira--save-existing-local-ticket ticket)))

(defun jira--save-new-local-ticket (ticket)
  (let* ((local (jira--read-local-tickets))
         (next-id (plist-get local :next-id))
         (tickets (plist-get local :tickets)))
    (plist-put ticket :name (format "%s" next-id))
    (plist-put local :next-id (+ next-id 1))
    (plist-put local :tickets (vconcat (vector ticket) tickets))
    (jira--write-local-tickets local)))

(defun jira--save-existing-local-ticket (ticket)
  (let* ((local (jira--read-local-tickets))
         (tickets (plist-get local :tickets))
         ;; Transform into alist with ticket name as key.
         (tickets (mapcar (lambda (ticket) (cons (plist-get ticket :name) ticket)) tickets)))
    (setf (alist-get (plist-get ticket :name) tickets nil nil 'equal) ticket)
    (plist-put local :tickets (apply 'vector (mapcar 'cdr tickets)))
    (jira--write-local-tickets local)))

(defun jira--delete-local-ticket (ticket)
  ;; Delete = mark as :pushed
  (let* ((local (jira--read-local-tickets))
         (tickets (plist-get local :tickets))
         (deleted-name (plist-get ticket :name)))
    (seq-doseq (tick tickets)
      (when (string= (plist-get tick :name) deleted-name)
        (plist-put tick :kind :pushed)))
    (jira--write-local-tickets local)))

;;; Function used once to migrate epics from "by name" to "by key"
;;;
;; (defun jira--migrate-tickets ()
;;   (let* ((local (jira--read-local-tickets))
;;          (tickets (plist-get local :tickets))
;;          (epics (jira--sget :epics)))
;;     (seq-doseq (tick tickets)
;;       (plist-put tick :epic-orig (plist-get tick :epic))
;;       (unless (null (plist-get tick :epic))
;;         (plist-put tick :epic (car (rassoc (plist-get tick :epic) epics)))))
;;     (plist-put local :tickets tickets)
;;     (jira--write-local-tickets local)))

;; (defun jira--migrate-tickets ()
;;   (let* ((local (jira--read-local-tickets))
;;          (tickets (plist-get local :tickets))
;;          (epics (jira--sget :epics)))
;;     (seq-doseq (tick tickets)
;;       (plist-put tick :epic-key (plist-get tick :epic))
;;       (plist-put tick :epic (plist-get tick :epic-orig)))
;;     (jira--write-local-tickets local)))


;; TODO:
;; sort by dimension [primary, secondary]
;; filter by dimension

;; n/p [next, previous]
;; N/P [next epic, previous epic]

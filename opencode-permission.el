;;; opencode-permission.el --- Permission response UI for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  nineluj

;; Author: nineluj <code@nineluj.com>
;; Keywords: tools, llm, opencode

;;; Commentary:

;; Provides a transient-based UI for responding to permission requests
;; from the opencode server.  Press o to accept once, a to accept
;; always (showing the patterns that will be permanently approved),
;; or n to deny.
;;
;; Dismissing the transient without responding (e.g. q or C-g) leaves
;; the request pending.  Re-open it later with `opencode-respond-permission'.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)
(require 'transient)

;;; Path utilities

(defun opencode--permission-normalize-path (path)
  "Return PATH normalized for display.
Relative to `default-directory' when inside it; otherwise use ~/
abbreviation for home directory paths, or the absolute path."
  (when (and path (not (string-empty-p path)))
    (let* ((expanded (expand-file-name path))
           (rel (file-relative-name expanded default-directory)))
      (if (string-prefix-p ".." rel)
          (let ((home (expand-file-name "~")))
            (if (string-prefix-p home expanded)
                (concat "~" (substring expanded (length home)))
              expanded))
        (if (string= rel "") "." rel)))))

;;; Permission title formatting

(defun opencode--permission-format-title (action metadata resources &optional message)
  "Return a human-readable title for a permission request.
ACTION is the permission kind string (e.g. \"shell\").
METADATA is an alist of extra fields supplied by the server.
RESOURCES is a list of the resources requested.
MESSAGE is the server's description of the request, preferred if given."
  (let-alist metadata
    (if (and message (not (string-empty-p message)))
        message
      (pcase action
        ("edit"
         (format "Edit %s"
                 (mapconcat #'opencode--permission-normalize-path
                            (or (mapcar (lambda (file) (alist-get 'file file)) .files)
                                (and .filepath (list .filepath))
                                resources)
                            ", ")))
        ("read"
         (format "Read %s"
                 (opencode--permission-normalize-path (or (car resources) ""))))
        ("glob"
         (format "Glob \"%s\"" (or (car resources) "")))
        ("grep"
         (format "Grep \"%s\"" (or (car resources) "")))
        ("shell"
         (format "Shell command: %s" (string-join resources " ")))
        ("webfetch"
         (format "WebFetch %s" (or .url (car resources) "")))
        ("websearch"
         (format "Web Search \"%s\"" (or .query (car resources) "")))
        ("subagent"
         (format "%s Task" (capitalize (or (car resources) "Unknown"))))
        ("skill"
         (format "Skill %s" (or (car resources) "")))
        ("question"
         "Ask a question")
        ("external_directory"
         (let* ((raw (when-let* ((p (car resources)))
                       (if (string-match-p "\\*" p)
                           (file-name-directory p)
                         p)))
                (dir (opencode--permission-normalize-path raw)))
           (format "Access external directory %s" (or dir ""))))
        (_
         (format "Call tool %s" action))))))

;;; State

(defvar opencode-permission--callback nil
  "Function to call with the user's choice string.
Called with one of \"once\", \"always\", or \"reject\".")

(defvar opencode-permission--type nil
  "The permission kind (e.g. \"bash\") for display in the transient header.")

(defvar opencode-permission--title nil
  "The permission title/description for display in the transient header.")

(defvar opencode-permission--always-patterns nil
  "The glob patterns approved by accepting always, for display in the transient.")

(defvar opencode-permission--handled nil
  "Non-nil when a response has been sent for the current transient.
Prevents double-handling if the transient exit hook fires after a suffix.")

;;; Suffix commands

(transient-define-suffix opencode-permission--do-accept ()
  "Accept the permission request (once)."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "once"))

(transient-define-suffix opencode-permission--do-accept-always ()
  "Accept the permission request permanently."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "always"))

(transient-define-suffix opencode-permission--do-deny ()
  "Deny the permission request."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "reject"))

;;; Transient exit hook

(defun opencode-permission--on-exit ()
  "Clean up the exit hook when the transient is dismissed.
Dismissing without responding leaves the permission pending so the
user can re-open it later with \\[opencode-respond-permission]."
  (remove-hook 'transient-exit-hook #'opencode-permission--on-exit))

;;; Transient prefix

(transient-define-prefix opencode-permission--transient ()
  "Respond to a permission request from the opencode server."
  [:description
   (lambda ()
     (or opencode-permission--title
         opencode-permission--type
         "permission"))
   ("o" "Ok"            opencode-permission--do-accept)
   ("a" opencode-permission--do-accept-always
    :description (lambda () (format "Accept always (%s)"
                                    (or opencode-permission--always-patterns
                                        opencode-permission--title ""))))
   ("n" "No"            opencode-permission--do-deny)])

;;; Entry point

(defun opencode-permission--prompt (type title always-patterns callback)
  "Display a transient popup to respond to a permission request.
TYPE is the permission kind, TITLE is the description,
ALWAYS-PATTERNS are the glob patterns for permanent approval.
CALLBACK is called with one of \"once\", \"always\", or \"reject\"."
  (setq opencode-permission--type type
        opencode-permission--title title
        opencode-permission--always-patterns always-patterns
        opencode-permission--callback callback
        opencode-permission--handled nil)
  (add-hook 'transient-exit-hook #'opencode-permission--on-exit)
  (transient-setup 'opencode-permission--transient))

;;; Permission request handling

(defun opencode--permission-queue-request (buffer permission-id session-id
                                                  action metadata resources save message)
  "Queue a permission request in BUFFER for SESSION-ID.
PERMISSION-ID identifies the request.  ACTION is the permission kind.
METADATA, RESOURCES, SAVE and MESSAGE come from the server request payload.
A request that's already queued is ignored."
  (with-current-buffer buffer
    (unless (seq-find (lambda (perm) (equal permission-id (plist-get perm :id)))
                      opencode-session-pending-permission)
      (let* ((title (opencode--permission-format-title action metadata resources message))
             (marker (make-marker)))
        (when (get-buffer-process buffer)
          (opencode--maybe-insert-block-spacing)
          (opencode--output
           (concat (propertize title 'face 'warning)
                   "\n\n"))
          ;; Marker points just before the trailing newlines so the
          ;; response label is appended inline on the same output line.
          (set-marker marker (- (opencode--session-process-position) 2)))
        (setq opencode-session-pending-permission
              (append opencode-session-pending-permission
                      (list (list :id permission-id
                                  :session-id session-id
                                  :type action
                                  :title title
                                  :always save
                                  :marker marker))))
        (if (opencode--buffer-active-p buffer)
            (run-at-time 0 nil #'opencode-respond-permission)
          (opencode--toast-show `((title . "OpenCode Permission Request")
                                  (message . ,title)
                                  (variant . "info")))
          (opencode-api-session (session-id)
              session
            (push session opencode-alerted-sessions)))))))

(defun opencode--permission-request (permission-id session-id action metadata
                                                   resources save &optional message)
  "Queue a permission request for SESSION-ID and output it to the session buffer.
PERMISSION-ID identifies the request.  ACTION is the permission kind
\(e.g. \"shell\").  METADATA is an alist of extra fields from the server.
RESOURCES is a list of what's requested.  SAVE is a list of patterns that
would be permanently approved.  MESSAGE is the server's description, if any.
A marker is saved so the response label can be appended to the same line.
If the session buffer is active, the response prompt is shown immediately.
Otherwise the session is opened so the request has somewhere to remain pending,
which fetches and queues its pending requests once it's displayed."
  (if-let* ((buffer (gethash session-id opencode-session-buffers))
            ((buffer-live-p buffer)))
      (opencode--permission-queue-request buffer permission-id session-id
                                          action metadata resources save message)
    (opencode-api-session (session-id)
        session
      (opencode-open-session session))))

(defun opencode--permission-label (perm choice)
  "Append a label for CHOICE to the output line of the permission request PERM."
  (when-let* ((marker (plist-get perm :marker))
              ((marker-buffer marker)))
    (let ((label (pcase choice
                   ("once"   (propertize " [accepted]"        'face 'success))
                   ("always" (propertize " [accepted always]" 'face 'success))
                   ("reject" (propertize " [denied]"          'face 'error))))
          (inhibit-read-only t))
      (save-excursion
        (goto-char marker)
        (insert label))
      (set-marker marker nil))))

(defun opencode--permission-replied (session-id request-id reply)
  "Drop the pending request REQUEST-ID of SESSION-ID, answered with REPLY elsewhere."
  (opencode--with-session-buffer session-id
    (when-let* ((perm (seq-find (lambda (perm) (equal request-id (plist-get perm :id)))
                                opencode-session-pending-permission)))
      (setq opencode-session-pending-permission
            (delq perm opencode-session-pending-permission))
      (opencode--permission-label perm reply))))

(defun opencode--send-permission-response (choice)
  "Send CHOICE as the response to the next pending permission request.
CHOICE is one of \"once\", \"always\", or \"reject\".
Pops the first request from the queue and appends an accepted/denied
label to the original output line via its saved marker."
  (unless opencode-session-pending-permission
    (user-error "No pending permission request"))
  (let* ((perm (car opencode-session-pending-permission))
         (permission-id (plist-get perm :id))
         (session-id (plist-get perm :session-id)))
    (setq opencode-session-pending-permission
          (cdr opencode-session-pending-permission))
    (opencode--permission-label perm choice)
    (when opencode-session-pending-permission
      (run-at-time 0 nil #'opencode-respond-permission))
    (opencode-api-respond-permission-request (session-id permission-id)
        `((decision . ,choice))
        _response)))

(defun opencode-respond-permission ()
  "Respond to the next pending permission request using a transient popup."
  (interactive)
  (unless opencode-session-pending-permission
    (user-error "No pending permission request"))
  (let* ((perm (car opencode-session-pending-permission))
         (type (plist-get perm :type))
         (title (plist-get perm :title))
         (always (plist-get perm :always))
         (always-display (when always (mapconcat #'identity always ", "))))
    (opencode-permission--prompt
     type title always-display #'opencode--send-permission-response)))

(provide 'opencode-permission)
;;; opencode-permission.el ends here

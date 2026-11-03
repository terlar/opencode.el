;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "31.1") (magit "4.0") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-sessions)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

;; pending fix upstream at https://github.com/r0man/plz-event-source/pull/15
;; see also: https://codeberg.org/sczi/opencode.el/issues/12
(setq plz-event-source-parser--line-regexp
      (rx (*? not-newline) (or "\r\n" "\n" "\r")))

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used when `opencode-serve-command' is nil to construct the serve command."
  :type 'string
  :group 'opencode)

(defcustom opencode-serve-command nil
  "Full command to start the opencode server.
When nil, the command is constructed from `opencode-command',
`opencode-host', and `opencode-port'.

Set this to a custom command for special cases like nix:
  \"nix run github:numtide/nix-ai-tools#opencode -- serve --port 4096 --hostname localhost\""
  :type '(choice (const :tag "Construct from opencode-command" nil)
          (string :tag "Custom command"))
  :group 'opencode)

(defcustom opencode-auto-start-server t
  "Whether to automatically start a server if none is running.
When nil, `opencode' will only connect to an already running server."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

(defvar opencode--process nil
  "Opencode server process when started by Emacs.")

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--server-running-p ()
  "Return non-nil if an opencode server is running at configured host and port."
  (condition-case err
      (plz 'get (format "http://%s:%d/global/health" opencode-host opencode-port)
        :headers (list (opencode--auth-header))
        :timeout 1)
    (plz-http-error
     (when-let* ((plz-error (cl-third err)))
       (when (= 401 (plz-response-status (plz-error-response plz-error)))
         (user-error "OpenCode server is already running but password protected: \
set `opencode-server-password' to connect to it"))))
    (error nil)))

(defun opencode--serve-command ()
  "Return the command to start the opencode server."
  (or opencode-serve-command
      (format "%s serve --port %d --hostname %s"
              opencode-command opencode-port opencode-host)))

(defun opencode--start-server (on-connect)
  "Start an opencode server and run ON-CONNECT when ready."
  (setf opencode-server-password
        (or opencode-server-password
            (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
              (apply #'string
                     (cl-loop repeat 32
                              collect (aref chars (random (length chars))))))))
  (setf opencode--process
        (let ((process-environment (cl-list*
                                    (format "OPENCODE_SERVER_USERNAME=%s"
                                            opencode-server-username)
                                    (format "OPENCODE_SERVER_PASSWORD=%s"
                                            opencode-server-password)
                                    process-environment)))
          (start-process-shell-command
           "opencode" "*opencode-serve*"
           (opencode--serve-command))))

  (set-process-filter
   opencode--process
   (lambda (process output)
     (with-current-buffer "*opencode-serve*"
       (goto-char (process-mark process))
       (insert output))
     (when (string-prefix-p "opencode server listening on" output)
       ;; Remove filter to avoid repeated callbacks
       (set-process-filter process nil)
       (opencode-connect opencode-host opencode-port on-connect))))

  (set-process-sentinel
   opencode--process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (unless (zerop (process-exit-status process))
         (pop-to-buffer (process-buffer process))
         (error "OpenCode failed, %s" (string-trim-right event "\n")))))))

(defun opencode-autoconnect (on-connect)
  "Connect to an existing server if one is running, otherwise start a new one.
Only starts new one if `opencode-auto-start-server' is non-nil.
Run ON-CONNECT after connected."
  (cond
   ;; Already connected
   (opencode--event-subscription
    (funcall on-connect))
   ;; We started a server process that's still alive
   ((process-live-p opencode--process)
    (funcall on-connect))
   ;; Server already running externally
   ((opencode--server-running-p)
    (opencode-connect opencode-host opencode-port on-connect))
   ;; Need to start a new server
   (opencode-auto-start-server
    (opencode--start-server on-connect))
   ;; Auto-start disabled, no server running
   (t
    (user-error "No opencode server running at %s:%d (auto-start disabled)"
                opencode-host opencode-port))))


;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory.
Connects to an existing server if one is running, otherwise starts a new one
if `opencode-auto-start-server' is non-nil."
  (interactive)
  (let ((project-dir (expand-file-name
                      (if-let* ((proj (project-current)))
                          (project-root proj)
                        default-directory))))
    (opencode-autoconnect (lambda () (opencode-open-project project-dir)))))

(defun opencode-connect (host port on-connect)
  "Connect to opencode server, prompting for HOST and PORT.
Call ON-CONNECT once connected."
  (interactive
   (list (read-string "Host: " opencode-host)
         (read-number "Port: " opencode-port)))
  (when opencode--event-subscription
    (user-error "Already connected"))
  (setq opencode-api-url (format "http://%s:%d" host port))
  (setq opencode--slash-commands-by-directory nil)
  (opencode--subscribe-global-events)
  (opencode-api-configured-providers result
    (setq opencode-providers (alist-get 'providers result))
    (opencode--fetch-agents on-connect))
  (message "Connected to %s" opencode-api-url))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (opencode--download-slash-commands directory)
  (let ((buffer-name (format "*OpenCode Sessions in %s*" directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq default-directory directory)
        (opencode-api-current-project project
          (let-alist project
            (setf (map-elt opencode--session-control-buffers .id)
                  (cons (current-buffer)
                        (seq-filter #'buffer-live-p
                                    (map-elt opencode--session-control-buffers
                                             .id))))))
        (opencode-sessions-redisplay)))
    (pop-to-buffer buffer-name)))

(defvar opencode-worktree-directory (expand-file-name "~/opencode_worktrees/")
  "Directory to store worktrees created for opencode.")

(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

(defun opencode-select-project ()
  "Completing read to prompt which project to select."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-projects projects
       (opencode-open-project
        (opencode--annotated-completion
         "Project: "
         (cl-loop for project in projects
                  for worktree = (alist-get 'worktree project)
                  when worktree
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 worktree)
                                worktree
                                (opencode--format-time-ago
                                 (opencode--time-ago
                                  project 'updated))))))))))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        type
                        event))
        (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defun opencode--maybe-run-file-edited-hook (part)
  "Record edited files and run per-file hooks if PART completed a file edit."
  (let-alist part
    (when (and (equal .type "tool")
               (equal .state.status "completed"))
      (dolist (file-ranges
               (pcase .tool
                 ("write"
                  (when .state.input.filePath
                    (list (cons .state.input.filePath nil))))
                 ((or "edit" "apply_patch")
                  (unless .state.metadata.diff
                    (error "Missing diff metadata for %s tool" .tool))
                  (opencode-diff->source-line-ranges .state.metadata.diff))))
        (when-let* ((file (opencode--resolve-file-reference (car file-ranges))))
          (opencode--with-session-buffer .sessionID
            (cl-pushnew file opencode--files-edited-this-turn :test #'equal))
          (run-hook-with-args 'opencode-file-edited-functions
                              file (cdr file-ranges)))))))

(defvar opencode--files-finished-editing-current nil
  "Current `opencode-files-finished-editing-functions' run context.")

(defvar opencode--files-finished-editing-queue nil
  "Queued `opencode-files-finished-editing-functions' run contexts.")

(defun opencode-report-diagnostic (message)
  "Queue diagnostic MESSAGE for the active finished-editing hook."
  (unless opencode--files-finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
  (when (and message (not (string= message "")))
    (push message
          (plist-get opencode--files-finished-editing-current :diagnostics))))

(defun opencode-hook-finished ()
  "Mark the active finished-editing hook as complete."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (unless (plist-get context :active-hook)
      (error "`opencode-hook-finished' called without an active hook"))
    (plist-put context :active-hook nil)
    (opencode--run-next-files-finished-editing-hook)))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless opencode--files-finished-editing-current
    (when opencode--files-finished-editing-queue
      (let ((context (pop opencode--files-finished-editing-queue)))
        (setq opencode--files-finished-editing-current context)
        (opencode--run-next-files-finished-editing-hook)))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setq opencode--files-finished-editing-current nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "No active finished-editing hook context"))
    (when-let* ((buffer (plist-get context :session-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if-let* ((hook (car (plist-get context :hooks))))
              (let (result errored)
                (plist-put context :hooks (cdr (plist-get context :hooks)))
                (plist-put context :active-hook hook)
                (condition-case err
                    (setq result (funcall hook (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq opencode--files-finished-editing-current context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq opencode--files-finished-editing-current context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let* ((hooks (append opencode-files-finished-editing-functions
                                   opencode-project-files-finished-editing-functions)))
          (setq opencode--files-finished-editing-queue
                (append opencode--files-finished-editing-queue
                        (list (list :session-buffer (current-buffer)
                                    :files files
                                    :hooks hooks
                                    :diagnostics nil
                                    :active-hook nil))))
          (opencode--maybe-start-files-finished-editing-hook))))))

(defun opencode--indent-files (files)
  "Indent FILES with Emacs."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer))))))

(defun opencode-run-command-diagnostic (command)
  "Run COMMAND asynchronously and report diagnostics.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " ")))
    (condition-case err
        (progn
          (make-process
           :name command-name
           :buffer buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (unwind-protect
                   (unless (zerop (process-exit-status process))
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string)))))
                       (opencode-report-diagnostic
                        (if (equal output "")
                            (format "`%s` failed" command-string)
                          (format "`%s` failed:\n\n%s"
                                  command-string
                                  output)))))
                 (kill-buffer buffer)
                 (opencode-hook-finished)))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        (format "Failed to start `%s`: %s"
                command-string
                (error-message-string err)))
       nil))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch")
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            append (list "-l" file))
   (list "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files)
  "Run pytest with uv and report diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (_files)
  "Run ERT and report diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)))

(defun opencode--selection-change-hook (&optional frame)
  "Hook to remove session for FRAME from the alerted sessions list when it's visited.
Also prompts for pending questions or permissions if any."
  (with-current-buffer (window-buffer (frame-selected-window frame))
    (when opencode-session-id
      (setf opencode-alerted-sessions
            (cl-delete-if (lambda (session)
                            (string= opencode-session-id
                                     (alist-get 'id session)))
                          opencode-alerted-sessions)
            opencode-last-session-buffer (current-buffer))
      ;; Handle pending questions when buffer becomes active
      (when opencode-session-pending-questions
        (let ((pending opencode-session-pending-questions))
          (setq opencode-session-pending-questions nil)
          (opencode--prompt-questions (car pending) (cdr pending))))
      ;; Handle pending permissions when buffer becomes active
      (when opencode-session-pending-permission
        (run-at-time 0 nil #'opencode-respond-permission)))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

(defun opencode--handle-message (data)
  "Handle decoded message DATA from opencode server."
  (let* ((msg-type (intern (alist-get 'type data)))
         (properties (alist-get 'properties data)))
    (unless (memq msg-type '(file.watcher.updated session.diff sync))
      (opencode--log-event "MESSAGE" data)
      (let-alist properties
        (cl-case msg-type
          (tui.toast.show (opencode--toast-show properties))
          (session.idle
           (unless (opencode--defer-files-finished-editing-hook
                    .sessionID)
             (opencode--maybe-run-files-finished-editing-hook .sessionID))
           (opencode-api-session (.sessionID)
               session
             (opencode--with-session-buffer .sessionID
               (opencode--show-prompt)
               (unless (or
                        (opencode--buffer-active-p (current-buffer))
                        ;; don't show alert for subagent sessions
                        (alist-get 'parentID session))
                 (opencode--toast-show `((title . "OpenCode Finished")
                                         (message . ,(alist-get 'title session))
                                         (variant . "success")))
                 (push session opencode-alerted-sessions)))))
          (session.status (pcase .status.type
                            ((or "busy" "idle")
                             (opencode-session--set-status .sessionID .status.type))
                            ("retry"
                             (opencode-api-session (.sessionID)
                                 session
                               (opencode--toast-show `((title . ,(concat "OpenCode: "
                                                                         (alist-get 'title session)))
                                                       (message . ,(format "%s\n\nRetry #%d"
                                                                           .status.message
                                                                           .status.attempt))
                                                       (variant . "warning")))))))
          ((session.created session.updated session.deleted)
           (when (eq 'session.deleted msg-type)
             (run-hook-with-args 'opencode-session-deleted-functions .info.id))
           (dolist (buffer (map-elt opencode--session-control-buffers .info.projectID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-sessions-redisplay))))
           (opencode--with-session-buffer .info.id
             (cl-case msg-type
               (session.updated (rename-buffer (format "*OpenCode: %s*" .info.title) t))
               (session.deleted (delete-process)))))
          (session.error (opencode-session--display-error .sessionID .error.data.message))
          (message.part.updated
           (opencode--maybe-run-file-edited-hook .part)
           (opencode-session--update-part .part.id .part .delta .part.type))
          (message.part.delta (opencode-session--update-part
                               .partID properties .delta
                               (gethash .partID opencode-part-type)))
          (message.updated (opencode-session--message-updated .info))
          (permission.asked
           (opencode--permission-request
            .id .sessionID .permission
            .metadata
            (seq-into .patterns 'list)
            (seq-into .always 'list)))
          (question.asked
           (opencode--question-request .id .sessionID .questions))
          (otherwise (opencode--log-event "WARNING" "unhandled message type")))))))

(defun opencode--handle-global-event (event)
  "Handle a global wrapper EVENT from opencode server."
  (let* ((data (json-read-from-string (plz-event-source-event-data event)))
         (directory (alist-get 'directory data))
         (payload (alist-get 'payload data)))
    (if (and directory payload)
        (with-current-buffer (process-buffer opencode--event-subscription)
          (let ((default-directory (opencode--normalize-directory directory)))
            (opencode--handle-message payload)))
      (unless (string= "server.heartbeat" (alist-get 'type payload))
        (opencode--log-event "WARNING GLOBAL EVENT" data)))))

(defun opencode--subscribe-global-events ()
  "Subscribe to the global opencode event stream."
  (setq opencode--event-subscription
        (plz-media-type-request
          'get (concat opencode-api-url "/global/event")
          :as `(media-types
                ((text/event-stream
                  . ,(plz-event-source:text/event-stream
                      :events `((open . ,(lambda (event)
                                           (opencode--log-event "OPEN" event)))
                                (message . opencode--handle-global-event)
                                (close . opencode-disconnect))))))
          :headers (delq nil (list (opencode--auth-header)))
          :then 'opencode-disconnect
          :else 'opencode-disconnect))
  (set-process-query-on-exit-flag opencode--event-subscription nil))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (when (process-live-p opencode--event-subscription)
    (kill-process opencode--event-subscription))
  (when (process-live-p opencode--process)
    (set-process-sentinel opencode--process nil)
    (kill-process opencode--process))
  (setq opencode--event-subscription nil
        opencode--slash-commands-by-directory nil))

(defun opencode--fetch-agents (callback)
  "Fetch available agents from server and filter out hidden agents.
Run CALLBACK once fetched."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-filter (lambda (agent)
                        (opencode--json-falsy (alist-get 'hidden agent)))
                      agents))
    (funcall callback)))

(cl-defun opencode-new-session (&key title callback initial-prompt)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created.
If INITIAL-PROMPT is given, it is sent after CALLBACK is called."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode-autoconnect
   (lambda ()
     (opencode--download-slash-commands default-directory)
     (opencode-api-create-session (if title
                                      `((title . ,title))
                                    (make-hash-table))
         session
       (opencode-open-session
        session
        :callback (lambda (session)
                    (when callback
                      (funcall callback session))
                    (when initial-prompt
                      (goto-char (point-max))
                      (insert initial-prompt)
                      (comint-send-input))))))))

(defun opencode-fix-flycheck ()
  "Start a new opencode session to fix the current buffer's Flycheck errors."
  (interactive)
  (unless (bound-and-true-p flycheck-mode)
    (user-error "Flycheck is not enabled in this buffer"))
  (let* ((default-directory (if-let* ((proj (project-current)))
                                (project-root proj)
                              default-directory))
         (prompt (or (opencode--flycheck-fix-prompt default-directory)
                     (user-error "No Flycheck errors in this buffer"))))
    (opencode-new-session :initial-prompt prompt)))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let ((mcp (opencode--annotated-completion
                "MCP: "
                (cl-loop for mcp in mcps
                         for (mcp-name . mcp-info) = mcp
                         collect (list (symbol-name mcp-name)
                                       mcp-name
                                       (pcase (alist-get 'status mcp-info)
                                         ("connected" "🟢 connected")
                                         ("disabled"  "⚪ disabled")
                                         ("failed"    "🔴 failed")))))))
      (pcase (map-nested-elt mcps `(,mcp status))
        ("connected" (opencode-api-disable-mcp (mcp)
                         _res
                       (message "Disabled %s" mcp)))
        ("disabled" (opencode-api-enable-mcp (mcp)
                        _res
                      (message "Enabled %s" mcp)))))))

(defun opencode-fork-session ()
  "Fork the current session from the message at point.
Creates a new session starting from the current user message.
If point is before the first prompt, creates a new session instead."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-fork-session (opencode-session-id)
            `((messageID . ,message-id))
            session
          (opencode-open-session-same-window session))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Select a message to revert in the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-revert-message (opencode-session-id)
            `((messageID . ,message-id))
            result
          (message
           (if result
               "Reverted edits after message"
             "Failed to revert message")))
      (user-error "No user message at point"))))

(defun opencode-delete-message ()
  "Delete the message at point from the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-exchange (user-id assistant-id)
    (if (and user-id assistant-id)
        (opencode-api-delete-message (opencode-session-id assistant-id)
            assistant-result
          (unless assistant-result
            (error "Failed to delete assistant message"))
          (opencode-api-delete-message (opencode-session-id user-id)
              user-result
            (unless user-result
              (error "Failed to delete user message"))
            (opencode--delete-message-at-point)))
      (user-error "No message with assistant response at point"))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      result
    (message
     (if result
         "Restored all edits in session"
       "Failed to restore edits"))))

(defun opencode--download-slash-commands (directory)
  "Download slash commands for DIRECTORY."
  (setf directory (opencode--normalize-directory directory))
  (unless (assoc directory opencode--slash-commands-by-directory)
    (let ((default-directory directory))
      (opencode-api-commands commands
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)))))

(provide 'opencode)
;;; opencode.el ends here

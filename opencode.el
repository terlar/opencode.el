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
      (plz 'get (format "http://%s:%d/api/info" opencode-host opencode-port)
        :headers (delq nil (list (opencode--auth-header)))
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
        (let ((process-environment (cons
                                    (format "OPENCODE_PASSWORD=%s"
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
     (when (string-search "server listening on" output)
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
  (setq opencode--slash-commands-by-directory nil
        opencode--location-by-directory nil)
  (opencode--subscribe-events)
  (opencode--fetch-models (lambda () (opencode--fetch-agents on-connect)))
  (message "Connected to %s" opencode-api-url))

(defun opencode-open-project (directory)
  "Open the session control buffer for the project of DIRECTORY.
Worktrees of a project share one, and new sessions started from it go in the
directory it was last opened for."
  (let* ((directory (file-name-as-directory directory))
         (default-directory directory))
    (opencode-api--cached-location
     (lambda (location)
       (let-alist location
         (with-current-buffer
             (get-buffer-create
              (format "*OpenCode Sessions: %s*"
                      (if (equal .project.id "global")
                          "global"
                        (file-name-nondirectory
                         (directory-file-name .project.canonical)))))
           (unless (derived-mode-p 'opencode-session-control-mode)
             (opencode-session-control-mode)
             (push (current-buffer) opencode--session-control-buffers))
           (setq default-directory directory
                 opencode-session-control-project-id .project.id)
           (opencode-sessions-redisplay)
           (pop-to-buffer (current-buffer))))))))

(defun opencode-reload-locations ()
  "Make the server resolve the project of each directory again.
Run this after a directory's project changes, such as on `git init', its
first commit or adding a remote."
  (interactive)
  (opencode-api-reload-locations _
    :else (lambda (error-msg)
            ;; It still reloads the others when a directory's been deleted
            (setq opencode--location-by-directory nil)
            (message "%s" error-msg))
    (setq opencode--location-by-directory nil)
    (message "Reloaded opencode locations")))

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
                  for canonical = (alist-get 'canonical project)
                  when canonical
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 canonical)
                                canonical
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

(defun opencode--maybe-run-file-edited-hook (session-id tool input metadata)
  "Record files edited by a completed TOOL call and run per-file hooks.
INPUT is the input of the call in SESSION-ID, and METADATA its result metadata."
  (dolist (file-ranges
           (pcase tool
             ("write"
              (when-let* ((path (alist-get 'path input)))
                (list (cons path nil))))
             ((or "edit" "patch")
              ;; The patch tool's diff headers have absolute paths, so trust .file
              (mapcar (lambda (file)
                        (cons (alist-get 'file file)
                              (mapcan (lambda (entry) (copy-sequence (cdr entry)))
                                      (opencode-diff->source-line-ranges
                                       (alist-get 'patch file)))))
                      (alist-get 'files metadata)))))
    (when-let* ((file (opencode--resolve-file-reference (car file-ranges))))
      (opencode--with-session-buffer session-id
        (cl-pushnew file opencode--files-edited-this-turn :test #'equal))
      (run-hook-with-args 'opencode-file-edited-functions
                          file (cdr file-ranges)))))

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
          (opencode--prompt-questions pending)))
      ;; Handle pending permissions when buffer becomes active
      (when opencode-session-pending-permission
        (run-at-time 0 nil #'opencode-respond-permission)))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

(defconst opencode--event-types
  '("session.created" "session.deleted" "session.renamed" "session.archived"
    "session.unarchived" "session.moved" "session.forked"
    "session.agent.selected" "session.model.selected"
    "session.execution.started" "session.execution.succeeded"
    "session.execution.failed" "session.execution.interrupted"
    "session.step.started" "session.step.ended" "session.step.failed"
    "session.text.delta" "session.text.ended"
    "session.reasoning.delta" "session.reasoning.ended"
    "session.tool.input.started" "session.tool.called"
    "session.tool.success" "session.tool.failed"
    "session.retry.scheduled" "session.shell.ended"
    "session.compaction.started" "session.compaction.ended"
    "session.compaction.failed"
    "permission.asked" "permission.replied"
    "form.created" "form.replied" "form.cancelled" "tui.toast.show"
    "command.updated")
  "Types of the events to subscribe to.")

(defun opencode--redisplay-session-control-buffers ()
  "Refresh all live session control buffers."
  (setq opencode--session-control-buffers
        (seq-filter #'buffer-live-p opencode--session-control-buffers))
  (dolist (buffer opencode--session-control-buffers)
    (with-current-buffer buffer
      (opencode-sessions-redisplay))))

(defun opencode--session-finished (session-id)
  "Handle the end of SESSION-ID's execution."
  (opencode-session--set-status session-id "idle")
  (unless (opencode--defer-files-finished-editing-hook session-id)
    (opencode--maybe-run-files-finished-editing-hook session-id))
  (opencode-api-session (session-id)
      session
    (opencode--with-session-buffer session-id
      (opencode--show-prompt)
      (unless (or
               (opencode--buffer-active-p (current-buffer))
               ;; don't show alert for subagent sessions
               (alist-get 'parentID session))
        (opencode--toast-show `((title . "OpenCode Finished")
                                (message . ,(opencode--session-title session))
                                (variant . "success")))
        (push session opencode-alerted-sessions)))))

(defun opencode--handle-message (event)
  "Handle decoded EVENT from opencode server."
  (let ((type (intern (alist-get 'type event)))
        (data (alist-get 'data event)))
    (opencode--log-event "MESSAGE" event)
    (let-alist data
      (let ((block-key (list (if (memq type '(session.text.delta session.text.ended))
                                 'text
                               'reasoning)
                             .assistantMessageID .ordinal))
            (tool-key (opencode--tool-key .assistantMessageID .id)))
        (cl-case type
          (server.connected (opencode--stream-connected))
          (tui.toast.show (opencode--toast-show data))
          (session.execution.started
           (opencode-session--set-status .sessionID "busy"))
          (session.execution.succeeded (opencode--session-finished .sessionID))
          (session.execution.interrupted
           ;; On a real shutdown the drain resumes after the restart, but the
           ;; server also reports any interrupt without a reason this way, like
           ;; rejecting a permission, and then nothing follows.
           (if (equal .reason "shutdown")
               (opencode--unless-session-active
                .sessionID (lambda () (opencode--session-finished .sessionID)))
             (opencode--session-finished .sessionID)))
          (session.execution.failed
           (opencode-session--display-error .sessionID .error.message)
           (opencode--session-finished .sessionID))
          (session.step.started
           (opencode--with-session-buffer .sessionID
             (setq opencode--current-block nil
                   opencode--blocks-with-output nil)))
          (session.step.ended
           (opencode--with-session-buffer .sessionID
             (opencode--align-finished-tables t)
             (setq opencode-session-tokens (opencode--tokens-total .tokens))
             (force-mode-line-update)
             (when (equal "stop" .finish)
               (opencode--maybe-insert-block-spacing))))
          (session.step.failed
           (opencode--with-session-buffer .sessionID
             (opencode--align-finished-tables t)))
          (session.retry.scheduled
           (opencode-api-session (.sessionID)
               session
             (opencode--toast-show
              `((title . ,(concat "OpenCode: " (opencode--session-title session)))
                (message . ,(format "%s\n\nRetry #%d" .error.message .attempt))
                (variant . "warning")))))
          (session.renamed
           (opencode--with-session-buffer .sessionID
             (rename-buffer (format "*OpenCode: %s*" .title) t))
           (opencode--redisplay-session-control-buffers))
          (session.deleted
           (run-hook-with-args 'opencode-session-deleted-functions .sessionID)
           (opencode--redisplay-session-control-buffers)
           (opencode--with-session-buffer .sessionID
             (delete-process)))
          ((session.created session.archived session.unarchived
                            session.moved session.forked)
           (opencode--redisplay-session-control-buffers))
          (session.agent.selected
           (opencode--with-session-buffer .sessionID
             (unless (equal .agent opencode--server-agent)
               (setq opencode--server-agent .agent)
               (opencode--adopt-selection
                .agent (alist-get 'model opencode-session-agent)))))
          (session.model.selected
           (opencode--with-session-buffer .sessionID
             (opencode--adopt-selection nil .model)))
          ((session.text.delta session.reasoning.delta)
           (opencode-session--block-delta .sessionID block-key .delta))
          ((session.text.ended session.reasoning.ended)
           (opencode-session--block-ended .sessionID block-key .text))
          (session.tool.input.started
           (puthash tool-key (list :name .name) opencode--tool-calls))
          (session.tool.called
           (puthash tool-key
                    (plist-put (gethash tool-key opencode--tool-calls) :input .input)
                    opencode--tool-calls)
           (opencode--with-session-buffer .sessionID
             (opencode-session--display-tool-call tool-key)))
          ((session.tool.success session.tool.failed)
           (opencode-session--tool-finished
            .sessionID tool-key
            `((status . ,(if (eq type 'session.tool.success) "completed" "error"))
              (error . ,.error) (content . ,.content) (metadata . ,.metadata))))
          (session.shell.ended
           (opencode--with-session-buffer .sessionID
             (opencode--insert-shell-output (or .output.output "")
                                            (opencode--shell-failed-p "shell" .shell))
             ;; A shell command isn't an execution, so no events follow to do so.
             (when (equal opencode-session-status "idle")
               (opencode--show-prompt))))
          (session.compaction.started
           (opencode--with-session-buffer .sessionID
             (opencode--insert-compaction-started)))
          (session.compaction.ended
           (opencode--with-session-buffer .sessionID
             (opencode--insert-compaction-summary .text)))
          (session.compaction.failed
           (opencode-session--display-error .sessionID .error.message))
          (permission.asked
           (opencode--permission-request
            .id .sessionID .action .metadata .resources .save .message))
          (permission.replied
           (opencode--permission-replied .sessionID .requestID .reply))
          (command.updated
           (opencode--slash-commands-updated
            (map-nested-elt event '(location directory))))
          (form.created (opencode--form-request .form))
          ((form.replied form.cancelled)
           (opencode--form-settled .sessionID .id))
          (otherwise (opencode--log-event "WARNING" "unhandled message type")))))))

(defun opencode--handle-event (event)
  "Handle EVENT from the opencode server's event stream."
  (let* ((data (json-parse-string (plz-event-source-event-data event)
                                  :array-type 'list
                                  :object-type 'alist))
         (directory (map-nested-elt data '(location directory)))
         (directory-for-event (if (stringp directory)
                                  (opencode--normalize-directory directory)
                                default-directory)))
    ;; Responses to requests made here run in the buffer that made them, so
    ;; use one that outlives the stream.  An error here shouldn't take the
    ;; stream's process filter down with it.
    (with-current-buffer (get-buffer-create " *opencode-events*")
      (let ((default-directory directory-for-event))
        (condition-case err
            (opencode--handle-message data)
          (error
           (message "opencode: error handling %s event: %s"
                    (alist-get 'type data) (error-message-string err))
           (opencode--log-event "WARNING EVENT HANDLER"
                                (format "%S failed: %s" data (error-message-string err)))))))))

(defvar opencode--stream-retries-left nil
  "Number of attempts left to resubscribe, or nil if the stream isn't recovering.")

(defvar opencode--stream-retry-timer nil
  "Timer for the next attempt to resubscribe.")

(defun opencode--subscribe-events ()
  "Subscribe to the opencode event stream."
  (let* (stream
         (ended (lambda (&optional event)
                  (when (eq stream opencode--event-subscription)
                    (opencode--stream-ended event)))))
    (setq stream (plz-media-type-request
                   'get (concat opencode-api-url "/api/event?"
                                (mapconcat (lambda (type) (concat "types=" type))
                                           opencode--event-types "&"))
                   :as `(media-types
                         ((text/event-stream
                           . ,(plz-event-source:text/event-stream
                               :events `((open . ,(lambda (event)
                                                    (opencode--log-event "OPEN" event)))
                                         (message . opencode--handle-event)
                                         (close . ,ended))))))
                   :headers (delq nil (list (opencode--auth-header)))
                   :then ended
                   :else ended)
          opencode--event-subscription stream)
    (set-process-query-on-exit-flag stream nil)))

(defun opencode--stream-ended (event)
  "Try to recover from the event stream ending with EVENT.
The server may still be fine, so it's left running."
  (opencode--log-event "STREAM ENDED" event)
  ;; A failed stream reports it more than once
  (unless (memq opencode--stream-retry-timer timer-list)
    (when (process-live-p opencode--event-subscription)
      (delete-process opencode--event-subscription))
    (setq opencode--stream-retries-left (or opencode--stream-retries-left 5))
    (if (zerop opencode--stream-retries-left)
        (progn
          (opencode-disconnect "resubscribing failed")
          (message "Lost connection to opencode at %s" opencode-api-url))
      (cl-decf opencode--stream-retries-left)
      (setq opencode--stream-retry-timer
            (run-at-time 2 nil #'opencode--subscribe-events)))))

(defun opencode--stream-connected ()
  "Handle the event stream connecting, rebuilding what it missed if recovering."
  (when opencode--stream-retries-left
    (setq opencode--stream-retries-left nil)
    (message "Reconnected to opencode at %s" opencode-api-url)
    (clrhash opencode--tool-calls)
    (opencode--redisplay-session-control-buffers)
    (maphash (lambda (_session-id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (opencode--rebuild-session))))
             opencode-session-buffers)))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (when (timerp opencode--stream-retry-timer)
    (cancel-timer opencode--stream-retry-timer))
  (let ((stream opencode--event-subscription))
    (setq opencode--event-subscription nil)
    (when (process-live-p stream)
      (delete-process stream)))
  (when (process-live-p opencode--process)
    (set-process-sentinel opencode--process nil)
    (kill-process opencode--process))
  (setq opencode--stream-retries-left nil
        opencode--stream-retry-timer nil
        opencode--slash-commands-by-directory nil
        opencode--location-by-directory nil))

(defun opencode--fetch-models (callback)
  "Fetch enabled models and the default model from server.
Run CALLBACK once fetched."
  (opencode-api-models models
    (setq opencode-models
          (seq-remove (lambda (model)
                        (opencode--json-falsy (alist-get 'enabled model)))
                      models))
    (opencode-api-default-model default
      (unless (opencode--find-model opencode-last-model)
        (setq opencode-last-model
              (unless (opencode--json-falsy default)
                (opencode--model-ref default))))
      (funcall callback))))

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
     (opencode-api-create-session
         `((location . ((directory . ,(directory-file-name
                                       (expand-file-name default-directory)))))
           ,@(when title
               `((title . ,title))))
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
    (let* ((name (opencode--annotated-completion
                  "MCP: "
                  (cl-loop for mcp in mcps
                           for name = (alist-get 'name mcp)
                           collect (list name
                                         name
                                         (pcase (map-nested-elt mcp '(status status))
                                           ("connected"  "🟢 connected")
                                           ("disabled"   "⚪ disabled")
                                           ("failed"     "🔴 failed")
                                           ("needs_auth" "🟡 needs auth")
                                           ("pending"    "⏳ pending"))))))
           (mcp (seq-find (lambda (mcp) (equal name (alist-get 'name mcp))) mcps)))
      (when mcp
        (if (equal "connected" (map-nested-elt mcp '(status status)))
            (opencode-api-disable-mcp (name)
                _res
              (message "Disabled %s" name))
          (opencode-api-enable-mcp (name)
              _res
            (message "Enabled %s" name)))))))

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
            `((before . ,message-id))
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
            _result
          (message "Reverted edits after message"))
      (user-error "No user message at point"))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      _result
    (message "Restored all edits in session")))

(defun opencode--slash-commands-updated (directory)
  "Refetch the slash commands of DIRECTORY if they were downloaded."
  (let ((directory (opencode--normalize-directory directory)))
    (when (assoc directory opencode--slash-commands-by-directory)
      (opencode--download-slash-commands directory t))))

(provide 'opencode)
;;; opencode.el ends here

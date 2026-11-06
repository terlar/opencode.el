;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'comint)
(require 'hideshow)
(require 'magit)
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-format-tool-calls)
(require 'project)
(require 'url-util)
(require 'vtable)
(require 'yank-media)

(defcustom opencode-shown-outputs '(shell-non-zero error edit patch)
  "Outputs in session buffers to show unfolded when first displayed.
A list of symbols: `reasoning', a tool name like `shell' or `grep',
`shell-non-zero' to show shell tool output only if the command failed, or
`error' to show the error of any tool call that failed.
Shown outputs can still be folded manually with `hs-toggle-hiding'."
  :type '(repeat (choice (const reasoning)
                         (const shell-non-zero)
                         (const error)
                         (symbol :tag "Tool name")))
  :group 'opencode)

(defun opencode--session-control-command (fun)
  "Return a command that calls FUN with the session at point."
  (lambda ()
    (:documentation (format "Call `%s' on the session at point." fun))
    (interactive)
    (funcall fun (or (vtable-current-object)
                     (user-error "No session at point")))))

(defvar opencode-session-control-mode-map
  (define-keymap
    ;; needed here instead of vtable :actions, as emojify-mode keymap was taking
    ;; precedence over vtable's keymap
    "RET" (opencode--session-control-command #'opencode-open-session-same-window)
    "o" (opencode--session-control-command #'opencode-open-session-same-window)
    "R" (opencode--session-control-command #'opencode-rename-session)
    "x" (opencode--session-control-command #'opencode-kill-session)
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "v" 'opencode-session-control-toggle-verbose))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

(defvar-local opencode-session-control-project-id nil
  "Id of the project whose sessions the session control buffer displays.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "C-c Y" 'opencode-copy-conversation
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c C" 'opencode-compact-session
    "C-c t" 'opencode-scroll-to-last-response-start
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-subagent))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
    (kbd "RET") (opencode--session-control-command #'opencode-open-session)
    "o" (opencode--session-control-command #'opencode-open-session)
    "R" (opencode--session-control-command #'opencode-rename-session)
    "x" (opencode--session-control-command #'opencode-kill-session)
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "v" 'opencode-session-control-toggle-verbose
    "gv" 'opencode-session-control-toggle-verbose)
  (evil-define-key* 'insert opencode-session-mode-map
    "@" 'opencode-add-subagent
    "/" 'opencode-insert-slash-command))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode--table-scan-marker nil
  "Marker at the unprocessed Markdown in the current assistant text part.")

(defvar-local opencode--reasoning-start nil
  "Start of the streaming reasoning block, to fold once the next block starts.")

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar-local opencode-session-agents nil
  "List of agents for the current session.
Buffer local so we can configure models and variants per agent per session.")

(defvar-local opencode--server-agent nil
  "Id of the agent the server has selected for this buffer's session.")

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, a plist of :session-id, :form-id and :questions, a vector.")

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defvar-local opencode--temp-files nil
  "Temporary files to delete after sending or killing a session buffer.")

(defvar-local opencode--files-edited-this-turn nil
  "Files edited by opencode during the current turn.")

(defvar-local opencode--skip-files-finished-editing-hook-on-next-idle nil
  "Non-nil means the next session idle should not run finished-editing hooks.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar opencode--tool-calls
  (make-hash-table :test 'equal)
  "A mapping of `opencode--tool-key's to plists of in-progress tool calls.
They have :name and :input, as finishing events don't carry them, and
:marker at the end of the call if it's displayed, where its output goes.
Global so that file edit hooks also run for sessions with no buffer.")

(defvar-local opencode--current-block nil
  "(TYPE . KEY) of the block last output in this step, or nil.
TYPE is text, reasoning, tool or compaction.  KEY identifies the block.")

(defvar-local opencode--blocks-with-output nil
  "KEYs of the text and reasoning blocks of this step that have output.")

(defvar-local opencode--replayed-unfinished-messages nil
  "Ids of the messages still being generated when they were replayed.
Their blocks had output in the replay, without this client seeing any delta.")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(defmacro opencode--with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer for SESSION-ID."
  (declare (indent 1))
  `(when-let* ((buffer (gethash ,session-id opencode-session-buffers)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

(defun opencode--selectable-agent-p (agent)
  "Return non-nil if AGENT can be the agent of a session."
  (not (equal "subagent" (alist-get 'mode agent))))

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((id (alist-get 'id opencode-session-agent))
         (agents opencode-session-agents)
         (pos (and id (cl-position-if
                       (lambda (agent)
                         (string= id (alist-get 'id agent)))
                       agents))))
    (when pos
      (setcar (nthcdr pos agents) opencode-session-agent)
      (when-let* ((next (seq-find
                         #'opencode--selectable-agent-p
                         (append (nthcdr (1+ pos) agents)
                                 (seq-take agents (1+ pos))))))
        (setq opencode-session-agent next))))
  (force-mode-line-update))

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions ()
  "Run `consult-line-multi' across all open opencode sessions."
  (interactive)
  (if (require 'consult nil t)
      (progn
        (declare-function consult-line-multi "consult")
        (consult-line-multi '(:predicate opencode--session-buffer-p)))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (pop opencode-alerted-sessions))
    (message "No idle sessions")))

(declare-function opencode-autoconnect "opencode")

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api--cached-location
      (lambda (location)
        (opencode-api-root-sessions-for-project ((map-nested-elt location '(project id)))
            sessions
          (opencode--select-sessions "Session: "
                                     sessions
                                     (format "No sessions in %s" default-directory)
                                     :pop-to-buffer nil)))))))

(cl-defun opencode--select-sessions (prompt sessions no-sessions-message &key (pop-to-buffer t))
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list (opencode--session-title session)
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated)))))
       :pop-to-buffer pop-to-buffer)
    (message no-sessions-message)))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode--model-ref (model)
  "Return the Model.Ref for MODEL, a Model.Info."
  `((id . ,(alist-get 'id model))
    (providerID . ,(alist-get 'providerID model))))

(defun opencode--find-model (ref)
  "Return the enabled Model.Info that the Model.Ref REF refers to, or nil."
  (seq-find (lambda (model)
              (and (equal (alist-get 'providerID ref) (alist-get 'providerID model))
                   (equal (alist-get 'id ref) (alist-get 'id model))))
            opencode-models))

(defun opencode--collect-all-models ()
  "Collect all models from `opencode-models' as a list.
Each element is (display-name model-ref provider-id)."
  (mapcar (lambda (model)
            (list (alist-get 'name model)
                  (opencode--model-ref model)
                  (alist-get 'providerID model)))
          opencode-models))

(defun opencode--model-variant-p (model variant)
  "Return non-nil if MODEL, a Model.Info, has a variant with id VARIANT."
  (seq-find (lambda (candidate)
              (equal variant (alist-get 'id candidate)))
            (alist-get 'variants model)))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let* ((ref (opencode--annotated-completion
                    "Model: "
                    (opencode--collect-all-models))))
    (let ((variant (map-nested-elt opencode-session-agent '(model variant))))
      (when (opencode--model-variant-p (opencode--find-model ref) variant)
        (setf (alist-get 'variant ref) variant))
      (setf (alist-get 'model opencode-session-agent) ref
            opencode-last-model (copy-alist ref)))))

(defun opencode--current-model ()
  "Return the active model for this session, as a Model.Info."
  (opencode--find-model (alist-get 'model opencode-session-agent)))

(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (let ((variant (opencode--annotated-completion
                        "Variant: "
                        (cl-loop for variant in variants
                                 for id = (alist-get 'id variant)
                                 collect (list id id
                                               (format "%s" (or (alist-get 'settings variant)
                                                                "")))))))
          (setf (alist-get 'model opencode-session-agent)
                (copy-alist (alist-get 'model opencode-session-agent)))
          (setf (alist-get 'variant (alist-get 'model opencode-session-agent))
                variant))
      (message "No variants"))))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (opencode-api-session ((alist-get 'parentID session))
        parent
      (opencode-open-session parent))))

(defun opencode--download-slash-commands (directory &optional force)
  "Download slash commands for DIRECTORY, unless already downloaded or FORCE."
  (setf directory (opencode--normalize-directory directory))
  (when (or force (not (assoc directory opencode--slash-commands-by-directory)))
    (let ((default-directory directory))
      (opencode-api-commands commands
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)))))

(defun opencode-insert-slash-command ()
  "Insert an opencode slash command."
  (interactive)
  (if (= (point) (cdr comint-last-prompt))
      (let* ((directory (opencode--normalize-directory default-directory))
             (commands (alist-get directory opencode--slash-commands-by-directory
                                  nil nil #'string=))
             (command (opencode--annotated-completion
                       "Slash command: "
                       (cl-loop for command in commands
                                collect (let-alist command
                                          (list
                                           .name
                                           .name
                                           .description))))))
        (when command
          (insert (concat "/" command))))
    (call-interactively #'self-insert-command)))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (let-alist opencode-session-agent
    (let* ((agent (pcase (or .name .id "-")
                    ("Planner-Sisyphus" "Planner")
                    (name name)))
           (model (opencode--current-model))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (limit (map-nested-elt model '(limit context)))
           (context-used (if (and (numberp limit) (> limit 0))
                             (* 100 (/ (float opencode-session-tokens) limit))
                           0)))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat (alist-get 'name model)
                        (when (and .model.variant
                                   (not (equal .model.variant "default")))
                          (propertize (format " %s" .model.variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode--session-title (session)
  "Return the title of SESSION, which a new session may not have yet."
  (or (alist-get 'title session) "Untitled"))

(defun opencode--session-status-icon (session)
  "Return an icon for SESSION's execution state, or nil if nothing to show.
A session is busy when its last execution started after the last one
ended, like the OpenCode app's inbox."
  (let-alist session
    (cond ((and .time.busy (or (null .time.idle) (> .time.busy .time.idle)))
           "⏳")
          ((equal .outcome "failed") "❌"))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (opencode--with-session-buffer session-id
    (setq opencode-session-status status)
    (force-mode-line-update)))

(defun opencode--defer-files-finished-editing-hook (session-id)
  "Return and clear SESSION-ID's next-idle finished-editing hook skip flag."
  (opencode--with-session-buffer session-id
    (prog1 opencode--skip-files-finished-editing-hook-on-next-idle
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil))))

(defun opencode--fontify-code-block (mode beg end)
  "Collect MODE faces for BEG..END without sharing session text properties."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))

(defun opencode--fontify-markdown (start end &optional loudly)
  "Fontify Markdown between START and END, optionally LOUDLY.
Isolate conventional code-block fontification from session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly)))
  ;; An overlay's background would hide the line backgrounds of diff faces.
  ;; Fontifying just a fence line resets its block's overlay, so look past it.
  (dolist (overlay (overlays-in (save-excursion (goto-char start) (pos-bol 0))
                                (save-excursion (goto-char end) (pos-bol 3))))
    (when (eq (overlay-get overlay 'markdown-ts-code-block-mode) 'diff-mode)
      (overlay-put overlay 'face nil))))

(defvar evil-buffer-regexps)

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  ;; Set up Markdown's parser and font-lock without replacing comint as the
  ;; major mode (and losing its process, prompt, and input handling).
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (error "OpenCode sessions require the markdown and markdown-inline Tree-sitter grammars"))
  (markdown-ts-mode--initialize)
  ;; Markdown's conventional-mode helper uses an indirect buffer sharing all
  ;; session properties.  Mode initialization/cleanup there can interfere with
  ;; already-fontified Markdown.  Collect code faces in an isolated buffer.
  (setq-local font-lock-defaults
              '(nil nil nil nil
                (font-lock-fontify-syntactically-function
                 . opencode--fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode--fontify-markdown)
  ;; File-reference buttons are created by comint output, not font-lock,
  ;; and prompt labels are displayed with `display'.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo display)))
  (visual-line-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image)
  ;; A foldable block starts with a line whose first char has an
  ;; `opencode-fold' property: a marker at the end of the block.
  (setq-local hs-block-start-regexp "^."
              hs-block-end-regexp nil
              hs-looking-at-block-start-predicate
              (lambda () (and (get-text-property (point) 'opencode-fold)
                              (looking-at ".*")))
              hs-forward-sexp-function
              (lambda (_) (goto-char (get-text-property (point) 'opencode-fold)))
              hs-find-block-beginning-function #'opencode--fold-beginning
              hs-inside-comment-predicate #'ignore
              ;; comint re-marks output read-only, which would unfold it
              hs-set-up-overlay
              (lambda (ov)
                (overlay-put ov 'modification-hooks nil)
                ;; redisplay skips a `display' string after invisible
                ;; text, like markup hidden at the end of the first line
                (overlay-put ov 'before-string
                             (concat " " (overlay-get ov 'display)))
                (overlay-put ov 'display nil))
              hs-display-lines-hidden t)
  (hs-minor-mode)
  ;; Evil picks a buffer's initial state from its minor modes before its
  ;; major mode, so a minor mode like `hs-minor-mode' (normal state in
  ;; evil-collection) would override comint's insert state.  Buffer name
  ;; rules are checked first, so always start in insert state.
  (setq-local evil-buffer-regexps '(("" . insert))))

(defun opencode--fold-beginning ()
  "Move to the start of the foldable block containing point, if any.
Folds don't nest, so this is the nearest block start before point."
  (when-let* ((start (previous-single-property-change (1+ (point)) 'opencode-fold))
              (start (if (get-text-property start 'opencode-fold)
                         start
                       (previous-single-property-change start 'opencode-fold)))
              ((>= (get-text-property start 'opencode-fold) (point))))
    (goto-char start)))

(defun opencode--fold (beg &optional end shown)
  "Make the output from BEG to END foldable, except its first line.
END defaults to the process marker.  Fold it now unless SHOWN is non-nil."
  (with-silent-modifications
    (put-text-property beg (1+ beg) 'opencode-fold
                       (copy-marker (opencode--output-content-end end))))
  (unless shown
    (save-excursion
      (goto-char beg)
      (hs-hide-block-at-point))))

;; Markdown hides whole fence lines, including their newline, only in view mode.
;; this also allows us to call markdown-ts-table-align-table in the buffer
(derived-mode-add-parents 'opencode-session-mode '(markdown-ts-view-mode))

(defun opencode-yank-code-block ()
  "Yank the code inside the markdown code block under point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown)
                                     "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node
      (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode-copy-conversation ()
  "Copy the current conversation as a Markdown transcript.
The transcript is built from raw session messages, so it includes tool
output that isn't displayed in the session buffer, such as edit results.

Useful if you have separate usage limits for opencode vs webchat, so you can let
it gather context and start a plan in opencode, and then iterate on the plan
in webchat."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (message "Fetching conversation...")
  (opencode-api-all-messages (opencode-session-id)
      messages
    (let ((transcript (opencode--conversation-to-markdown messages)))
      (kill-new transcript)
      (message "Copied conversation (%d messages, %d characters)"
               (length messages)
               (length transcript)))))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      _result
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-buffer (current-buffer)))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--attachable-file-p (file)
  "Return non-nil if the model can be sent FILE.
The server silently drops all but directories, text, PDFs and some images."
  (or (file-directory-p file)
      (let ((bytes (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file nil 0 (* 1024 1024))
                     (buffer-string))))
        (or (string-match-p "\\`\\(?:\x89PNG\\|\xff\xd8\xff\\|GIF8\\|BM\\|%PDF-\\|RIFF[^z-a]\\{4\\}WEBP\\)"
                            bytes)
            (and (not (string-search "\0" bytes))
                 (not (memq 'eight-bit
                            (find-charset-string
                             (decode-coding-string
                              ;; the size limit can cut the last character in half
                              (replace-regexp-in-string "[\xc0-\xff][\x80-\xbf]*\\'" "" bytes)
                              'utf-8 t)))))))))

(defun opencode--insert-label (name part)
  "Insert a label showing NAME that sends PART with the prompt.
The label is a single character, so it is deleted as a unit, and undo
restores it together with PART."
  (insert (propertize "￼"
                      'display (propertize name 'face 'markdown-ts-code-span)
                      'opencode-label-name name
                      'opencode-part part
                      'rear-nonsticky t)))

(defun opencode--code-span (text)
  "Return TEXT as a Markdown code span."
  (let ((delimiter (make-string (1+ (opencode--longest-backtick-run text)) ?`))
        (padding (if (or (string-prefix-p "`" text)
                         (string-suffix-p "`" text))
                     " "
                   "")))
    (concat delimiter padding text padding delimiter)))

(defun opencode--input-text-and-parts (start end)
  "Return (TEXT . PARTS) for the input between START and END.
PARTS are those of the labels in the input, which TEXT shows as code."
  (let ((pos start)
        chunks
        parts)
    (while (< pos end)
      (if-let* ((name (get-text-property pos 'opencode-label-name)))
          (progn
            (push (opencode--code-span name) chunks)
            (push (get-text-property pos 'opencode-part) parts)
            (cl-incf pos))
        (let ((next (next-single-property-change pos 'opencode-part nil end)))
          (push (buffer-substring-no-properties pos next) chunks)
          (setq pos next))))
    (cons (apply #'concat (nreverse chunks))
          ;; a label yanked twice shares its part
          (delete-dups (nreverse parts)))))

(defun opencode--file-attachment (file name)
  "Return a label part attaching FILE as NAME."
  `(files . ((uri . ,(concat "file://"
                             (url-hexify-string (expand-file-name file)
                                                url-path-allowed-chars)))
             (name . ,name))))

(defun opencode--text-attachment (text name)
  "Return a label part attaching TEXT as NAME."
  `(files . ((uri . ,(concat "data:text/plain;base64,"
                             (base64-encode-string
                              (encode-coding-string text 'utf-8) t)))
             (name . ,name))))

(defun opencode--add-file-to-context (file &optional display-name)
  "Add FILE to context with optional DISPLAY-NAME."
  (unless (opencode--attachable-file-p file)
    (user-error "Can't attach %s: unsupported file type" file))
  (let ((display-name (or display-name
                          (opencode--relative-path-for-display file))))
    (opencode--insert-label display-name
                            (opencode--file-attachment file display-name))
    (insert " ")))

(defun opencode--image-extension (mime)
  "Return a file extension for image MIME."
  (or (cadr (split-string mime "[/;+]" t))
      "img"))

(defun opencode--yank-image (type data)
  "Add pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted into the current prompt"))
  (let* ((mime (symbol-name type))
         (extension (opencode--image-extension mime))
         (file (make-temp-file "opencode-image-" nil
                               (concat "." extension))))
    (condition-case error
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil file nil 'silent))
          (push file opencode--temp-files)
          (opencode--add-file-to-context file (file-name-nondirectory file)))
      (error
       (ignore-errors (delete-file file))
       (signal (car error) (cdr error))))))

(defun opencode--delete-temp-files (files)
  "Delete temporary FILES created by opencode."
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))

(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))

(defmacro with-last-opencode-session (&rest body)
  "Run BODY with the last used opencode session buffer active."
  `(if opencode-last-session-buffer
       (if (buffer-live-p opencode-last-session-buffer)
           (with-current-buffer opencode-last-session-buffer
             (unless (comint-after-pmark-p)
               (goto-char (point-max)))
             ,@body)
         (user-error "Last selected OpenCode session buffer is no longer active"))
     (user-error "Open an OpenCode session first")))

(defun opencode-add-file (&optional file prompt-directories)
  "Add a FILE to context, or prompt for file in current project.
With a prefix argument, show project directories instead of files."
  (interactive (list nil current-prefix-arg))
  (with-last-opencode-session
   (let* ((project (project-current t))
          (root (project-root project))
          (all-files (project-files project)))
     (opencode--add-file-to-context
      (or file
          (project--read-file-name
           project
           "Add to context"
           (if prompt-directories
               (delete root
                       (delete-dups
                        (cons
                         (concat root "./")
                         (mapcar #'file-name-directory all-files))))
             all-files)
           nil
           'file-name-history))))))

(defun opencode-add-file-dwim ()
  "If in Dired, add all marked files, or file at point if none marked.
Otherwise add the current buffer's file.
Otherwise prompt for file in current project."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (if-let* ((files (dired-get-marked-files)))
          (mapc #'opencode-add-file files)
        (if-let* ((file (dired-get-filename nil t)))
            (opencode-add-file file)
          (user-error "No file at point, and no marked files")))
    (opencode-add-file (buffer-file-name))))

(defun opencode-add-buffer-dwim ()
  "Add current buffer or prompt for one to add."
  (interactive)
  (let ((buffer (if opencode-session-id
                    (read-buffer "Add to context: ")
                  (buffer-name))))
    (with-last-opencode-session
     (opencode--insert-label
      buffer
      (opencode--text-attachment
       (with-current-buffer buffer
         (buffer-substring-no-properties (point-min) (point-max)))
       buffer))
     (insert " "))))

(defun opencode-add-region ()
  "Add the active region to context."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning)
                                                    (region-end))))
        (with-last-opencode-session
         (let ((name (format "region: %s"
                             (truncate-string-to-width
                              region 24 0 nil (truncate-string-ellipsis)))))
           (opencode--insert-label name (opencode--text-attachment region name)))
         (insert " ")))
    (user-error "No active region")))

(defun opencode-add-subagent ()
  "Insert a subagent mention into the current input."
  (interactive)
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
    (let ((subagents (cl-loop for agent in opencode-agents
                              when (string= "subagent" (alist-get 'mode agent))
                              collect agent)))
      (unless subagents
        (user-error "No subagents available"))
      (let ((name (opencode--annotated-completion
                   "Subagent: "
                   (cl-loop for agent in subagents
                            collect (let-alist agent
                                      (list .name .name .description))))))
        (delete-char -1)
        (opencode--insert-label (concat "@" name)
                                `(agents . ((name . ,name))))))))

(defun opencode--adopt-selection (agent model)
  "Make the local agent and model AGENT and MODEL, if non-nil."
  (when (and agent (not (equal agent (alist-get 'id opencode-session-agent))))
    (setq opencode-session-agent
          (or (seq-find (lambda (candidate)
                          (equal agent (alist-get 'id candidate)))
                        opencode-session-agents)
              ;; a hidden agent
              `((id . ,agent) (name . ,agent)))))
  (when model
    (setf (alist-get 'model opencode-session-agent) (copy-alist model)))
  (force-mode-line-update))

(defun opencode--send-failed (error-msg)
  "Display ERROR-MSG of input that wasn't accepted, and go back to a prompt.
No events will follow to do so."
  (opencode--insert-error error-msg)
  (opencode--show-prompt))

(defun opencode--sync-agent-and-model (callback)
  "Make the server select the local agent and model, then run CALLBACK.
The server ignores a switch to the same model, but a switch of agent always
records a message, so that's only done if it differs."
  (let ((agent (alist-get 'id opencode-session-agent))
        (model (alist-get 'model opencode-session-agent)))
    (cl-flet ((sync-model ()
                (if model
                    (opencode-api-session-model (opencode-session-id)
                        `((model . ,model))
                        _response
                      :else #'opencode--send-failed
                      (funcall callback))
                  (funcall callback))))
      (if (or (null agent) (equal agent opencode--server-agent))
          (sync-model)
        (opencode-api-session-agent (opencode-session-id)
            `((agent . ,agent))
            _response
          :else #'opencode--send-failed
          (setq opencode--server-agent agent)
          (sync-model))))))

(defun opencode--unless-session-active (session-id function)
  "Call FUNCTION unless the server is still running SESSION-ID."
  (opencode-api-sessions-active
    active
    (unless (assq (intern session-id) active)
      (funcall function))))

(defun opencode--prompt-body (text parts)
  "Return the body of a prompt with TEXT and the label PARTS."
  (let ((files (cl-loop for (kind . part) in parts
                        when (eq kind 'files) collect part))
        (agents (cl-loop for (kind . part) in parts
                         when (eq kind 'agents) collect part)))
    `((text . ,text)
      ,@(when files `((files . ,(vconcat files))))
      ,@(when agents `((agents . ,(vconcat agents)))))))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  ;; STRING is the buffer text of the input, where each label is one character
  (pcase-let ((`(,string . ,extra-parts)
               (opencode--input-text-and-parts
                comint-last-input-start
                (+ comint-last-input-start (length string))))
              (temp-files opencode--temp-files))
    (setf opencode--temp-files nil)
    (cond
     ((string-prefix-p "/" string)
      (let ((space-pos (seq-position string ?\s)))
        (opencode--sync-agent-and-model
         (lambda ()
           (opencode-api-session-command (opencode-session-id)
               `((name . ,(substring string 1 space-pos))
                 (text . ,(if space-pos
                              (substring string (1+ space-pos))
                            "")))
               _response
             :else #'opencode--send-failed
             ;; Commands can just run a callback, then no events follow to do so.
             (opencode--unless-session-active
              opencode-session-id
              (lambda ()
                (unless (equal opencode-session-status "busy")
                  (opencode--show-prompt))))))))
      (opencode--delete-temp-files temp-files))
     ((string-prefix-p "!" string)
      (opencode-api-session-shell (opencode-session-id)
          `((command . ,(substring string 1)))
          _response
        :else #'opencode--send-failed)
      (opencode--delete-temp-files temp-files))
     (t
      (opencode--sync-agent-and-model
       (lambda ()
         (opencode-api-session-prompt (opencode-session-id)
             (opencode--prompt-body string extra-parts)
             _result
           :else (lambda (error-msg)
                   (opencode--delete-temp-files temp-files)
                   (opencode--send-failed error-msg))
           (opencode--delete-temp-files temp-files))))))))

(defun opencode-session--send-synthetic-input (string)
  "Send STRING to the current opencode session.
Preserve any pending input context while sending STRING as a plain prompt."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending-prompt (copy-marker (process-mark process) t))
         (original-point (copy-marker (point) t))
         (prompt-start (opencode--session-process-position))
         start
         end)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char prompt-start)
              (setq start (point))
              (insert string)
              (setq end (point))))
          (set-marker (process-mark process) end)
          (let ((comint-last-input-start start)
                (comint-last-input-end end)
                (opencode--temp-files nil))
            (opencode--send-input process string)
            (opencode--maybe-insert-block-spacing)))
      (set-marker (process-mark process) pending-prompt)
      (goto-char original-point)
      (set-marker pending-prompt nil)
      (set-marker original-point nil))))

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((eq ?\n (char-before pos))
       (if (eq ?\n (char-before (1- pos))) "" "\n"))
      (t "")))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (let ((start (save-excursion
                 (goto-char (opencode--session-process-position))
                 (let ((inhibit-field-text-motion t))
                   (line-beginning-position)))))
    (comint-output-filter (get-buffer-process (current-buffer)) string)
    (opencode--buttonize-file-references
     start (opencode--session-process-position))))

(defun opencode--align-output-table (beg end)
  "Align the completed Markdown table between BEG and END.
Markdown's table command applies only its text changes, so session
properties and markers survive on unchanged text."
  (let ((final-newline (eq (char-before end) ?\n))
        (end (copy-marker end t))
        (inhibit-read-only t))
    (unwind-protect
        (save-excursion
          (goto-char beg)
          ;; The command only aligns tables with an in-table overlay.
          (markdown-ts-in-table-mode 1)
          (unwind-protect
              (markdown-ts-table-align-table t)
            (markdown-ts-in-table-mode -1))
          ;; The command always adds a newline.  Do not introduce one if the
          ;; final text part ended at the last cell instead.
          (when (and (not final-newline) (eq (char-before end) ?\n))
            (delete-region (1- end) end))
          ;; Diff insertion preserves existing cells, but newly inserted
          ;; padding also needs comint's output properties.
          (opencode--mark-as-output beg end))
      (set-marker end nil))))

(defun opencode--mark-as-output (beg end)
  "Mark text from BEG to END, inserted outside `comint-output-filter', as output."
  (comint--mark-as-output beg end)
  (when comint-prompt-read-only
    (with-silent-modifications
      (put-text-property beg end 'read-only t))))

(defun opencode--align-finished-tables (&optional finished)
  "Align tables whose end is confirmed by subsequent complete output lines.
If FINISHED is non-nil, also align a table at the end of the text part.
Retain one line of context for headers whose delimiter has yet to arrive."
  (when opencode--table-scan-marker
    (save-excursion
      (save-restriction
        ;; Pending comint input is not part of the assistant's Markdown.
        (narrow-to-region (point-min) (opencode--session-process-position))
        (let* ((inhibit-field-text-motion t)
               (limit (copy-marker (opencode--session-process-position)))
               (complete-end
                (copy-marker
                 (if finished
                     limit
                   (goto-char limit)
                   (line-beginning-position))))
               waiting)
          (unwind-protect
              (progn
                (goto-char opencode--table-scan-marker)
                (while (and (< (point) complete-end) (not waiting))
                  (let ((node (treesit-node-at (point) 'markdown)))
                    (while (and node (not (equal (treesit-node-type node) "pipe_table")))
                      (setq node (treesit-node-parent node)))
                    ;; At whitespace, `treesit-node-at' can return the table
                    ;; immediately before point.  Never revisit that table.
                    (if (and node (< (point) (treesit-node-end node)))
                        (let ((beg (treesit-node-start node))
                              (end (treesit-node-end node)))
                          (if (and (<= end limit)
                                   (or finished
                                       (< (save-excursion
                                            (goto-char end)
                                            (if (bolp) (point)
                                              (line-beginning-position 2)))
                                          complete-end)))
                              (let ((next (copy-marker end t)))
                                (unwind-protect
                                    (progn
                                      (opencode--align-output-table beg end)
                                      (goto-char next)
                                      (set-marker opencode--table-scan-marker next))
                                  (set-marker next nil)))
                            (set-marker opencode--table-scan-marker beg)
                            (setq waiting t)))
                      (set-marker opencode--table-scan-marker (point))
                      (forward-line 1))))
                (when finished
                  (set-marker opencode--table-scan-marker nil)
                  (setq opencode--table-scan-marker nil)))
            (set-marker limit nil)
            (set-marker complete-end nil)))))))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for left in logo-left
             for right in logo-right
             do (opencode--output (propertize left 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize right 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output."
  (let ((start (opencode--session-process-position)))
    (opencode--output "> ")
    (let ((ov (make-overlay start (opencode--session-process-position))))
      (overlay-put ov 'invisible t)))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight))

(defun opencode-session--start-block (type key)
  "Start the block of TYPE identified by KEY, finishing the last one."
  (let ((last-type (car opencode--current-block)))
    (opencode--align-finished-tables t)
    (when (and (eq type 'text)
               (not (eq last-type 'text)))
      (setf opencode-last-response-start
            (opencode--session-process-position)))
    (when last-type
      (opencode--maybe-insert-block-spacing))
    ;; fold reasoning once the next block starts
    (when opencode--reasoning-start
      (opencode--fold opencode--reasoning-start nil
                      (memq 'reasoning opencode-shown-outputs))
      (setq opencode--reasoning-start nil))
    (when (eq type 'reasoning)
      (setq opencode--reasoning-start
            (opencode--session-process-position)))
    (when (eq type 'text)
      (setq opencode--table-scan-marker
            (copy-marker (opencode--session-process-position))))
    (setq opencode--current-block (cons type key))))

(defun opencode-session--block-delta (session-id key delta)
  "Output DELTA of the text or reasoning block KEY of SESSION-ID.
KEY is (TYPE MESSAGE-ID ORDINAL)."
  (opencode--with-session-buffer session-id
    (let* ((type (car key))
           (new-block-p (not (equal key (cdr opencode--current-block))))
           ;; don't display margins on extra whitespace
           (delta (if new-block-p (string-trim-left delta) delta)))
      ;; blocks start once they have visible content
      (unless (string-empty-p delta)
        (when new-block-p
          (opencode-session--start-block type key))
        (cl-pushnew key opencode--blocks-with-output :test #'equal)
        (if (eq type 'reasoning)
            (opencode--insert-reasoning-block delta)
          (opencode--output delta))
        (when (and (eq type 'text) (string-search "\n" delta))
          (opencode--align-finished-tables nil))))))

(defun opencode-session--block-ended (session-id key text)
  "Finish the text or reasoning block KEY of SESSION-ID with final TEXT.
TEXT is only output if the block had no delta, as some providers only
send the whole block once it's done."
  (opencode--with-session-buffer session-id
    (when (and text
               (not (member key opencode--blocks-with-output))
               (not (member (cadr key) opencode--replayed-unfinished-messages)))
      (opencode-session--block-delta session-id key text))
    (when (and (eq (car key) 'text)
               (equal key (cdr opencode--current-block)))
      (opencode--align-finished-tables t))))

(defun opencode--tokens-total (tokens)
  "Return the sum of the fields of TOKENS, a TokenUsage.Info."
  (let-alist tokens
    (+ .input .output .reasoning .cache.read .cache.write)))

(defun opencode--format-tokens (n)
  "Return token count N in compact form, like 3.2k."
  (file-size-human-readable (or n 0) 'si))

(defun opencode--tool-key (message-id id)
  "Return the key of tool call ID, as they are only unique within MESSAGE-ID."
  (concat message-id "/" id))

(defun opencode-session--display-tool-call (key)
  "Start a block for the tool call KEY, unless it has one or is a question.
Questions are displayed by their form."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool")))
    (unless (or (plist-get call :marker)
                (equal name "question"))
      (opencode-session--start-block 'tool key)
      (puthash key
               (plist-put call :marker
                          (opencode--insert-tool-block name (plist-get call :input)))
               opencode--tool-calls))))

(declare-function opencode--maybe-run-file-edited-hook "opencode")

(defun opencode-session--tool-finished (session-id key state)
  "Display the result of the tool call KEY of SESSION-ID, finished with STATE."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool"))
         (input (plist-get call :input)))
    (opencode--with-session-buffer session-id
      ;; A call with malformed input fails without ever being called.
      (opencode-session--display-tool-call key)
      (opencode--maybe-insert-tool-output
       name state (plist-get (gethash key opencode--tool-calls) :marker)))
    (remhash key opencode--tool-calls)
    (when (equal "completed" (alist-get 'status state))
      (opencode--maybe-run-file-edited-hook
       session-id name input (alist-get 'metadata state)))))

(defun opencode--insert-shell-output (output failed)
  "Insert OUTPUT of a user's shell command, styled as an error if FAILED."
  (unless (string-empty-p output)
    (let ((start (opencode--session-process-position)))
      (opencode--insert-block-with-margin
       (opencode--fenced-block output "text")
       (if failed 'error 'opencode-tool-margin-highlight))
      (when failed
        (opencode--add-error-face (save-excursion
                                    (goto-char start)
                                    (forward-line 1)
                                    (point))
                                  (opencode--output-content-end)))
      (opencode--maybe-insert-block-spacing))))

(defun opencode--insert-compaction-started ()
  "Insert the status line for a compaction starting."
  (opencode-session--start-block 'compaction (float-time))
  (opencode--maybe-insert-block-spacing)
  (opencode--output (propertize "🗜 Compacting session...\n\n" 'face 'shadow)))

(defun opencode--insert-compaction-summary (summary)
  "Insert the SUMMARY of a finished compaction, folded."
  (let ((start (opencode--session-process-position)))
    (opencode--output (concat "🗜 Compaction summary\n" (string-trim summary)))
    (opencode--output "\n\n")
    (opencode--fold start))
  (setq opencode-session-tokens 0)
  (force-mode-line-update))

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  ;; a reversed range would highlight the lines before START
  (when (> end start)
    (let ((ov (make-overlay start (1- end)))
          (margin (propertize ">" 'display
                              `((margin left-margin)
                                ,(propertize "▎" 'face face)))))
      (overlay-put ov 'line-prefix margin)
      (overlay-put ov 'wrap-prefix margin))))

(defun opencode--replay-input (text)
  "Replay a prompt with input TEXT."
  (opencode--show-prompt)
  (insert text "\n")
  (let ((comint-input-sender #'opencode--highlight-input))
    (comint-send-input)))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--replay-input (alist-get 'text message)))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (1+ (opencode--output-content-end)) face))))

(defun opencode--output-content-end (&optional pos)
  "Return the end of the last visible line of output before POS.
POS defaults to the process marker.  Markdown hides closing fence lines
along with their newline, so they are excluded, or they would show as an
extra margin line, and the line after a fold would join its first line."
  (save-excursion
    (goto-char (or pos (opencode--session-process-position)))
    (skip-chars-backward "\r\n[:blank:]")
    (let ((end (point))
          (inhibit-field-text-motion t))
      (beginning-of-line)
      (if (looking-at-p "[`~]\\{3,\\}[[:blank:]]*$")
          (1- (point))
        end))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--refine-diff-hunks (start)
  "Refine and hide diff hunk headers from START to the process marker."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start (opencode--session-process-position))
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re nil t)
          (let* ((beg (match-beginning 0))
                 (header-end (line-beginning-position 2))
                 (end (progn
                        (goto-char beg)
                        ;; patch tool headers have synthetic line counts.
                        (diff-end-of-hunk nil t)
                        (point))))
            (when diff-refine
              (diff--refine-hunk beg end t))
            ;; Font-lock can clear invisible text properties on refontification.
            (let ((overlay (make-overlay beg header-end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'evaporate t))
            (goto-char (max header-end end))))))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block.
Return a marker at the end of the call, where its output goes."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    (prog1 (save-excursion
             (goto-char (opencode--session-process-position))
             (skip-chars-backward "\n")
             (point-marker))
      (opencode--maybe-insert-block-spacing)
      (when (member tool '("edit" "patch"))
        (opencode--refine-diff-hunks start)
        (opencode--fold start nil (memq (intern tool) opencode-shown-outputs))))))

(defun opencode--shell-failed-p (tool info)
  "Return non-nil if INFO is of a failed shell TOOL.
INFO has an optional status and exit, like the metadata of a tool call."
  (let-alist info
    (and (string= tool "shell")
         (or (member .status '("timeout" "killed"))
             (and (numberp .exit) (/= .exit 0))))))

(defun opencode--tool-output-shown-p (tool state)
  "Return non-nil if the output of the TOOL call with STATE starts unfolded."
  (let-alist state
    (or (memq (intern tool) opencode-shown-outputs)
        (and (string= .status "error")
             (memq 'error opencode-shown-outputs))
        (and (memq 'shell-non-zero opencode-shown-outputs)
             (opencode--shell-failed-p tool .metadata)))))

(defconst opencode--ignore-output-for-tools '("edit" "write" "patch"
                                              "question" "todowrite")
  "Don't show the output for these tools.")

(defun opencode--tool-content-text (content)
  "Return the text items of tool output CONTENT joined, or nil if there are none."
  (when-let* ((texts (cl-loop for item in content
                              when (equal "text" (alist-get 'type item))
                              collect (alist-get 'text item))))
    (string-join texts "\n")))

(defun opencode--add-error-face (beg end)
  "Style BEG to END with the `error' face."
  ;; Font-lock can clear text properties on refontification.
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'error)
    (overlay-put overlay 'evaporate t)))

(defun opencode--maybe-insert-tool-output (tool state call-end)
  "Insert the output or error of the finished TOOL call with STATE at CALL-END.
Parallel tool calls can complete after later output, so it's inserted
after its call, foldable onto the call's last line, and folded unless
`opencode--tool-output-shown-p'.  Edits show only their diff, unless
they failed.  Errors and the output of shell tool calls that exited with a
non-zero status are highlighted with the `error' face."
  (let-alist state
    (when (and (member .status '("completed" "error"))
               call-end
               (marker-position call-end))
      (let* ((failed (string= .status "error"))
             (output (if failed
                         .error.message
                       (and (not (member tool opencode--ignore-output-for-tools))
                            (opencode--tool-content-text .content))))
             (errorp (or failed (opencode--shell-failed-p tool .metadata))))
        (when output
          (let ((inhibit-read-only t)
                (end (copy-marker call-end t)))
            (save-excursion
              (goto-char call-end)
              (insert "\n" (opencode--fenced-block output "text")))
            (opencode--mark-as-output call-end end)
            (opencode--buttonize-file-references call-end end)
            ;; The first visible output line starts at the call's hidden
            ;; closing fence, so that's where its margin is read from.
            (opencode--add-margin (1+ (opencode--output-content-end call-end))
                                  (1+ (opencode--output-content-end end))
                                  (if errorp 'error 'opencode-tool-margin-highlight))
            (when errorp
              (opencode--add-error-face (save-excursion
                                          (goto-char call-end)
                                          (forward-line 2)
                                          (point))
                                        (opencode--output-content-end end)))
            (opencode--fold (save-excursion
                              (goto-char (opencode--output-content-end call-end))
                              (let ((inhibit-field-text-motion t))
                                (line-beginning-position)))
                            end
                            (opencode--tool-output-shown-p tool state))
            (set-marker end nil))))
      (set-marker call-end nil))))

(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))

(defconst opencode--migrated-tool-names
  '(("bash" . "shell") ("apply_patch" . "patch") ("task" . "subagent"))
  "Old names of tools, that sessions migrated from v1 still have.")

(defconst opencode--migrated-tool-inputs
  '((filePath . path) (subagent_type . agent))
  "Old names of tool inputs, that sessions migrated from v1 still have.")

(defun opencode--migrate-tool-input (input)
  "Return tool INPUT with the current names of its arguments."
  (if (listp input)
      (mapcar (lambda (pair)
                (cons (alist-get (car pair) opencode--migrated-tool-inputs (car pair))
                      (cdr pair)))
              input)
    input))

(defun opencode--replay-tool (message-id tool)
  "Display the TOOL content item of the assistant message MESSAGE-ID.
Calls not yet finished are recorded so that later events finish them."
  (let-alist tool
    (let ((key (opencode--tool-key message-id .id))
          (name (or (cdr (assoc .name opencode--migrated-tool-names)) .name))
          (input (opencode--migrate-tool-input .state.input)))
      (pcase .state.status
        ;; its input is still incomplete, so it's displayed once called
        ("streaming" (puthash key (list :name name) opencode--tool-calls))
        ("running"
         (puthash key (list :name name :input input) opencode--tool-calls)
         (opencode-session--display-tool-call key))
        (_ (opencode--maybe-insert-tool-output
            name .state (opencode--insert-tool-block name input)))))))

(defun opencode--replay-assistant-message (message)
  "Display the assistant MESSAGE."
  (let-alist message
    (unless .time.completed
      (push .id opencode--replayed-unfinished-messages))
    (let (last-type)
      (dolist (item .content)
        (let ((type (alist-get 'type item))
              (text (string-trim (or (alist-get 'text item) ""))))
          (if (string= type "tool")
              (opencode--replay-tool .id item)
            (unless (string-empty-p text)
              (let ((start (opencode--session-process-position)))
                (pcase type
                  ("text"
                   (unless (equal last-type "text")
                     (setf opencode-last-response-start start))
                   (setq opencode--table-scan-marker (copy-marker start))
                   (opencode--output text)
                   (opencode--align-finished-tables t))
                  ("reasoning"
                   (opencode--insert-reasoning-block text)))
                (opencode--output "\n\n")
                (when (equal type "reasoning")
                  (opencode--fold
                   start nil
                   (memq 'reasoning opencode-shown-outputs))))
              (setq last-type type))))))
    (when .tokens
      (setq opencode-session-tokens (opencode--tokens-total .tokens)))))

(defun opencode--replay-message (message)
  "Display the past session MESSAGE."
  (let-alist message
    (pcase .type
      ("user" (opencode--replay-user-request message))
      ("assistant" (opencode--replay-assistant-message message))
      ("shell"
       (opencode--replay-input (concat "!" .command))
       (opencode--insert-shell-output (or .output.output "")
                                      (opencode--shell-failed-p "shell" message)))
      ("compaction"
       (opencode--insert-compaction-started)
       (pcase .status
         ("completed" (opencode--insert-compaction-summary .summary))
         ("failed" (opencode--insert-error .error.message))))
      ("idle"
       (when (equal .outcome "failed")
         (opencode--insert-error (or .error.message "Session failed")))))))

(defun opencode--replay-session-messages (messages &optional revert-message-id)
  "Display past session MESSAGES.
Those from REVERT-MESSAGE-ID on have a staged revert, so are hidden."
  (let ((inhibit-redisplay t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions)))
    (dolist (message messages)
      (unless (and revert-message-id
                   (not (string< (alist-get 'id message) revert-message-id)))
        (opencode--replay-message message)))
    (setq opencode--current-block nil)))

(declare-function opencode--permission-request "opencode-permission")
(declare-function opencode--form-request "opencode-question")

(defun opencode--fetch-pending (session-id)
  "Queue SESSION-ID's pending permissions and forms.
Events are lost when not connected, so this is the only way to see them."
  (opencode-api-session-permissions (session-id)
      requests
    (dolist (request requests)
      (let-alist request
        (opencode--permission-request
         .id .sessionID .action .metadata .resources .save .message))))
  (opencode-api-session-forms (session-id)
      forms
    (mapc #'opencode--form-request forms)))

(defun opencode--load-session (session &optional callback)
  "Display SESSION in the current buffer and go to a prompt, unless it's busy.
If CALLBACK is given, it will be called with SESSION afterwards."
  (let-alist session
    (opencode-api-all-messages (.id)
        messages
      (setq opencode--replayed-unfinished-messages nil
            opencode--server-agent
            (or .agent
                ;; sessions migrated from v1 only have it on their messages
                (alist-get 'agent
                           (seq-find (lambda (message)
                                       (equal "assistant" (alist-get 'type message)))
                                     (reverse messages)))
                ;; a session with no agent yet uses the default, listed first
                (alist-get 'id (seq-find #'opencode--selectable-agent-p
                                         opencode-agents))))
      (opencode--replay-session-messages messages .revert.messageID)
      ;; A new session keeps the agent and model of where it was opened from.
      (when messages
        (opencode--adopt-selection opencode--server-agent .model))
      (opencode-api-sessions-active
        active
        (if (assq (intern .id) active)
            (setq opencode-session-status "busy")
          (setq opencode-session-status "idle")
          (opencode--show-prompt))
        (force-mode-line-update)
        (opencode--fetch-pending .id)
        (when callback
          (funcall callback session))))))

(cl-defun opencode-open-session (session &key (pop-to-buffer t) callback)
  "Open comint based shell for SESSION.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer.
Returns the buffer.
If CALLBACK is given, it will be called with the session after it is initialized."
  (let-alist session
    (let ((old-buffer (gethash .id opencode-session-buffers)))
      (if (buffer-live-p old-buffer)
          (if pop-to-buffer
              (pop-to-buffer old-buffer)
            (switch-to-buffer old-buffer))
        (let ((buffer (generate-new-buffer
                       (format "*OpenCode: %s*" (opencode--session-title session))))
              (agent (copy-tree opencode-session-agent))
              (agents (copy-tree opencode-session-agents)))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory .location.directory)
                  opencode-session-agents (mapcar (lambda (agent)
                                                    (unless (alist-get 'model agent)
                                                      (setf (alist-get 'model agent)
                                                            opencode-last-model))
                                                    agent)
                                                  (or agents
                                                      (copy-tree opencode-agents)))
                  opencode-session-agent (or agent
                                             (seq-find #'opencode--selectable-agent-p
                                                       opencode-session-agents))
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (hack-dir-local-variables-non-file-buffer)
            (opencode--download-slash-commands default-directory)
            (puthash .id buffer opencode-session-buffers)
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode--load-session session callback))
            (if pop-to-buffer
                (pop-to-buffer buffer)
              (switch-to-buffer buffer))))))))

(defun opencode--reload-session ()
  "Redisplay the current session from the server, keeping any typed input.
It keeps its properties, which hold the parts of its labels."
  (let ((input (buffer-substring (opencode--session-process-position)
                                 (point-max))))
    (let ((inhibit-read-only t))
      (remove-overlays)
      (erase-buffer))
    (setq opencode--current-block nil)
    (opencode-insert-logo)
    (opencode-api-session (opencode-session-id)
        session
      (opencode--load-session
       session
       (lambda (_session)
         (when (equal opencode-session-status "idle")
           (goto-char (point-max))
           (insert input)))))))

(defun opencode--prompt-start ()
  "Return the start of the line of the prompt before point."
  (save-excursion
    (end-of-line)
    (comint-previous-prompt 1)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--truncate-to-prompt (start text)
  "Delete from START to the end of the buffer, then prompt with TEXT as input."
  (let ((inhibit-read-only t))
    (remove-overlays start (point-max))
    (delete-region start (point-max)))
  (set-marker (process-mark (get-buffer-process (current-buffer))) start)
  (setq opencode--current-block nil)
  (opencode--show-prompt)
  (goto-char (point-max))
  (insert text))

(defun opencode--current-message-number ()
  "Return the 0-indexed message number at point.
Counts prompts from the beginning of the buffer to the current position.
Returns nil if point is before the first prompt."
  (save-excursion
    (end-of-line)
    (comint-previous-prompt 1)
    (let ((target-point (point)))
      (goto-char (point-min))
      (cl-loop do (comint-next-prompt 1)
               while (< (point) target-point)
               count t))))

(defmacro opencode--current-user-message (result &rest body)
  "Run BODY with RESULT as the user message at point, nil before the first.
Prompts are user messages and shell commands, in order."
  (declare (indent defun))
  (let ((message-number (gensym "message-number"))
        (messages (gensym "messages"))
        (continue (gensym "continue")))
    `(let ((,message-number (opencode--current-message-number))
           (,continue (lambda (,result) ,@body)))
       (if (not ,message-number)
           (funcall ,continue nil)
         (opencode-api-all-messages (opencode-session-id)
             ,messages
           (let ((message (nth ,message-number
                               (seq-filter (lambda (message)
                                             (member (alist-get 'type message)
                                                     '("user" "shell")))
                                           ,messages))))
             (when (equal "shell" (alist-get 'type message))
               (user-error "Can't do that to a shell command"))
             (funcall ,continue message)))))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let* ((old-title (or (alist-get 'title session)
                        (string-remove-prefix
                         "*OpenCode: "
                         (string-trim-right (buffer-name)
                                            "\\*\\(<[0-9]+>\\)?"))))
         (title (read-string "Title: " old-title)))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res)))

(defun opencode--insert-error (message)
  "Insert error MESSAGE in the current session buffer."
  (opencode--output (propertize message 'face 'error))
  (opencode--output "\n\n"))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID."
  (opencode--with-session-buffer session-id
    (opencode--insert-error message)))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (setq opencode--skip-files-finished-editing-hook-on-next-idle t)
  (opencode-api-interrupt-session (opencode-session-id)
      result
    (when (opencode--json-falsy (alist-get 'interrupted result))
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil)
      (message "Failed to abort session."))))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode-compact-session ()
  "Compact the current session."
  (interactive)
  (if opencode-session-id
      (opencode-api-compact-session (opencode-session-id)
          (make-hash-table)
          _result)
    (user-error "Not in an OpenCode session")))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the top of the last text part of assistant response."
  (interactive)
  (unless opencode-last-response-start
    (user-error "No last response"))
  (goto-char opencode-last-response-start)
  (forward-word 1)
  (backward-word 1)
  (recenter-top-bottom 0))

(defun opencode--session-control-display (sessions)
  "Display SESSIONS in the current session control buffer."
  (let ((inhibit-read-only t)
        (point (point))
        cache)
    (erase-buffer)
    (if sessions
        (make-vtable
         :columns '((:name "Title" :max-width "50%")
                    (:name "Branch" :min-width 6)
                    (:name "Last Updated" :width 12
                     :formatter opencode--format-time-ago
                     :primary ascend)
                    (:name "Cost" :width 8 :align right)
                    (:name "Tokens")
                    (:name "Cache" :width 6 :align right)
                    (:name "Created at" :width 10
                     :formatter opencode--format-time-ago))
         :objects sessions
         :getter (lambda (object column vtable)
                   (let-alist object
                     (pcase (vtable-column vtable column)
                       ("Title" (let ((icon (opencode--session-status-icon object)))
                                  (concat (when icon (concat icon " "))
                                          (opencode--session-title object))))
                       ("Branch" (if (and .location.directory
                                          (file-exists-p .location.directory))
                                     (let ((default-directory .location.directory))
                                       (with-memoization
                                           (map-elt cache .location.directory)
                                         (or (magit-get-current-branch) "-")))
                                   "-"))
                       ("Last Updated" (opencode--time-ago object 'updated))
                       ("Cost" (format "$%.2f" (or .cost 0)))
                       ("Tokens" (format "↑%s ↓%s 🧠%s"
                                         (opencode--format-tokens
                                          (+ (or .tokens.input 0)
                                             (or .tokens.cache.read 0)
                                             (or .tokens.cache.write 0)))
                                         (opencode--format-tokens .tokens.output)
                                         (opencode--format-tokens .tokens.reasoning)))
                       ("Cache" (let ((total (+ (or .tokens.input 0)
                                                (or .tokens.cache.read 0)
                                                (or .tokens.cache.write 0))))
                                  (if (zerop total)
                                      "-"
                                    (format "%d%%"
                                            (round (* 100.0 (or .tokens.cache.read 0))
                                                   total)))))
                       ("Created at" (opencode--time-ago object 'created)))))
         :separator-width 3
         :keymap opencode-session-control-mode-map)
      (insert "No sessions in " (or default-directory "unknown directory")))
    (goto-char point)))

(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (when opencode-session-control-project-id
    (if opencode-session-control-verbose
        (opencode-api-sessions-for-project (opencode-session-control-project-id)
            sessions
          (opencode--session-control-display sessions))
      (opencode-api-root-sessions-for-project (opencode-session-control-project-id)
          sessions
        (opencode--session-control-display sessions)))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here

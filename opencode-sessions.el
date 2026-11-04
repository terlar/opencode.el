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
(require 'mailcap)
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-format-tool-calls)
(require 'project)
(require 'vtable)
(require 'yank-media)

(defcustom opencode-shown-outputs '(bash-non-zero error edit apply_patch)
  "Outputs in session buffers to show unfolded when first displayed.
A list of symbols: `reasoning', a tool name like `bash' or `grep',
`bash-non-zero' to show bash output only if the command failed, or
`error' to show the error of any tool call that failed.
Shown outputs can still be folded manually with `hs-toggle-hiding'."
  :type '(repeat (choice (const reasoning)
                         (const bash-non-zero)
                         (const error)
                         (symbol :tag "Tool name")))
  :group 'opencode)

(defvar opencode-session-control-mode-map
  (define-keymap
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "U" 'opencode-unshare-all-sessions
    "v" 'opencode-session-control-toggle-verbose))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

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
    "C-c s" 'opencode-share-session
    "C-c u" 'opencode-unshare-session
    "C-c U" 'opencode-unshare-all-sessions
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c D" 'opencode-delete-message
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-subagent))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session
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

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, contains a cons cell (QUESTION-ID . QUESTIONS-VECTOR).")

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

(defvar-local opencode--tool-calls-displayed nil
  "A hash table mapping displayed callIDs to a marker at the end of the call.
Its output is inserted there once the call completes.")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(defvar opencode-assistant-messages
  nil
  "An alist mapping updating assistant message ids to (type . part-id).")

(defmacro opencode--with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer for SESSION-ID."
  (declare (indent 1))
  `(when-let* ((buffer (gethash ,session-id opencode-session-buffers)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((name (alist-get 'name opencode-session-agent))
         (agents opencode-session-agents)
         (pos (and name (cl-position-if
                         (lambda (agent)
                           (string= name (alist-get 'name agent)))
                         agents))))
    (when pos
      (setcar (nthcdr pos agents) opencode-session-agent)
      (when-let* ((next (seq-find
                         (lambda (agent)
                           (string= "primary" (alist-get 'mode agent)))
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
     (opencode-api-sessions sessions
       (opencode--select-sessions "Session: "
                                  (seq-remove (lambda (session)
                                                (alist-get 'parentID session))
                                              sessions)
                                  (format "No sessions in %s" default-directory)
                                  :pop-to-buffer nil)))))

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
                           (list .title
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

(defun opencode--collect-all-models ()
  "Collect all models from `opencode-providers' as a list.
Each element is (display-name . (provider-id provider-name model-id))."
  (let (result)
    (dolist (provider opencode-providers)
      (let-alist provider
        (dolist (model-entry .models)
          (let ((model (cdr model-entry)))
            (push (list (alist-get 'name model)
                        `((providerID . ,.id)
                          (modelID . ,(alist-get 'id model)))
                        .name)
                  result)))))
    (nreverse result)))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let* ((model (opencode--annotated-completion
                      "Model: "
                      (opencode--collect-all-models))))
    (setf (alist-get 'model opencode-session-agent) model
          opencode-last-model model)
    (when-let* ((variant (alist-get 'variant opencode-session-agent)))
      (unless (alist-get variant
                         (alist-get 'variants (opencode--current-model)))
        (setq opencode-session-agent
              (assq-delete-all 'variant opencode-session-agent))))))

(defun opencode--current-model ()
  "Return the active model for this session."
  (let-alist opencode-session-agent
    (map-nested-elt
     (seq-find (lambda (provider)
                 (string= .model.providerID (alist-get 'id provider)))
               opencode-providers)
     `(models ,(intern .model.modelID)))))

(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (setf (alist-get 'variant opencode-session-agent)
              (opencode--annotated-completion
               "Variant: "
               (cl-loop for (variant . options) in
                        variants
                        collect (list (symbol-name variant)
                                      variant
                                      (format "%s" options)))))
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

(defun opencode-share-session (&optional session)
  "Share SESSION or the current session."
  (interactive)
  (opencode-api-share-session ((or (alist-get 'id session)
                                   opencode-session-id))
      session
    (let ((url (map-nested-elt session '(share url))))
      (gui-select-text url)
      (message "Copied to clipboard: %s" url))))

(defun opencode-unshare-session (&optional session)
  "Unshare SESSION or the current session."
  (interactive)
  (opencode-api-unshare-session ((or (alist-get 'id session)
                                     opencode-session-id))
      _session
    (message "Session no longer shared")))

(defun opencode-unshare-all-sessions ()
  "Unshare all sessions across all projects."
  (interactive)
  (opencode-api-projects projects
    (dolist (project projects)
      (let ((default-directory (alist-get 'worktree project)))
        (opencode-api-sessions sessions
          (dolist (session sessions)
            (when (alist-get 'share session)
              (opencode-unshare-session session))))))))

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
    (let* ((agent (pcase .name
                    ("Planner-Sisyphus" "Planner")
                    (name name)))
           (model (opencode--current-model))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (context-used (* 100 (/ (float opencode-session-tokens)
                                   (map-nested-elt model '(limit context))))))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat (alist-get 'name model)
                        (when .variant
                          (propertize (format " %s" .variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

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
      (treesit-font-lock-fontify-region start end loudly))))

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
  (opencode-api-session-messages (opencode-session-id)
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
      result
    (unless result
      (error "Unable to delete session"))
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-buffer (current-buffer)))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--mimetype (file)
  "Return guess of mimetype for FILE."
  (pcase (file-name-extension file)
    ((or "org" "ts" (pred null)) "text/plain")
    (ext (or (mailcap-extension-to-mime ext)
             "text/plain"))))

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

(defun opencode--add-file-to-context (file &optional mime display-name)
  "Add FILE to context with optional MIME and DISPLAY-NAME."
  (let* ((display-name (or display-name
                           (opencode--relative-path-for-display file)))
         (mime (or mime (opencode--mimetype file)))
         (url (concat "file://" file)))
    (opencode--insert-label display-name
                            `((type . file)
                              (filename . ,display-name)
                              (mime . ,mime)
                              (url . ,url)))
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
          (opencode--add-file-to-context file mime (file-name-nondirectory file)))
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
      `((type . text)
        (text . ,(concat
                  (format "<buffer name=\"%s\">" buffer)
                  (with-current-buffer buffer
                    (buffer-substring-no-properties (point-min) (point-max)))
                  "</buffer>"))
        (synthetic . t))))))

(defun opencode-add-region ()
  "Add the active region to context."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning)
                                                    (region-end))))
        (with-last-opencode-session
         (opencode--insert-label
          (format "region: %s"
                  (truncate-string-to-width region 24 0 nil (truncate-string-ellipsis)))
          `((type . text)
            (text . ,(concat "<region>" region "</region>"))
            (synthetic . t)))
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
                                `((type . "agent")
                                  (name . ,name)))))))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  ;; STRING is the buffer text of the input, where each label is one character
  (pcase-let ((`(,string . ,extra-parts)
               (opencode--input-text-and-parts
                comint-last-input-start
                (+ comint-last-input-start (length string))))
              (temp-files opencode--temp-files)
              (sent-message nil))
    (setf opencode--temp-files nil)
    (let-alist opencode-session-agent
      (cond
       ((string-prefix-p "/" string)
        (let ((space-pos (seq-position string ?\s)))
          (opencode-api-execute-command (opencode-session-id)
              `((agent . ,.name)
                (model . ,(concat .model.providerID "/" .model.modelID))
                (command . ,(substring string 1 space-pos))
                (arguments . ,(if space-pos
                                  (substring string (1+ space-pos))
                                "")))
              _response)))
       ((string-prefix-p "!" string)
        (opencode-api-execute-shell (opencode-session-id)
            `((agent . ,.name)
              (command . ,(substring string 1)))
            _response))
       (t
        (setq sent-message t)
        (opencode-api-send-message (opencode-session-id)
            `((agent . ,.name)
              ,(assoc 'model opencode-session-agent)
              ,@(when .variant
                  `((variant . ,.variant)))
              (parts ((type . text) (text . ,string))
                     ,@extra-parts))
            _result
          (opencode--delete-temp-files temp-files)))))
    (unless sent-message
      (opencode--delete-temp-files temp-files))))

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

(defun opencode-session--message-updated (info)
  "Handle message.updated event with INFO."
  (let-alist info
    (pcase .role
      ("assistant"
       (when .time.completed
         (opencode--with-session-buffer .sessionID
           (opencode--align-finished-tables t)
           (setq opencode-session-tokens
                 (+ .tokens.input .tokens.output .tokens.reasoning
                    .tokens.cache.read .tokens.cache.write))
           (force-mode-line-update)))
       (if (or .finish .error)
           (setf opencode-assistant-messages
                 (assoc-delete-all .id opencode-assistant-messages))
         (push (cons .id nil) opencode-assistant-messages))))))

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

(defun opencode-session--update-part (part-id part delta type)
  "Display PART, partial message output. DELTA is new text since last update.
TYPE is text|reasoning|tool|step-finish
PART-ID is id of PART"
  (let-alist part
    (if .time.end
        (remhash part-id opencode-part-type)
      ;; only will follow up with message.part.delta updates when it has "" as text
      (when (string-empty-p .text)
        (puthash part-id type opencode-part-type)))
    (opencode--with-session-buffer .sessionID
      (when-let* ((message-parts (assoc-string .messageID opencode-assistant-messages)))
        (let* ((last-type (cadr message-parts))
               (last-part-id (cddr message-parts))
               (current-type (pcase type
                               ("reasoning" 'reasoning)
                               ("text" 'text)
                               ("tool" 'tool)
                               (_ nil)))
               (new-block-p (not (equal part-id last-part-id))))
          ;; don't display margins on extra whitespace
          (when (and delta
                     current-type
                     new-block-p)
            (setf delta (string-trim-left delta)))
          ;; text and reasoning parts start once they have visible content,
          ;; so leading whitespace deltas stay trimmed
          (when (and current-type
                     new-block-p
                     ;; switch the type to reasoning only when we get the first
                     ;; non-empty delta of the reasoning block
                     (or (eq current-type 'tool)
                         (and delta (not (string-empty-p delta)))))
            (opencode--align-finished-tables t)
            (when (and (eq current-type 'text)
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
            (when (eq current-type 'reasoning)
              (setq opencode--reasoning-start
                    (opencode--session-process-position)))
            (when (eq current-type 'text)
              (setq opencode--table-scan-marker
                    (copy-marker (opencode--session-process-position))))
            (setf (cdr message-parts) (cons current-type part-id)))
          (pcase type
            ((and "reasoning" (guard delta))
             (opencode--insert-reasoning-block delta))
            ((and "text" (guard delta))
             (opencode--output delta))
            ("tool"
             (when (and
                    ;; only when it first starts running
                    (string= .state.status "running")
                    ;; avoid duplicate display
                    (not (gethash .callID opencode--tool-calls-displayed))
                    ;; skip the question tool, handled by question.asked event
                    (not (string= .tool "question")))
               (puthash .callID (opencode--insert-tool-block .tool .state.input)
                        opencode--tool-calls-displayed))
             (opencode--maybe-insert-tool-output
              part (gethash .callID opencode--tool-calls-displayed)))
            ("step-finish"
             (when (string= "stop" .reason)
               (opencode--maybe-insert-block-spacing))))
          (when (and (equal type "text")
                     (or .time.end (and delta (string-search "\n" delta))))
            (opencode--align-finished-tables .time.end)))))))

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

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (let-alist message
    (dolist (part .parts)
      (let-alist part
        (unless .synthetic
          (pcase .type
            ("text" (insert .text))))))
    (insert "\n")
    (let ((comint-input-sender #'opencode--highlight-input))
      (comint-send-input))
    (let ((agent (seq-find (lambda (agent)
                             (string= (alist-get 'name agent)
                                      .info.agent))
                           opencode-session-agents)))
      (setq opencode-session-agent agent)
      (setf (alist-get 'model opencode-session-agent) .info.model)
      (setf (alist-get 'variant opencode-session-agent) .info.model.variant))))

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
                        ;; apply_patch headers have synthetic line counts.
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
      (when (member tool '("edit" "apply_patch"))
        (opencode--refine-diff-hunks start)
        (opencode--fold start nil (memq (intern tool) opencode-shown-outputs))))))

(defun opencode--bash-non-zero-p (part)
  "Return non-nil if PART is a bash call that exited with a non-zero status."
  (let-alist part
    (and (string= .tool "bash")
         (numberp .state.metadata.exit)
         (/= .state.metadata.exit 0))))

(defun opencode--tool-output-shown-p (part)
  "Return non-nil if the output of the tool call in PART starts unfolded."
  (let-alist part
    (or (memq (intern .tool) opencode-shown-outputs)
        (and (string= .state.status "error")
             (memq 'error opencode-shown-outputs))
        ;; Only a user's shell command has an empty title.
        (and (string= .tool "bash") (equal .state.title ""))
        (and (memq 'bash-non-zero opencode-shown-outputs)
             (opencode--bash-non-zero-p part)))))

(defun opencode--maybe-insert-tool-output (part call-end)
  "Insert the output or error of the finished tool call in PART at CALL-END.
Parallel tool calls can complete after later output, so it's inserted
after its call, foldable onto the call's last line, and folded unless
`opencode--tool-output-shown-p'.  Edits show only their diff, unless
they failed.  Errors and the output of bash commands that exited with a
non-zero status are highlighted with the `error' face."
  (let-alist part
    (when (and (member .state.status '("completed" "error"))
               call-end
               (marker-position call-end))
      (let* ((failed (string= .state.status "error"))
             (output (if failed
                         .state.error
                       (and (not (member .tool '("edit" "write" "apply_patch" "question")))
                            .state.output)))
             (errorp (or failed (opencode--bash-non-zero-p part))))
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
              ;; Font-lock can clear text properties on refontification.
              (let ((overlay (make-overlay (save-excursion
                                             (goto-char call-end)
                                             (forward-line 2)
                                             (point))
                                           (opencode--output-content-end end))))
                (overlay-put overlay 'face 'error)
                (overlay-put overlay 'evaporate t)))
            (opencode--fold (save-excursion
                              (goto-char (opencode--output-content-end call-end))
                              (let ((inhibit-field-text-motion t))
                                (line-beginning-position)))
                            end
                            (opencode--tool-output-shown-p part))
            (set-marker end nil))))
      (set-marker call-end nil))))

(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))

(defun opencode--replay-session-messages (messages)
  "Display past session MESSAGES."
  (let ((inhibit-redisplay t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions)))
    (dolist (message messages)
      (let-alist (alist-get 'info message)
        (pcase .role
          ("user" (opencode--replay-user-request message))
          ("assistant"
           (let (last-part-type)
             (dolist (part (alist-get 'parts message))
               (let-alist part
                 (if (string= .type "tool")
                     (opencode--maybe-insert-tool-output
                      part (opencode--insert-tool-block .tool .state.input))
                   (when .text
                     (let ((text (string-trim .text)))
                       (unless (string-empty-p text)
                         (let ((start (opencode--session-process-position)))
                           (pcase .type
                             ("text"
                              (unless (equal last-part-type "text")
                                (setf opencode-last-response-start start))
                              (setq opencode--table-scan-marker (copy-marker start))
                              (opencode--output text)
                              (opencode--align-finished-tables t))
                             ("reasoning"
                              (opencode--insert-reasoning-block text)))
                           (opencode--output "\n\n")
                           (when (equal .type "reasoning")
                             (opencode--fold
                              start nil
                              (memq 'reasoning opencode-shown-outputs))))
                         (setq last-part-type .type))))))))
           (setq opencode-session-tokens
                 (+ .tokens.input .tokens.output .tokens.reasoning
                    .tokens.cache.read .tokens.cache.write))))))))

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
        (let ((buffer (generate-new-buffer (format "*OpenCode: %s*" .title)))
              (agent (copy-tree opencode-session-agent))
              (agents (copy-tree opencode-session-agents)))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory .directory)
                  opencode--tool-calls-displayed (make-hash-table :test 'equal)
                  opencode-session-agents (mapcar (lambda (agent)
                                                    (unless (alist-get 'model agent)
                                                      (setf (alist-get 'model agent)
                                                            opencode-last-model))
                                                    agent)
                                                  (or agents
                                                      (copy-tree opencode-agents)))
                  opencode-session-agent (or agent (car opencode-session-agents))
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (hack-dir-local-variables-non-file-buffer)
            (puthash .id buffer opencode-session-buffers)
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode-api-session-messages (.id)
                  messages
                (opencode--replay-session-messages messages)
                (opencode-api-sessions-status
                  busy-sessions
                  (if-let* ((session-status (cdr (assoc-string .id busy-sessions))))
                      (opencode-session--set-status
                       .id
                       (alist-get 'type session-status))
                    (opencode--show-prompt))
                  (when callback
                    (funcall callback session)))))
            (if pop-to-buffer
                (pop-to-buffer buffer)
              (switch-to-buffer buffer))))))))

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

(defmacro opencode--current-message-id (result &rest body)
  "Run BODY with RESULT as the message id of the user message at point."
  (declare (indent defun))
  `(opencode--current-message-exchange (,result _assistant-id)
     ,@body))

(defmacro opencode--current-message-exchange (bindings &rest body)
  "Run BODY with BINDINGS (user-id assistant-id)
bound to the exchange ids at point."
  (declare (indent defun))
  (let ((user-id (car bindings))
        (assistant-id (cadr bindings)))
    `(when-let* ((message-number (opencode--current-message-number)))
       (opencode-api-session-messages (opencode-session-id)
           messages
         (let* ((user-message-index
                 (cl-loop for message in messages
                          for index from 0
                          when (string= "user" (map-nested-elt message '(info role)))
                          count t into count
                          when (= (1- count) message-number)
                          return index))
                (user-message (and user-message-index
                                   (nth user-message-index messages)))
                (assistant-message (and user-message-index
                                        (let ((next-message (nth (1+ user-message-index)
                                                                 messages)))
                                          (when (string= "assistant"
                                                         (map-nested-elt next-message
                                                                         '(info role)))
                                            next-message))))
                (,user-id (map-nested-elt user-message '(info id)))
                (,assistant-id (map-nested-elt assistant-message '(info id))))
           ,@body)))))

(defun opencode--delete-message-at-point ()
  "Delete the prompt at point and its output from the session buffer."
  (let ((start (save-excursion
                 (end-of-line)
                 (comint-previous-prompt 1)
                 (line-beginning-position)))
        (end (save-excursion
               (end-of-line)
               (comint-previous-prompt 1)
               (goto-char (line-beginning-position 2))
               (if (ignore-errors (comint-next-prompt 1) t)
                   (line-beginning-position)
                 (point-max)))))
    (let ((inhibit-read-only t))
      (remove-overlays start end)
      (delete-region start end))))

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

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID and then new prompt."
  (opencode--with-session-buffer session-id
    (opencode--output (propertize message 'face 'error))
    (opencode--output "\n\n")
    (opencode--show-prompt)))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (setq opencode--skip-files-finished-editing-hook-on-next-idle t)
  (opencode-api-abort-session (opencode-session-id)
      success-p
    (unless success-p
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
      (opencode-api-summarize (opencode-session-id)
          (alist-get 'model opencode-session-agent)
          result
        (unless result
          (error "Unable to compact session")))
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

(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (opencode-api-sessions sessions
    (let ((inhibit-read-only t)
          (point (point))
          (sessions (if opencode-session-control-verbose
                        sessions
                      (seq-remove (lambda (session)
                                    (alist-get 'parentID session))
                                  sessions)))
          cache)
      (erase-buffer)
      (if sessions
          (make-vtable
           :columns '("Title"
                      (:name "Branch" :min-width 6)
                      (:name "Last Updated" :width 12
                       :formatter opencode--format-time-ago
                       :primary ascend)
                      (:name "Files changed" :width 13 :align right)
                      (:name "Created at" :width 10
                       :formatter opencode--format-time-ago))
           :objects sessions
           :actions '("x" opencode-kill-session
                      "R" opencode-rename-session
                      "s" opencode-share-session
                      "u" opencode-unshare-session
                      "RET" opencode-open-session-same-window
                      "o" opencode-open-session-same-window)
           :getter (lambda (object column vtable)
                     (let-alist object
                       (pcase (vtable-column vtable column)
                         ("Title" (if .share
                                      (concat (propertize "shared " 'face
                                                          '(bold opencode-request-margin-highlight))
                                              .title)
                                    .title))
                         ("Branch" (if (and .directory (file-exists-p .directory))
                                       (let ((default-directory .directory))
                                         (with-memoization
                                             (map-elt cache .directory)
                                           (magit-get-current-branch)))
                                     "-"))
                         ("Last Updated" (opencode--time-ago object 'updated))
                         ("Files changed" (let-alist .summary
                                            (if (opencode--json-falsy .files)
                                                "none"
                                              (format "%d  +%d-%d"
                                                      .files
                                                      (if (opencode--json-falsy .additions)
                                                          0 .additions)
                                                      (if (opencode--json-falsy .deletions)
                                                          0 .deletions)))))
                         ("Created at" (opencode--time-ago object 'created)))))
           :separator-width 3
           :keymap opencode-session-control-mode-map)
        (insert "No sessions in " (or default-directory "unknown directory")))
      (goto-char point))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here

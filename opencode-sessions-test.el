;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-sessions)

(defmacro with-opencode-session-test-buffer (&rest body)
  "Run BODY in a temporary `opencode-session-mode' buffer.
BODY can refer to the buffer's process as `process'.  The default directory
is the opencode source directory, and session state is bound so that events
for session \"session\" render here."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq default-directory
           (file-name-directory (locate-library "opencode-sessions")))
     (opencode-session-mode)
     (let ((process (start-process "opencode-test" (current-buffer) nil))
           (opencode-session-buffers (make-hash-table :test 'equal))
           (opencode--tool-calls (make-hash-table :test 'equal)))
       (setq opencode-session-id "session")
       (puthash "session" (current-buffer) opencode-session-buffers)
       (unwind-protect
           (progn ,@body)
         (delete-process process)))))

(ert-deftest opencode-read-tool-file-location-button-test ()
  "Format read offsets as working file-and-line buttons."
  (let* ((file-directory (file-name-directory
                          (locate-library "opencode-sessions")))
         (default-directory file-directory)
         (formatted (opencode--format-tool-call
                     "read" '((path . "opencode-sessions.el")
                              (offset . 440)
                              (limit . 35)))))
    (with-temp-buffer
      (setq default-directory file-directory)
      (insert formatted)
      (opencode--buttonize-file-references (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el:440")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'button-data)
                       (cons (expand-file-name "opencode-sessions.el"
                                               file-directory)
                             440)))))))

(ert-deftest opencode-session-markdown-fontification-test ()
  "Stream Markdown without replacing text or losing session decorations."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--output "# Heading\n\nSee `opencode-sessions")
    (opencode--output ".el` and *emphasis*.\n\n")
    (opencode--insert-tool-block
     "edit" '((path . "opencode-sessions.el")
              (oldString . "old") (newString . "new")))
    (opencode--output "**Testing button behavior**\n\n")
    (opencode--insert-block-with-margin
     (concat "emacs_eval_elisp:\n"
             (opencode--fenced-block "(+ 1 2)" "elisp"))
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    (opencode--show-prompt)
    (insert "pending input")
    (let ((text (buffer-substring-no-properties (point-min) (point-max)))
          (mark (marker-position (process-mark process)))
          (position (point)))
      (font-lock-ensure)
      ;; Refontifying code blocks must not clear earlier inline
      ;; Markdown, even with normal JIT font-lock enabled.
      (font-lock-flush)
      (font-lock-ensure)
      (should (equal text (buffer-substring-no-properties
                           (point-min) (point-max))))
      (should (= mark (marker-position (process-mark process))))
      (should (= position (point)))
      (should (equal "pending input" (buffer-substring-no-properties
                                      mark (point-max)))))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Heading")
      (should (memq 'markdown-ts-heading-1
                    (get-text-property (1- (point)) 'face)))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (car (button-get button 'button-data))
                       (expand-file-name "opencode-sessions.el"))))
      (goto-char (point-min))
      (search-forward "-old")
      (should (memq 'diff-removed
                    (get-text-property (1- (point)) 'face)))
      (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                       (overlays-at (1- (point)))))
      (search-forward "```")
      (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                           (overlays-at (- (point) 2))))
      (search-forward "**Testing button behavior**")
      (should (memq 'markdown-ts-bold
                    (get-text-property (- (point) 27) 'face)))
      (should (get-char-property (- (point) 27) 'invisible))
      (search-forward "(+ 1 2)")
      (should (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                       (overlays-at (1- (point)))))
      (search-forward "```")
      (should-not (cl-some (lambda (ov) (overlay-get ov 'line-prefix))
                           (overlays-at (- (point) 2))))
      (goto-char (point-min))
      (search-forward "> ")
      (should (get-char-property (- (point) 2) 'invisible)))))

(ert-deftest opencode-session-label-test ()
  "Show the label name, and send it as code along with the label's part."
  (dolist (case '(("AGENTS.md" . "`AGENTS.md`")
                  ("a`b" . "``a`b``")
                  ("`x`" . "`` `x` ``")))
    (with-opencode-session-test-buffer
      (opencode--output "Hi!\n\n")
      (opencode--show-prompt)
      (let ((start (point))
            (part (list (cons 'name (car case)))))
        (insert "see ")
        (opencode--insert-label (car case) part)
        (insert " now")
        (font-lock-ensure)
        (should (equal (car case)
                       (get-char-property (+ start 4) 'display)))
        (should (equal (cons (concat "see " (cdr case) " now") (list part))
                       (opencode--input-text-and-parts start (point))))
        (delete-region (+ start 4) (+ start 5))
        (should (equal (cons "see  now" nil)
                       (opencode--input-text-and-parts start (point))))))))

(ert-deftest opencode-session-label-typing-test ()
  "Text typed right after a label is sent as text, not as copies of the label."
  (with-opencode-session-test-buffer
    (opencode--output "Hi!\n\n")
    (opencode--show-prompt)
    (let ((start (point))
          (part '((name . "AGENTS.md"))))
      (opencode--insert-label "AGENTS.md" part)
      (dolist (char (string-to-list "hi"))
        (let ((last-command-event char))
          (self-insert-command 1)))
      (should (equal (cons "`AGENTS.md`hi" (list part))
                     (opencode--input-text-and-parts start (point)))))))

(ert-deftest opencode-session-diff-refinement-test ()
  "Refine entire synthetic hunks without touching input or earlier output."
  (with-opencode-session-test-buffer
    (let ((diff-refine 'font-lock))
      (opencode--output "Earlier output\n")
      (insert "pending input")
      (opencode--insert-tool-block
       "patch"
       '((patchText . "*** Begin Patch\n*** Update File: example.el\n@@\n-old first value\n-old second value\n+new first value\n+new second value\n@@\n-before last value\n+after last value\n*** End Patch")))
      (let ((text (buffer-substring-no-properties (point-min) (point-max)))
            (mark (marker-position (process-mark process)))
            (position (point)))
        (font-lock-ensure)
        (font-lock-flush)
        (font-lock-ensure)
        (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
        (should (= mark (process-mark process)))
        (should (= position (point)))
        (should (equal "pending input"
                       (buffer-substring-no-properties mark (point-max)))))
      (save-excursion
        (goto-char (point-min))
        (should-not (get-char-property (point) 'invisible))
        (dolist (change '(("old second" . diff-refine-removed)
                          ("new second" . diff-refine-added)
                          ("before last" . diff-refine-removed)
                          ("after last" . diff-refine-added)))
          (search-forward (car change))
          (should (cl-some
                   (lambda (overlay)
                     (eq (overlay-get overlay 'face) (cdr change)))
                   (overlays-at (- (point) (length (car change)))))))
        (goto-char (point-min))
        (while (search-forward "@@ -0 +0 @@" nil t)
          (should (get-char-property (- (point) 11) 'invisible)))))))

(ert-deftest opencode-session-diff-line-background-test ()
  "Show diff faces' backgrounds, keeping the code block background elsewhere."
  (with-opencode-session-test-buffer
    (opencode--output "```diff\n-old\n+new\n```\n\n```python\nx = 1\n```\n\n")
    (font-lock-ensure)
    (font-lock-flush)
    (font-lock-ensure)
    (cl-flet ((check-diff-faces ()
                (dolist (change '(("old" . diff-removed)
                                  ("new" . diff-added)))
                  (goto-char (point-min))
                  (search-forward (car change))
                  (let ((face (get-char-property (1- (point)) 'face)))
                    (should (memq (cdr change) (ensure-list face)))))))
      (check-diff-faces)
      ;; A chunk with only a fence line resets the overlay it doesn't touch.
      (dolist (fence '("```diff" "+new\n```"))
        (goto-char (point-min))
        (search-forward fence)
        (opencode--fontify-markdown (pos-bol) (pos-eol))
        (check-diff-faces)))
    (goto-char (point-min))
    (search-forward "x = 1")
    (should (eq 'markdown-ts-code-block-markup-hidden
                (get-char-property (1- (point)) 'face)))))

(ert-deftest opencode-session-hidden-fence-lines-test ()
  "Hide whole fence lines, including their newlines."
  (with-opencode-session-test-buffer
    (opencode--output "Text:\n\n```python\nx = 1\n```\n\nAfter\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "```python")
    (should (get-char-property (pos-bol) 'invisible))
    (should (get-char-property (pos-eol) 'invisible))
    (search-forward "```")
    (should (get-char-property (pos-eol) 'invisible))
    (search-forward "After")
    (should-not (get-char-property (1- (pos-bol)) 'invisible))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (with-opencode-session-test-buffer
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (let ((node (treesit-node-at (point) 'markdown)))
      (while (and node (not (equal (treesit-node-type node) "fenced_code_block")))
        (setq node (treesit-node-parent node)))
      (should node))
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(defun opencode-session-test--delta (type delta &optional ordinal)
  "Output DELTA of the block of TYPE and ORDINAL, through the event dispatcher."
  (opencode--handle-message
   `((type . ,(format "session.%s.delta" type))
     (data . ((sessionID . "session")
              (assistantMessageID . "message")
              (ordinal . ,(or ordinal 0))
              (delta . ,delta))))))

(defun opencode-session-test--ended (type text &optional ordinal)
  "End the block of TYPE and ORDINAL with TEXT, through the event dispatcher."
  (opencode--handle-message
   `((type . ,(format "session.%s.ended" type))
     (data . ((sessionID . "session")
              (assistantMessageID . "message")
              (ordinal . ,(or ordinal 0))
              (text . ,text))))))

(ert-deftest opencode-session-completed-tables-test ()
  "Align completed streamed tables once without disturbing pending input."
  (with-opencode-session-test-buffer
    (let ((align (symbol-function 'opencode--align-output-table))
          (calls 0))
      (cl-letf (((symbol-function 'opencode--align-output-table)
                 (lambda (beg end)
                   (cl-incf calls)
                   (funcall align beg end))))
        (insert "pending input")
        (opencode-session-test--delta 'text "| File | Value |\n")
        (opencode-session-test--delta 'text "| :--- | ---: |\n")
        (opencode-session-test--delta 'text "| `opencode-sessions.el` | 7 |\n")
        (should (= calls 0))
        ;; An incomplete following line cannot yet establish the boundary.
        (opencode-session-test--delta 'text "# Following")
        (should (= calls 0))
        (opencode-session-test--delta 'text " prose.\n")
        (should (= calls 1))
        (let ((output (buffer-substring-no-properties
                       (point-min) (process-mark process))))
          (should (string-match-p "| +7|" output))
          (should (string-suffix-p "Following prose.\n" output)))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))
        (should (= (point) (point-max)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "opencode-sessions.el")
          (should (button-at (1- (point))))
          (search-forward "7|")
          (should (eq (get-text-property (- (point) 3) 'field) 'output)))
        ;; A fenced table is code and must never be aligned.
        (opencode-session-test--delta
         'text "\n```markdown\n| Code | Value |\n| --- | --- |\n| x | y |\n```\n\n")
        (should (= calls 1))
        ;; A blank line completes a table before the text part ends.
        (opencode-session-test--delta
         'text "| Name | Value |\n| --- | --- |\n| x | y |\n\n")
        (should (= calls 2))
        (opencode-session-test--delta 'text "More prose.\n\n")
        (should (= calls 2))
        ;; Flush a final table without a trailing newline on part completion.
        (opencode-session-test--delta
         'text "| Last | Value |\n| --- | --- |\n| x | y |")
        (should (= calls 2))
        (opencode-session-test--ended 'text "ignored, as the block had deltas")
        (should (= calls 3))
        (should-not opencode--table-scan-marker)
        (should (eq (char-before (process-mark process)) ?|))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))))))

(ert-deftest opencode-session-reasoning-leading-newline-test ()
  "A streamed reasoning part starting with a newline adds no blank line or margin."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--delta 'reasoning "\n")
    (opencode-session-test--delta 'reasoning "Thinking")
    (should (equal (buffer-substring-no-properties
                    (point-min) (process-mark process))
                   "request\n\nThinking"))
    (dolist (pos (number-sequence (point-min) (+ (point-min) 8)))
      (should-not (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                            (overlays-at pos))))
    (should (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                      (overlays-at (+ (point-min) 9))))))

(defun opencode-session-test--folded-p (text)
  "Return non-nil if the block whose first line contains TEXT is folded."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (eq 'hs (get-char-property (pos-eol) 'invisible))))

(defun opencode-session-test--tool (tool status input &optional output call-id exit)
  "Send the events of the TOOL call with STATUS, INPUT, OUTPUT and EXIT status.
OUTPUT is the error message when STATUS is \"error\".  CALL-ID defaults to TOOL."
  (let* ((id (or call-id tool))
         (data `((sessionID . "session") (assistantMessageID . "message") (id . ,id))))
    (pcase status
      ("running"
       (opencode--handle-message
        `((type . "session.tool.input.started")
          (data . ((name . ,tool) ,@data))))
       (opencode--handle-message
        `((type . "session.tool.called")
          (data . ((input . ,input) (executed . :false) ,@data)))))
      ("completed"
       (opencode--handle-message
        `((type . "session.tool.success")
          (data . ((content . (((type . "text") (text . ,output))))
                   (metadata . ((exit . ,exit)))
                   (executed . :false)
                   ,@data)))))
      ("error"
       (opencode--handle-message
        `((type . "session.tool.failed")
          (data . ((error . ((type . "unknown") (message . ,output)))
                   (executed . :false)
                   ,@data))))))))

(ert-deftest opencode-session-folding-test ()
  "Fold reasoning, edit diffs and tool output, but not user shell output."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(shell-non-zero error))
          (edit '((path . "opencode-sessions.el")
                  (oldString . "old")
                  (newString . "new")))
          (grep '((pattern . "foo")))
          (shell '((command . "ls")))
          (agent-shell '((command . "uname")))
          (eval '((code . "(progn\n  nil)"))))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "Thinking\nmore thoughts")
      (should-not (opencode-session-test--folded-p "Thinking"))
      (opencode-session-test--tool "edit" "running" edit)
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (opencode-session-test--tool "edit" "completed" edit "Edit applied")
      (should-not (string-search "Edit applied" (buffer-string)))
      ;; Parallel calls: each output goes after its own call.
      (opencode-session-test--tool "grep" "running" grep)
      (opencode-session-test--tool "shell" "running" shell)
      (opencode-session-test--tool "shell" "running" agent-shell nil "agent-shell")
      (opencode-session-test--tool "eval" "running" eval)
      (opencode-session-test--tool "grep" "completed" grep "a.el:1\nb.el:2")
      (opencode-session-test--tool "shell" "completed" shell "ls-file")
      (opencode-session-test--tool "shell" "completed" agent-shell "agent-file"
                                   "agent-shell")
      (opencode-session-test--tool "eval" "completed" eval "result")
      (should (opencode-session-test--folded-p "grep \"foo\""))
      (should (opencode-session-test--folded-p "$ ls"))
      (should (opencode-session-test--folded-p "$ uname"))
      (should (opencode-session-test--folded-p "  nil)"))
      (should-not (opencode-session-test--folded-p "eval\n"))
      (should (string-suffix-p
               (concat "grep \"foo\"\n```text\na.el:1\nb.el:2\n```\n\n"
                       "$ ls\n```text\nls-file\n```\n\n"
                       "$ uname\n```text\nagent-file\n```\n\n"
                       "eval\n(progn\n  nil)\n```text\nresult\n```\n\n")
               (buffer-substring-no-properties
                (point-min) (opencode--session-process-position))))
      (should (eq (get-text-property (point-min) 'field) 'output))
      (goto-char (point-min))
      (search-forward "b.el")
      (should (eq (get-text-property (point) 'field) 'output))
      (should (get-text-property (point) 'read-only))
      (opencode-session-test--delta 'text "Done.")
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      ;; A user's shell command output isn't folded.
      (opencode--output "\n\n")
      (opencode--insert-shell-output "user-file" nil)
      (goto-char (point-min))
      (search-forward "user-file")
      (should-not (invisible-p (1- (point))))
      ;; Toggle a fold from its header line.
      (goto-char (point-min))
      (search-forward "Thinking")
      (hs-toggle-hiding)
      (should-not (opencode-session-test--folded-p "Thinking"))
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking"))
      ;; Refold from inside an expanded body.
      (hs-toggle-hiding)
      (search-forward "more")
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking"))
      ;; Outside any fold, toggling does nothing.
      (goto-char (point-min))
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking")))))

(ert-deftest opencode-session-shown-outputs-test ()
  "`opencode-shown-outputs' picks what starts unfolded; all of it can be folded."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(reasoning grep edit))
          (edit '((path . "opencode-sessions.el")
                  (oldString . "old")
                  (newString . "new")))
          (patch '((patchText . "*** Update File: a.el\n@@\n-old\n+new"))))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "Thinking\nmore thoughts")
      (opencode-session-test--tool "edit" "running" edit)
      (opencode-session-test--tool "patch" "running" patch)
      (opencode-session-test--tool "grep" "running" '((pattern . "foo")))
      (opencode-session-test--tool "glob" "running" '((pattern . "*.el")))
      (opencode-session-test--tool "grep" "completed" '((pattern . "foo"))
                                   "a.el:1\nb.el:2")
      (opencode-session-test--tool "glob" "completed" '((pattern . "*.el"))
                                   "a.el\nb.el")
      (should-not (opencode-session-test--folded-p "Thinking"))
      (should-not (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (should (opencode-session-test--folded-p "patch:"))
      (should-not (opencode-session-test--folded-p "grep \"foo\""))
      (should (opencode-session-test--folded-p "glob \"*.el\""))
      ;; Shown blocks can still be folded, and unfolded again.
      (dolist (header '("Thinking" "edit opencode-sessions.el:" "grep \"foo\""))
        (goto-char (point-min))
        (search-forward header)
        (hs-toggle-hiding)
        (should (opencode-session-test--folded-p header))
        (hs-toggle-hiding)
        (should-not (opencode-session-test--folded-p header))))))

(ert-deftest opencode-session-shown-outputs-shell-test ()
  "`shell-non-zero' shows only failed commands, `shell' shows all of them."
  (with-opencode-session-test-buffer
    (let ((run (lambda (command output exit)
                 (let ((input `((command . ,command))))
                   (opencode-session-test--tool "shell" "running" input nil command)
                   (opencode-session-test--tool "shell" "completed" input output
                                                command exit)))))
      (opencode--output "request\n\n")
      (let ((opencode-shown-outputs '(shell-non-zero)))
        (funcall run "good" "out1" 0)
        (funcall run "bad" "out2" 1)
        (funcall run "killed" "out3" nil))
      (let ((opencode-shown-outputs '(shell)))
        (funcall run "good2" "out5" 0))
      (let ((opencode-shown-outputs nil))
        (funcall run "bad2" "out6" 1))
      ;; Each search stops at the first match, before any later "2" command.
      (should (opencode-session-test--folded-p "$ good"))
      (should-not (opencode-session-test--folded-p "$ bad"))
      (should (opencode-session-test--folded-p "$ killed"))
      (should-not (opencode-session-test--folded-p "$ good2"))
      (should (opencode-session-test--folded-p "$ bad2"))
      ;; Only non-zero exits are styled as errors.
      (should (opencode-session-test--error-styled-p "out2"))
      (should-not (opencode-session-test--error-styled-p "out5")))))

(ert-deftest opencode-session-shown-outputs-replay-test ()
  "Replayed reasoning follows `opencode-shown-outputs'."
  (with-opencode-session-test-buffer
    (let ((replay
           (lambda (text)
             (opencode--replay-session-messages
              `(((id . "msg_1") (type . "assistant") (agent . "build")
                 (time . ((created . 1) (completed . 2)))
                 (tokens . ((input . 0) (output . 0) (reasoning . 0)
                            (cache . ((read . 0) (write . 0)))))
                 (content . (((type . "reasoning")
                              (text . ,(concat text "\nmore thoughts")))))))))))
      (let ((opencode-shown-outputs '(reasoning)))
        (funcall replay "Shown"))
      (let ((opencode-shown-outputs nil))
        (funcall replay "Hidden"))
      (should-not (opencode-session-test--folded-p "Shown"))
      (should (opencode-session-test--folded-p "Hidden")))))

(defun opencode-session-test--error-styled-p (text)
  "Return non-nil if TEXT has the `error' face and margin, and is visible."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (let ((pos (match-beginning 0)))
      (and (eq 'error (get-char-property pos 'face))
           (not (invisible-p pos))
           (seq-some (lambda (ov)
                       (when-let* ((prefix (overlay-get ov 'line-prefix))
                                   (display (get-text-property 0 'display prefix)))
                         (eq 'error (get-text-property 0 'face (cadr display)))))
                     (overlays-at pos))))))

(ert-deftest opencode-session-tool-error-test ()
  "Failed calls show their error, including tools that normally hide output."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error))
          (eval '((expression . "(car 1)")))
          (edit '((path . "opencode-sessions.el")
                  (oldString . "old")
                  (newString . "new"))))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "emacs_eval_elisp" "running" eval)
      (opencode-session-test--tool "edit" "running" edit)
      (opencode-session-test--tool "emacs_eval_elisp" "error" eval "Wrong type")
      (opencode-session-test--tool "edit" "error" edit "oldString not found")
      (should-not (opencode-session-test--folded-p "(car 1)"))
      (should (opencode-session-test--error-styled-p "Wrong type"))
      ;; The edit's diff is folded, but its error stays visible below it.
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (should (opencode-session-test--error-styled-p "oldString not found")))))

(ert-deftest opencode-session-tool-error-folded-test ()
  "Errors start folded when `error' is not in `opencode-shown-outputs'."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs nil)
          (eval '((expression . "(car 1)"))))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "emacs_eval_elisp" "running" eval)
      (opencode-session-test--tool "emacs_eval_elisp" "error" eval "Wrong type")
      (should (opencode-session-test--folded-p "(car 1)")))))

(ert-deftest opencode-session-tool-error-replay-test ()
  "Replayed failed calls show their error."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error)))
      (opencode--replay-session-messages
       '(((id . "msg_1") (type . "assistant") (agent . "build")
          (time . ((created . 1) (completed . 2)))
          (tokens . ((input . 0) (output . 0) (reasoning . 0)
                     (cache . ((read . 0) (write . 0)))))
          (content . (((type . "tool")
                       (id . "call")
                       (name . "emacs_eval_elisp")
                       (state . ((status . "error")
                                 (input . ((expression . "(car 1)")))
                                 (error . ((type . "unknown")
                                           (message . "Wrong type")))))))))))
      (should-not (opencode-session-test--folded-p "(car 1)"))
      (should (opencode-session-test--error-styled-p "Wrong type")))))

(ert-deftest opencode-conversation-tool-error-test ()
  "Exported conversations include the error of failed tool calls."
  (should (string-search
           "Error:\n\n```text\nWrong type\n```"
           (opencode--conversation-format-tool-item
            '((name . "emacs_eval_elisp")
              (state . ((status . "error")
                        (error . ((type . "unknown") (message . "Wrong type"))))))))))

(ert-deftest opencode-session-tool-output-margin-after-fenced-call-test ()
  "Output's margin covers a fenced call's closing fence, but only once it's there.
The first visible output line starts at that hidden fence."
  (with-opencode-session-test-buffer
    (let ((input '((expression . "(+ 1 2)")))
          (fence-margin-p
           (lambda ()
             (save-excursion
               (goto-char (point-min))
               (search-forward "(+ 1 2)")
               (search-forward "```")
               (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                         (overlays-at (- (point) 3)))))))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "emacs_eval_elisp" "running" input)
      (should-not (funcall fence-margin-p))
      (opencode-session-test--tool "emacs_eval_elisp" "completed" input "3")
      (should (funcall fence-margin-p)))))

(ert-deftest opencode-session-table-alignment-context-test ()
  "Table alignment must not schedule a context mode in the session buffer."
  (with-opencode-session-test-buffer
    (insert "| Name | Value |\n| --- | --- |\n| x | y |\n")
    (let ((markdown-ts-enable-table-mode t)
          (markdown-ts-enable-code-block-context-mode t)
          (timer (symbol-function 'run-with-timer))
          context-timers)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (seconds repeat function &rest args)
                   (if (memq function '(markdown-ts--enable-in-table-mode
                                        markdown-ts--enable-code-block-in-context-mode))
                       (push function context-timers)
                     (apply timer seconds repeat function args)))))
        (opencode--align-output-table (point-min) (point-max)))
      (should-not context-timers)
      (should-not markdown-ts-table-mode)
      (should-not markdown-ts-in-table-mode)
      (should (eq (key-binding (kbd "RET")) 'comint-send-input)))))

(ert-deftest opencode-session-history-table-alignment-test ()
  "Align replayed assistant tables, preserving user requests and code fences."
  (with-opencode-session-test-buffer
    (let* ((table "| Name | Value |\n| --- | ---: |\n| x | y |")
           (fenced (concat "```markdown\n" table "\n```"))
           (messages
            `(((id . "msg_1") (type . "user") (text . ,table))
              ((id . "msg_2") (type . "assistant") (agent . "build")
               (time . ((created . 1) (completed . 2)))
               (tokens . ((input . 0) (output . 0) (reasoning . 0)
                          (cache . ((read . 0) (write . 0)))))
               (content . (((type . "text") (text . ,table))
                           ((type . "text") (text . ,fenced))))))))
      ;; Capture the request separately to verify it reaches the user
      ;; renderer without going through the table alignment path.
      (cl-letf (((symbol-function 'opencode--replay-user-request)
                 (lambda (message)
                   (opencode--output (alist-get 'text message))
                   (opencode--output "\n\n"))))
        (opencode--replay-session-messages messages))
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-prefix-p (concat table "\n\n") output))
        (should (string-match-p "|x +| +y|" output))
        (should (string-suffix-p (concat fenced "\n\n") output)))
      (should-not opencode--table-scan-marker)
      (should (= (process-mark process) (point-max)))
      (should-not markdown-ts-in-table-mode))))

(defmacro opencode-session-test--with-stubbed-api (active &rest body)
  "Run BODY with api requests answered, and toasts and alerts discarded.
Requests for active sessions return ACTIVE, and others a session."
  (declare (indent 1))
  `(let ((opencode-toast-function #'ignore)
         (opencode-alerted-sessions nil)
         (opencode-file-edited-functions nil))
     (cl-letf (((symbol-function 'opencode-api--request)
                (lambda (_method path &rest args)
                  (funcall (plist-get args :then)
                           (if (equal path "/api/session/active")
                               ,active
                             '((id . "session") (title . "Title")))))))
       ,@body)))

(defun opencode-session-test--at-prompt-p ()
  "Return non-nil if the session's output ends with a prompt."
  (string-suffix-p "> " (buffer-substring-no-properties
                         (point-min) (process-mark (get-buffer-process
                                                    (current-buffer))))))

(ert-deftest opencode-session-event-text-deltas-test ()
  "Text is output from its deltas, trimming only the start of the block."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--delta 'text "\n\n  Hello")
    (opencode-session-test--delta 'text " world\n")
    (opencode-session-test--ended 'text "\n\n  Hello world\n")
    (should (equal "request\n\nHello world\n"
                   (buffer-substring-no-properties
                    (point-min) (process-mark process))))))

(ert-deftest opencode-session-event-only-ended-text-test ()
  "A block that only has final text, and no deltas, is output once."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--delta 'text "First.")
    (opencode-session-test--ended 'text "First.")
    (opencode-session-test--ended 'text "Second." 1)
    (opencode-session-test--ended 'text "  \n" 2)
    (should (equal "request\n\nFirst.\n\nSecond."
                   (buffer-substring-no-properties
                    (point-min) (process-mark process))))))

(ert-deftest opencode-session-event-reasoning-then-text-test ()
  "Text and reasoning number their ordinals separately, so are different blocks."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(reasoning)))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "thinking")
      (opencode-session-test--ended 'reasoning "thinking")
      (opencode-session-test--ended 'text "answer")
      (should (equal "request\n\nthinking\n\nanswer"
                     (buffer-substring-no-properties
                      (point-min) (process-mark process))))
      (should opencode-last-response-start))))

(ert-deftest opencode-session-event-parallel-tools-test ()
  "Parallel tools finishing out of order put their output after their own call.
Call ids are only unique within a step, so they're keyed with the message too."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(grep)))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "grep" "running" '((pattern . "one")) nil "a")
      (opencode-session-test--tool "grep" "running" '((pattern . "two")) nil "b")
      (opencode-session-test--tool "grep" "completed" '((pattern . "two")) "out-b" "b")
      (opencode-session-test--tool "grep" "completed" '((pattern . "one")) "out-a" "a")
      (should (string-suffix-p
               (concat "grep \"one\"\n```text\nout-a\n```\n\n"
                       "🔍 grep \"two\"\n```text\nout-b\n```\n\n")
               (buffer-substring-no-properties
                (point-min) (process-mark process))))
      (should (zerop (hash-table-count opencode--tool-calls))))))

(ert-deftest opencode-session-event-tool-failed-without-called-test ()
  "A tool call that fails with malformed input is never called, but still shown."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error)))
      (opencode--output "request\n\n")
      (opencode--handle-message
       '((type . "session.tool.input.started")
         (data . ((sessionID . "session") (assistantMessageID . "message")
                  (id . "call") (name . "edit")))))
      (opencode-session-test--tool "edit" "error" nil "Malformed input" "call")
      (should (string-match-p "\\`request\n\nedit\n```text\nMalformed input\n```"
                              (buffer-substring-no-properties
                               (point-min) (process-mark process))))
      (should (opencode-session-test--error-styled-p "Malformed input")))))

(ert-deftest opencode-session-event-question-tool-test ()
  "A question tool call has no block of its own, even when it's dismissed."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--tool "question" "running" '((questions)) nil "call")
    (opencode-session-test--tool "question" "error" nil "dismissed" "call")
    (should (equal "request\n\n"
                   (buffer-substring-no-properties
                    (point-min) (process-mark process))))))

(ert-deftest opencode-session-event-execution-test ()
  "Ending an execution goes back to a prompt, unless it'll resume after a shutdown."
  (dolist (case '(("session.execution.succeeded" nil t)
                  ("session.execution.interrupted" ((reason . "user")) t)
                  ("session.execution.interrupted" ((reason . "shutdown")) t)
                  ("session.execution.failed"
                   ((error . ((type . "unknown") (message . "It broke")))) t)))
    (with-opencode-session-test-buffer
      (opencode-session-test--with-stubbed-api nil
        (opencode--output "request\n\n")
        (setq opencode-session-status "busy")
        (opencode--handle-message
         `((type . ,(car case))
           (data . ((sessionID . "session") ,@(cadr case)))))
        (should (equal opencode-session-status "idle"))
        (should (eq (opencode-session-test--at-prompt-p) (caddr case)))
        (when (equal (car case) "session.execution.failed")
          (should (string-search "It broke" (buffer-string)))))))
  ;; A real shutdown leaves the drain running, to resume after the restart.
  (with-opencode-session-test-buffer
    (opencode-session-test--with-stubbed-api '((session . ((type . "running"))))
      (opencode--output "request\n\n")
      (setq opencode-session-status "busy")
      (opencode--handle-message
       '((type . "session.execution.interrupted")
         (data . ((sessionID . "session") (reason . "shutdown")))))
      (should (equal opencode-session-status "busy"))
      (should-not (opencode-session-test--at-prompt-p)))))

(ert-deftest opencode-session-event-shell-test ()
  "A user's shell command isn't an execution, so it goes back to a prompt itself."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode--handle-message
     '((type . "session.shell.ended")
       (data . ((sessionID . "session")
                (shell . ((status . "exited") (exit . 2)))
                (output . ((output . "bad output\n")))))))
    (should (opencode-session-test--at-prompt-p))
    (should (opencode-session-test--error-styled-p "bad output"))))

(ert-deftest opencode-session-event-selection-test ()
  "Selections made elsewhere are followed, and ours don't disturb local state."
  (with-opencode-session-test-buffer
    (setq opencode-session-agents (list '((id . "build") (name . "Build"))
                                        '((id . "plan") (name . "Plan")))
          opencode-session-agent (car opencode-session-agents)
          opencode--server-agent "build")
    (opencode--handle-message
     '((type . "session.agent.selected")
       (data . ((sessionID . "session") (agent . "plan")))))
    (should (equal "plan" (alist-get 'id opencode-session-agent)))
    (opencode--handle-message
     '((type . "session.model.selected")
       (data . ((sessionID . "session")
                (model . ((id . "m") (providerID . "p")))))))
    (should (equal "m" (map-nested-elt opencode-session-agent '(model id))))
    ;; An unsent local change isn't undone by an event for what's selected.
    (setq opencode-session-agent (car opencode-session-agents))
    (opencode--handle-message
     '((type . "session.agent.selected")
       (data . ((sessionID . "session") (agent . "plan")))))
    (should (equal "build" (alist-get 'id opencode-session-agent)))))

(ert-deftest opencode-session-status-indicator-agent-test ()
  "The mode line never shows nil for an agent."
  (with-opencode-session-test-buffer
    (cl-flet ((indicator (agent)
                (let ((opencode-session-agent agent))
                  (opencode--session-status-indicator))))
      (should (string-search "🤖 Plan" (indicator '((id . "plan") (name . "Plan")))))
      (should (string-search "🤖 plan" (indicator '((id . "plan")))))
      (should (string-search "🤖 -" (indicator '((model . ((id . "m")))))))
      (should-not (string-search "nil" (indicator nil))))))

(ert-deftest opencode-open-project-buffers-test ()
  "A control buffer is named after the main checkout of its project."
  (let (opencode--session-control-buffers buffers)
    (cl-letf* (((symbol-function 'opencode-sessions-redisplay) #'ignore)
               ((symbol-function 'pop-to-buffer)
                (lambda (buffer) (push buffer buffers)))
               ((symbol-function 'opencode-api--request)
                (lambda (_method _path &rest args)
                  (funcall
                   (plist-get args :then)
                   (pcase default-directory
                     ((or "/src/repo/" "/src/worktree/")
                      '((project . ((id . "repo") (canonical . "/src/repo")))))
                     ("/other/repo/"
                      '((project . ((id . "other") (canonical . "/other/repo")))))
                     ("/scratch/"
                      '((project . ((id . "global") (canonical . "/"))))))))))
      (unwind-protect
          (progn
            (opencode-open-project "/src/repo")
            (should (equal "*OpenCode Sessions: repo*" (buffer-name (car buffers))))
            ;; A worktree shares the buffer, and new sessions go in the worktree.
            (opencode-open-project "/src/worktree/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "/src/worktree/"
                           (buffer-local-value 'default-directory (car buffers))))
            ;; Same name, different project: it shows the one last opened.
            (opencode-open-project "/other/repo/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "other"
                           (buffer-local-value 'opencode-session-control-project-id
                                               (car buffers))))
            (opencode-open-project "/scratch/")
            (should (equal "*OpenCode Sessions: global*" (buffer-name (car buffers)))))
        (mapc #'kill-buffer (delete-dups buffers))))))

(ert-deftest opencode-session-default-variant-test ()
  "The server reports no variant as \"default\", which is no selection."
  (with-opencode-session-test-buffer
    (cl-letf (((symbol-function 'opencode--current-model)
               (lambda () '((name . "Model"))))
              ((symbol-function 'window-width) (lambda () 200)))
      (cl-flet ((indicator (variant)
                  (let ((opencode-session-agent
                         `((id . "a") (model . ((id . "m") (variant . ,variant))))))
                    (opencode--session-status-indicator))))
        (should-not (string-search "default" (indicator "default")))
        (should (string-search "Model high" (indicator "high")))))))

(ert-deftest opencode-session-sync-only-what-differs-test ()
  "Switching agent always records a message, so is only done if it differs."
  (with-opencode-session-test-buffer
    (let (requests sent)
      (cl-letf (((symbol-function 'opencode-api--request)
                 (lambda (_method path &rest args)
                   (push path requests)
                   (funcall (plist-get args :then) nil))))
        (setq opencode-session-agent '((id . "build")
                                       (model . ((id . "m") (providerID . "p"))))
              opencode--server-agent "build")
        (opencode--sync-agent-and-model (lambda () (setq sent t)))
        (should sent)
        (should (equal requests '("/api/session/session/model")))
        (setq requests nil
              opencode--server-agent "plan")
        (opencode--sync-agent-and-model #'ignore)
        (should (equal requests '("/api/session/session/model"
                                  "/api/session/session/agent")))
        (should (equal opencode--server-agent "build"))))))

(ert-deftest opencode-session-load-selection-test ()
  "Opening a session selects what it has, or what its messages used if migrated."
  (with-opencode-session-test-buffer
    (let ((opencode-session-agents '(((id . "build") (name . "Build"))
                                     ((id . "plan") (name . "Plan"))))
          (opencode-agents '(((id . "build") (name . "Build") (mode . "primary")))))
      (cl-flet ((load-session (session messages)
                  (cl-letf (((symbol-function 'opencode-api--fetch-all-messages)
                             (lambda (_id callback) (funcall callback messages)))
                            ((symbol-function 'opencode--replay-session-messages) #'ignore)
                            ((symbol-function 'opencode-api--request) #'ignore))
                    (setq opencode-session-agent (car opencode-session-agents))
                    (opencode--load-session `((id . "session") ,@session))))
                (assistant (agent)
                  `((id . "m") (type . "assistant") (agent . ,agent))))
        (load-session '((agent . "plan") (model . ((id . "m") (providerID . "p"))))
                      (list (assistant "build")))
        (should (equal "plan" (alist-get 'id opencode-session-agent)))
        (should (equal "m" (map-nested-elt opencode-session-agent '(model id))))
        (load-session nil (list (assistant "build") (assistant "plan")))
        (should (equal "plan" opencode--server-agent))
        (should (equal "plan" (alist-get 'id opencode-session-agent)))
        ;; A new session keeps the local selection, and has the default on the server.
        (load-session nil nil)
        (should (equal "build" opencode--server-agent))
        (should (equal "build" (alist-get 'id opencode-session-agent)))))))

(ert-deftest opencode-session-replay-test ()
  "Replay prompts, a staged revert, and calls that later events finish."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(grep)))
      (opencode--replay-session-messages
       '(((id . "msg_1") (type . "user") (text . "first"))
         ((id . "msg_2") (type . "shell") (shellID . "sh") (command . "ls")
          (status . "exited") (exit . 0) (output . ((output . "listing"))))
         ((id . "msg_3") (type . "agent-switched") (agent . "plan"))
         ((id . "msg_4") (type . "synthetic") (text . "not shown"))
         ((id . "msg_5") (type . "assistant") (agent . "plan")
          (model . ((id . "m") (providerID . "p")))
          (time . ((created . 1)))
          (content . (((type . "text") (text . "partial"))
                      ((type . "tool") (id . "call") (name . "grep")
                       (state . ((status . "running")
                                 (input . ((pattern . "foo")))
                                 (metadata))))
                      ((type . "tool") (id . "later") (name . "grep")
                       (state . ((status . "streaming") (input . "{")))))))
         ((id . "msg_6") (type . "user") (text . "reverted")))
       "msg_6")
      (let ((output (buffer-string)))
        (should (string-search "first" output))
        (should (string-search "!ls" output))
        (should (string-search "listing" output))
        (should-not (string-search "not shown" output))
        (should-not (string-search "reverted" output)))
      ;; The running call is finished by a later event.
      (opencode--handle-message
       '((type . "session.tool.success")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (id . "call")
                  (content . (((type . "text") (text . "matched")))) (executed . t)))))
      (should (string-search "grep \"foo\"\n```text\nmatched" (buffer-string)))
      ;; The call replayed while streaming keeps its name once called.
      (opencode--handle-message
       '((type . "session.tool.called")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (id . "later")
                  (input . ((pattern . "bar")))))))
      (should (string-search "grep \"bar\"" (buffer-string)))
      ;; The partly replayed block isn't repeated when it ends.
      (opencode--handle-message
       '((type . "session.text.ended")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (ordinal . 0)
                  (text . "partial and the rest")))))
      (should-not (string-search "the rest" (buffer-string))))))

(ert-deftest opencode-session-replay-migrated-tools-test ()
  "Sessions migrated from v1 have old tool and input names."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs nil))
      (opencode--replay-session-messages
       '(((id . "msg_1") (type . "assistant") (agent . "build")
          (time . ((created . 1) (completed . 2)))
          (content . (((type . "tool") (id . "a") (name . "bash")
                       (state . ((status . "completed")
                                 (input . ((command . "ls")))
                                 (content . (((type . "text") (text . "out"))))
                                 (metadata . ((exit . 0))))))
                      ((type . "tool") (id . "b") (name . "read")
                       (state . ((status . "completed")
                                 (input . ((filePath . "x.el")))
                                 (content . (((type . "text") (text . "out"))))))))))))
      (should (string-search "$ ls" (buffer-string)))
      (should (string-search "read x.el" (buffer-string))))))

(ert-deftest opencode-session-prompt-body-test ()
  "Split label parts into the fields of a prompt, omitting empty ones."
  (should (equal '((text . "hi")) (opencode--prompt-body "hi" nil)))
  (let ((file (opencode--file-attachment "/tmp/a b.txt" "a b.txt"))
        (text (opencode--text-attachment "é" "region"))
        (agent '(agents . ((name . "explore")))))
    (should (equal "file:///tmp/a%20b.txt" (alist-get 'uri (cdr file))))
    (should (equal "data:text/plain;base64,w6k=" (alist-get 'uri (cdr text))))
    (should (equal `((text . "hi")
                     (files . ,(vector (cdr file) (cdr text)))
                     (agents . ,(vector (cdr agent))))
                   (opencode--prompt-body "hi" (list file agent text))))))

(ert-deftest opencode-session-attachable-file-test ()
  "Files that look like binaries the server would silently drop can't be attached."
  (let ((file (make-temp-file "opencode-test")))
    (unwind-protect
        (cl-flet ((with-bytes (bytes)
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region bytes nil file nil 'silent))
                    (opencode--attachable-file-p file)))
          (should (with-bytes "plain text é\n"))
          (should (with-bytes ""))
          (should (with-bytes "\x89PNG\r\n\x1a\n\0\0"))
          (should (with-bytes "%PDF-1.7\0"))
          (should (with-bytes "RIFF\0\0\0\0WEBPVP8 "))
          (should (with-bytes "\xff\xd8\xff\0"))
          (should-not (with-bytes "binary\0data"))
          (should-not (with-bytes "RIFF\0\0\0\0WAVE"))
          (should-not (with-bytes "\xff\xfe\xfd invalid utf-8"))
          ;; A multibyte character cut in half by the size limit, in any position.
          (dolist (padding '(0 1 2 3))
            (should (with-bytes (concat (make-string (+ padding (* 1024 1024)) ?a)
                                        "\xf0\x9f\x98\x80")))
            (should (with-bytes (concat (make-string (- (* 1024 1024) padding) ?a)
                                        "\xf0\x9f\x98\x80")))))
      (delete-file file)))
  (should (opencode--attachable-file-p temporary-file-directory)))

(ert-deftest opencode-question-form-reply-test ()
  "A question form becomes questions, and answers are by field type."
  (let* ((form '((id . "frm_1") (sessionID . "session") (title . "Questions")
                 (metadata . ((kind . "question")))
                 (fields . (((key . "q0") (title . "Letter") (description . "Pick a letter")
                             (type . "string") (custom . t)
                             (options . (((value . "A") (label . "A")))))
                            ((key . "q1") (title . "Numbers") (description . "Pick numbers")
                             (type . "multiselect") (custom . t)
                             (options . (((value . "1") (label . "One"))))))))) 
         (opencode-question--questions (opencode--form-questions form))
         (opencode-question--answers '(("1" "custom") ("A"))))
    (should (equal "Pick a letter"
                   (alist-get 'question (aref opencode-question--questions 0))))
    (should (vectorp (alist-get 'options (aref opencode-question--questions 0))))
    (should (equal '((q0 . "A") (q1 . ["1" "custom"]))
                   (opencode-question--answer)))
    (should (equal "{\"answer\":{\"q0\":\"A\",\"q1\":[\"1\",\"custom\"]}}"
                   (json-encode `((answer . ,(opencode-question--answer))))))))

(ert-deftest opencode-unsupported-form-test ()
  "A form that isn't a question is cancelled with a message so nothing hangs."
  (let (path toast)
    (cl-letf (((symbol-function 'opencode-api--request)
               (lambda (method request-path &rest _args)
                 (should (eq method 'delete))
                 (setq path request-path))))
      (let ((opencode-toast-function (lambda (properties) (setq toast properties))))
        (opencode--form-request '((id . "frm_1") (sessionID . "global")
                                  (title . "Login") (fields)))))
    (should (string-match-p "\\`/api/session/global/form/frm_1\\?message=opencode\\.el%20"
                            path))
    (should (string-search "Login" (alist-get 'message toast)))))

(ert-deftest opencode-json-falsy-test ()
  "All of JSON's false values are recognized, as `:false' is truthy in Lisp."
  (dolist (value '(nil :null :false 0))
    (should (opencode--json-falsy value)))
  (dolist (value '(t 1 "text"))
    (should-not (opencode--json-falsy value))))

(ert-deftest opencode-api-all-messages-test ()
  "Follow cursors through all pages, and return messages oldest first."
  (let (paths result)
    (cl-letf (((symbol-function 'opencode-api--request)
               (lambda (_method path &rest args)
                 (push path paths)
                 (funcall (plist-get args :then)
                          (if (string-search "cursor=" path)
                              '((data . (((id . "1"))))
                                (cursor . ((previous . "p") (next . :null))))
                            '((data . (((id . "3")) ((id . "2"))))
                              (cursor . ((next . "a=b")))))))))
      (opencode-api-all-messages ("session")
          messages
        (setq result messages)))
    (should (equal '("1" "2" "3") (mapcar (lambda (m) (alist-get 'id m)) result)))
    (should (equal "/api/session/session/message?limit=200&cursor=a%3Db" (car paths)))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here

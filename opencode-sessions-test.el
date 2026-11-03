;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode-sessions)

(defmacro with-opencode-session-test-buffer (&rest body)
  "Run BODY in a temporary `opencode-session-mode' buffer.
BODY can refer to the buffer's process as `process'.  The default directory
is the opencode source directory, and session state is bound so that parts
for session \"session\" and assistant message \"message\" render here."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq default-directory
           (file-name-directory (locate-library "opencode-sessions")))
     (opencode-session-mode)
     (let ((process (start-process "opencode-test" (current-buffer) nil))
           (opencode-session-buffers (make-hash-table :test 'equal))
           (opencode-part-type (make-hash-table :test 'equal))
           (opencode-assistant-messages (list (cons "message" nil))))
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
                     "read" '((filePath . "opencode-sessions.el")
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
     "edit" '((filePath . "opencode-sessions.el")
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
       "apply_patch"
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

(ert-deftest opencode-session-completed-tables-test ()
  "Align completed streamed tables once without disturbing pending input."
  (with-opencode-session-test-buffer
    (let ((part '((sessionID . "session") (messageID . "message") (text . "")))
          (align (symbol-function 'opencode--align-output-table))
          (calls 0))
      (cl-letf (((symbol-function 'opencode--align-output-table)
                 (lambda (beg end)
                   (cl-incf calls)
                   (funcall align beg end))))
        (insert "pending input")
        (opencode-session--update-part "text" part "| File | Value |\n" "text")
        (opencode-session--update-part "text" part "| :--- | ---: |\n" "text")
        (opencode-session--update-part
         "text" part "| `opencode-sessions.el` | 7 |\n" "text")
        (should (= calls 0))
        ;; An incomplete following line cannot yet establish the boundary.
        (opencode-session--update-part "text" part "# Following" "text")
        (should (= calls 0))
        (opencode-session--update-part "text" part " prose.\n" "text")
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
        (opencode-session--update-part
         "text" part "\n```markdown\n| Code | Value |\n| --- | --- |\n| x | y |\n```\n\n"
         "text")
        (should (= calls 1))
        ;; A blank line completes a table before the text part ends.
        (opencode-session--update-part
         "text" part "| Name | Value |\n| --- | --- |\n| x | y |\n\n" "text")
        (should (= calls 2))
        (opencode-session--update-part "text" part "More prose.\n\n" "text")
        (should (= calls 2))
        ;; Flush a final table without a trailing newline on part completion.
        (opencode-session--update-part
         "text" part "| Last | Value |\n| --- | --- |\n| x | y |" "text")
        (should (= calls 2))
        (opencode-session--update-part
         "text" (append '((time . ((end . 1)))) part) nil "text")
        (should (= calls 3))
        (should-not opencode--table-scan-marker)
        (should (eq (char-before (process-mark process)) ?|))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))))))

(ert-deftest opencode-session-reasoning-leading-newline-test ()
  "A streamed reasoning part starting with a newline adds no blank line or margin."
  (with-opencode-session-test-buffer
    (let ((part '((sessionID . "session") (messageID . "message") (text . ""))))
      (opencode--output "request\n\n")
      (opencode-session--update-part "reasoning" part nil "reasoning")
      (opencode-session--update-part "reasoning" part "\n" "reasoning")
      (opencode-session--update-part "reasoning" part "Thinking" "reasoning")
      (should (equal (buffer-substring-no-properties
                      (point-min) (process-mark process))
                     "request\n\nThinking"))
      (dolist (pos (number-sequence (point-min) (+ (point-min) 8)))
        (should-not (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                              (overlays-at pos))))
      (should (seq-some (lambda (ov) (overlay-get ov 'line-prefix))
                        (overlays-at (+ (point-min) 9)))))))

(defun opencode-session-test--folded-p (text)
  "Return non-nil if the block whose first line contains TEXT is folded."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (eq 'hs (get-char-property (pos-eol) 'invisible))))

(defun opencode-session-test--tool (tool status input &optional output title call-id)
  "Display the TOOL call part with STATUS, INPUT, OUTPUT and TITLE.
CALL-ID defaults to TOOL."
  (opencode-session--update-part
   tool `((sessionID . "session") (messageID . "message")
          (tool . ,tool) (callID . ,(or call-id tool))
          (state . ((status . ,status) (input . ,input) (output . ,output)
                    (title . ,title))))
   nil "tool"))

(ert-deftest opencode-session-folding-test ()
  "Fold reasoning, edit diffs and tool output, but not user shell output."
  (with-opencode-session-test-buffer
    (let ((part '((sessionID . "session") (messageID . "message") (text . "")))
          (edit '((filePath . "opencode-sessions.el")
                  (oldString . "old")
                  (newString . "new")))
          (grep '((pattern . "foo")))
          (bash '((command . "ls")))
          (agent-bash '((command . "uname")))
          (eval '((code . "(progn\n  nil)"))))
      (setq opencode--tool-calls-displayed (make-hash-table :test 'equal))
      (opencode--output "request\n\n")
      (opencode-session--update-part
       "reasoning" part "Thinking\nmore thoughts" "reasoning")
      (should-not (opencode-session-test--folded-p "Thinking"))
      (opencode-session-test--tool "edit" "running" edit)
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (opencode-session-test--tool "edit" "completed" edit "Edit applied")
      (should-not (string-search "Edit applied" (buffer-string)))
      ;; Parallel calls: each output goes after its own call.
      (opencode-session-test--tool "grep" "running" grep)
      (opencode-session-test--tool "bash" "running" bash)
      (opencode-session-test--tool "bash" "running" agent-bash nil nil "agent-bash")
      (opencode-session-test--tool "eval" "running" eval)
      (opencode-session-test--tool "grep" "completed" grep "a.el:1\nb.el:2" "foo")
      ;; Only a user's shell command has an empty title.
      (opencode-session-test--tool "bash" "completed" bash "user-file" "")
      (opencode-session-test--tool "bash" "completed" agent-bash "agent-file"
                                   "uname" "agent-bash")
      (opencode-session-test--tool "eval" "completed" eval "result")
      (should (opencode-session-test--folded-p "grep \"foo\""))
      (should-not (opencode-session-test--folded-p "$ ls"))
      (should (opencode-session-test--folded-p "$ uname"))
      (should (opencode-session-test--folded-p "  nil)"))
      (should-not (opencode-session-test--folded-p "eval\n"))
      (should (string-suffix-p
               (concat "grep \"foo\"\n```text\na.el:1\nb.el:2\n```\n\n"
                       "$ ls\n```text\nuser-file\n```\n\n"
                       "$ uname\n```text\nagent-file\n```\n\n"
                       "eval\n(progn\n  nil)\n```text\nresult\n```\n\n")
               (buffer-substring-no-properties
                (point-min) (opencode--session-process-position))))
      (should (eq (get-text-property (point-min) 'field) 'output))
      (goto-char (point-min))
      (search-forward "b.el")
      (should (eq (get-text-property (point) 'field) 'output))
      (should (get-text-property (point) 'read-only))
      (opencode-session--update-part "text" part "Done." "text")
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
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
      (setq opencode--tool-calls-displayed (make-hash-table :test 'equal))
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
            `(((info . ((role . "user")))
               (parts . (((type . "text") (text . ,table)))))
              ((info . ((role . "assistant")
                        (tokens . ((input . 0) (output . 0) (reasoning . 0)
                                   (cache . ((read . 0) (write . 0)))))))
               (parts . (((type . "text") (text . ,table))
                         ((type . "text") (text . ,fenced))))))))
      ;; Capture the request separately to verify it reaches the user
      ;; renderer without going through the table alignment path.
      (cl-letf (((symbol-function 'opencode--replay-user-request)
                 (lambda (message)
                   (opencode--output (alist-get 'text (car (alist-get 'parts message))))
                   (opencode--output "\n\n"))))
        (opencode--replay-session-messages messages))
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-prefix-p (concat table "\n\n") output))
        (should (string-match-p "|x +| +y|" output))
        (should (string-suffix-p (concat fenced "\n\n") output)))
      (should-not opencode--table-scan-marker)
      (should (= (process-mark process) (point-max)))
      (should-not markdown-ts-in-table-mode))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here

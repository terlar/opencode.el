;;; opencode-format-tool-calls.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing formatting opencode tool calls for display

;;; Code:

(require 'diff)
(require 'diff-mode)
(require 'opencode-common)

(defcustom opencode-tool-formatters nil
  "Alist mapping opencode tool names to formatter functions.

Each element has the form (TOOL-NAME . FUNCTION), where TOOL-NAME
is a string naming the tool and FUNCTION is called with the tool
arguments and should return a string.

`opencode-define-tool-formatter' can be used to register one"
  :type '(alist :key-type string :value-type function)
  :group 'opencode)

(defmacro opencode-define-tool-formatter (tool-name &rest body)
  "Register a formatter for TOOL-NAME.

BODY is evaluated inside `(let-alist tool-args ...)'."
  (declare (indent 1)
           (debug (form body)))
  (cl-with-gensyms (input-var)
    `(setf (alist-get ,tool-name opencode-tool-formatters nil nil #'equal)
           (lambda (,input-var)
             (let-alist ,input-var
               ,@body)))))

(opencode-define-tool-formatter "edit"
  (concat "✏ edit " .filePath ":\n"
          (opencode--fenced-block
           (opencode--format-edit-diff .oldString .newString) "diff")))

(opencode-define-tool-formatter "apply_patch"
  (concat "✏ apply_patch:\n"
          (opencode--fenced-block
           (opencode--format-apply-patch .patchText) "diff")))

(opencode-define-tool-formatter "write"
  (format "📝 write %s" .filePath))

(opencode-define-tool-formatter "read"
  (if (and .offset .limit)
      (format "📖 read `%s:%d` limit=%d"
              .filePath .offset .limit)
    (format "read %s" .filePath)))

(opencode-define-tool-formatter "grep"
  (concat
   (format "🔍 grep \"%s\"" .pattern)
   (when (or .include .path)
     (format " in %s" (or .include .path)))))

(opencode-define-tool-formatter "bash"
  (concat (when .description
            (format "# %s\n" .description))
          (format "$ %s" .command)))

(opencode-define-tool-formatter "websearch"
  (format "🌐 websearch \"%s\"" .query))

(opencode-define-tool-formatter "call_omo_agent"
  (format "🤖 call_omo_agent: %s\n\n%s" .description .prompt))

(opencode-define-tool-formatter "glob"
  (if .path
      (format "🔍 glob \"%s\" in %s"
              .pattern
              (opencode--relative-path-for-display .path))
    (format "🔍 glob \"%s\"" .pattern)))

(opencode-define-tool-formatter "todowrite"
  (opencode--format-todos .todos))

(opencode-define-tool-formatter "question"
  (opencode--format-questions .questions))

(opencode-define-tool-formatter "task"
  (if (string= "explore" .subagent_type)
      (format "🔍 Explore: %s" .description)
    (format "🤖 Subagent Task: %s" .description)))

(defun opencode--format-tool-call (tool input)
  "Format TOOL call with INPUT arguments for display."
  (when (alist-get 'filePath input)
    (setf (alist-get 'filePath input) (opencode--relative-path-for-display (alist-get 'filePath input))))

  (if-let* ((tool-formatter (cdr (assoc-string tool opencode-tool-formatters))))
      (funcall tool-formatter input)
    (if (= 1 (length input))
        (let ((arg (cdar input)))
          (format "%s%s%s"
                  tool
                  (if (string-match-p "\n" arg)
                      "\n"
                    " ")
                  arg))
      ;; Multiple arguments: tool-name, then arg-name: value per line
      (concat tool " ("
              (mapconcat (lambda (pair)
                           (format "%s=%s" (car pair) (cdr pair)))
                         input
                         ", ")
              ")"))))

(defun opencode--format-todos (todos)
  "Return TODOS as a markdown todo list."
  (mapconcat
   (lambda (todo)
     (let-alist todo
       (format "%s %s"
               (pcase .status
                 ("pending" "📌")
                 ("in_progress" "▶")
                 ("completed" "✅")
                 ("cancelled" "❌")
                 (_ " "))
               .content)))
   todos
   "\n"))

(defun opencode--format-edit-diff (old-string new-string)
  "Generate diff output comparing OLD-STRING to NEW-STRING."
  (with-temp-buffer
    (let ((old-buf (current-buffer)))
      (insert old-string)
      (insert "\n")
      (with-temp-buffer
        (let ((new-buf (current-buffer)))
          (insert new-string)
          (insert "\n")
          (with-temp-buffer
            (let ((inhibit-read-only t))
              ;; Run diff synchronously into this temp buffer
              (diff-no-select old-buf new-buf nil t (current-buffer))
              ;; Delete first 3 lines (diff command, ---, +++)
              (goto-char (point-min))
              (forward-line 3)
              (delete-region (point-min) (point))
              ;; Delete last 2 lines (diff finished timestamp)
              (goto-char (point-max))
              (forward-line -2)
              (delete-region (point) (point-max))
              ;; Return the diff content
              (buffer-string))))))))

(defun opencode--format-apply-patch (patch-text)
  "Return PATCH-TEXT formatted for `apply_patch' display."
  (with-temp-buffer
    (insert patch-text)
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*\\* \\(?:Begin\\|End\\) Patch\\n?" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at opencode--apply-patch-file-header-regexp)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (operation (match-string 1))
              (file (match-string 2)))
          (delete-region beg end)
          (goto-char beg)
          (insert (format "*** %s: %s"
                          operation
                          (opencode--relative-path-for-display file)))))
       ((looking-at "\\*\\*\\* Move to: \\(.*\\)$")
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (file (match-string 1)))
          (delete-region beg end)
          (goto-char beg)
          (insert (format "*** Move to: %s"
                          (opencode--relative-path-for-display file)))))
       ((looking-at "@@\\(.*\\)$")
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (suffix (match-string 1)))
          (unless (save-match-data
                    (string-match-p "\\` -[0-9,]+ \\+[0-9,]+ @@" suffix))
            (delete-region beg end)
            (goto-char beg)
            (insert (format "@@ -0 +0 @@%s" suffix))))))
      (forward-line 1))
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (delete-region (point-min) (point))
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (buffer-string)))

(provide 'opencode-format-tool-calls)
;;; opencode-format-tool-calls.el ends here

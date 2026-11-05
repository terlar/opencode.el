;;; opencode-common.el --- Common code shared through the package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Common code shared through the package

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'map)
(require 'notifications)
(require 'opencode-flycheck)
(require 'project)
(require 'savehist)

(defvar opencode-last-model nil
  "Last model used, as a Model.Ref alist of id, providerID and optional variant.
When nil or not available, connecting sets it to the server's default model.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar opencode-agents nil
  "List of available agents excluding hidden agents.")

(defvar opencode--slash-commands-by-directory nil
  "Alist mapping normalized project directories to available slash commands.")

(defvar opencode--location-by-directory nil
  "Alist mapping normalized directories to their opencode location.")

(defvar opencode-alerted-sessions nil
  "List of unvisited idle sessions.")

(defvar opencode-last-session-buffer nil
  "Last active opencode session buffer.")

(defvar opencode-models nil
  "List of enabled models.")

(defvar opencode--event-subscription nil
  "SSE event process for the global event stream.")

(defvar opencode--session-control-buffers nil
  "List of session control buffers.")

(defun opencode--normalize-directory (directory)
  "Return DIRECTORY as a canonical directory name."
  (file-name-as-directory (file-truename directory)))

(defun opencode--longest-backtick-run (text)
  "Return the length of the longest run of backticks in TEXT."
  (let ((start 0)
        (longest 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    longest))

(defun opencode--fenced-block (text &optional language)
  "Return TEXT in a Markdown fenced block labeled LANGUAGE."
  (let* ((text (or text ""))
         (fence (make-string (max 3 (1+ (opencode--longest-backtick-run text)))
                             ?`)))
    (concat fence (or language "") "\n"
            text
            (unless (string-suffix-p "\n" text) "\n")
            fence)))

(defcustom opencode-toast-function 'opencode--default-toast-show
  "Function to use to show toast notifications.
It receives a single argument, PROPERTIES, which is an alist with string keys
for title, message, variant (error/warning/info/success), and duration"
  :type 'function
  :group 'opencode)

(defcustom opencode-server-password nil
  "Password to connect to opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-file-edited-functions
  '(opencode--fix-parens
    opencode--reload-elisp-file)
  "Hook run after opencode edits a file.
This is an abnormal hook.  Each function receives two arguments, FILE and
RANGES, after a completed write, edit, or patch tool call.  FILE is the
edited file path.  RANGES is a list of (START-LINE . END-LINE) cons cells in
the post-edit file, or nil when the whole file was written.  For deletion-only
edits, END-LINE is nil."
  :type 'hook
  :group 'opencode)

(defcustom opencode-files-finished-editing-functions
  '(opencode--indent-files
    opencode--flycheck-edited-files)
  "Run hooks after opencode finishes editing files.
This is an abnormal hook.  Each entry receives one argument, FILES,
the list of file paths from completed write, edit, or patch tool calls.
Hooks run when the session becomes idle.  Hooks that finish
synchronously can just return.  Hooks that finish asynchronously must
return `:opencode-async' and call `opencode-hook-finished' when done.
While a hook is active, it may call `opencode-report-diagnostic' zero
or more times to queue diagnostic messages; all queued diagnostics are
sent to the session together after all hooks finish."
  :type 'hook
  :group 'opencode)

(defcustom opencode-project-files-finished-editing-functions
  nil
  "Like `opencode-files-finished-editing-functions', but per-project.
Designed to be set in `.dir-locals.el'"
  :local t
  :type 'hook
  :group 'opencode)

(defconst opencode--apply-patch-file-header-regexp
  "^\\*\\*\\* \\(Add File\\|Update File\\|Delete File\\): \\(.*\\)$"
  "Regexp matching file operation headers in patch tool input.")

(defun opencode--time-ago (opencode-object type)
  "Return .time.TYPE value from OPENCODE-OBJECT, as seconds ago.
Returns nil if the timestamp is not present."
  (when-let* ((timestamp (map-nested-elt opencode-object `(time ,type))))
    (- (float-time) (/ timestamp 1000))))

(defun opencode--format-time-ago (seconds)
  "Format SECONDS as a human-readable duration string.
Returns \"-\" if SECONDS is nil."
  (if seconds
      (seconds-to-string seconds)
    "-"))

(defun opencode--json-falsy (value)
  "Return t if VALUE is null, :null, :false, or 0."
  (cl-case value
    ((nil :null :false 0) t)
    (t nil)))

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

(cl-defun opencode--annotated-completion (prompt candidates)
  "Read a candidate with PROMPT and annotations.
CANDIDATES is a list of lists, where the first element is the string to show,
the second is the value to return, the third is the annotation to show, and
the optional fourth element is a number to sort by."
  (if candidates
      (let* ((max-length (seq-max
                          (mapcar (lambda (candidate)
                                    (length (car candidate)))
                                  candidates)))
             (candidate-results (make-hash-table :test 'equal))
             (has-sort-values nil)
             (candidates (cl-loop for (name return-value maybe-annotation sort-value) in candidates
                                  for annotation = (or maybe-annotation "")
                                  for candidate = (concat name
                                                          (propertize annotation 'invisible t))
                                  when sort-value do (setf has-sort-values t)
                                  do (puthash candidate return-value candidate-results)
                                  collect (propertize candidate
                                                      'opencode-annotation
                                                      (concat (make-string (+ 5 (- max-length
                                                                                   (length name)))
                                                                           ?\s)
                                                              annotation)
                                                      'opencode-sort-value sort-value)))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (candidate)
                   (get-text-property 0 'opencode-annotation candidate))
                :display-sort-function
                ,(when has-sort-values
                   (lambda (candidates)
                     (sort candidates
                           (lambda (a b)
                             (< (get-text-property 0 'opencode-sort-value a)
                                (get-text-property 0 'opencode-sort-value b)))))))))
        (gethash
         (completing-read
          prompt
          candidates
          nil t)
         candidate-results))
    (message "%sNo completion candidates" prompt)))

(defun opencode--format-questions (questions)
  "Format QUESTIONS showing ❓ prefix for each question."
  (string-join
   (cl-loop for q being the elements of questions
            for question-text = (alist-get 'question q)
            collect (concat "❓ " question-text "\n"))))

(defun opencode--relative-path-for-display (file)
  "Return FILE relative to project root when possible."
  (when file
    (let* ((project (project-current))
           (root (if project
                     (project-root project)
                   default-directory)))
      (if (and (file-name-absolute-p file)
               root
               (file-in-directory-p file root))
          (file-relative-name file root)
        file))))

(defun opencode--resolve-file-reference (file)
  "Return absolute path for FILE, or nil if no file exists."
  (let ((project (project-current)))
    (seq-find (lambda (candidate)
                (and candidate
                     (file-exists-p candidate)
                     (not (file-directory-p candidate))))
              (delete-dups
               (delq nil
                     (list (and (file-name-absolute-p file) file)
                           (when project
                             (expand-file-name file (project-root project)))
                           (expand-file-name file default-directory)))))))

(defun opencode--visit-file-location (location)
  "Open LOCATION in another window."
  (let ((file (car location))
        (line (cdr location)))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun opencode--buttonize-file-references (start end)
  "Turn backticked file references between START and END into buttons."
  (let ((inhibit-read-only t)
        (regexp
         (rx "`"
             (group (any alnum "._~/")
                    (* (any alnum "@%_./~+-")))
             (optional ":"
                       (group (+ digit))
                       (optional (any "-" "–") (+ digit)))
             "`")))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (let* ((match-beg (match-beginning 0))
               (match-end (match-end 0))
               (file (match-string-no-properties 1))
               (line-string (match-string-no-properties 2))
               (line (if line-string
                         (string-to-number line-string)
                       1))
               (path (opencode--resolve-file-reference file)))
          (when (and path (not (button-at match-beg)))
            (make-text-button
             match-beg match-end
             'action #'opencode--visit-file-location
             'button-data (cons path line)
             'follow-link t
             'help-echo (if line-string
                            (format "Open %s:%d in another window" file line)
                          (format "Open %s in another window" file))
             'mouse-face 'highlight)))))))

(defun opencode--default-toast-show (properties)
  "Default notifier to show notification with PROPERTIES from opencode."
  (let-alist properties
    (cond
     ((featurep 'dbusbind)
      (notifications-notify
       :title .title
       :body .message
       :urgency (pcase .variant
                  ((or "error" "warning" )'critical)
                  ((or "info" "success") 'normal))
       :timeout (or .duration 5000)
       :replaces-id 5647
       :app-icon 'none))
     ((eq system-type 'darwin)
      (let ((title (concat (pcase .variant
                             ("error" "❌")
                             ("warning" "⚠️")
                             ((or "info" "success") "ℹ️"))
                           " " .title)))
        (if (executable-find "terminal-notifier")
            (start-process "opencode-notification" nil "terminal-notifier"
                           "-title" title
                           "-message" .message
                           "-group" "opencode-toast"
                           "-activate" "org.gnu.Emacs"
                           "-sound" (pcase .variant
                                      ((or "error" "warning") "Basso")
                                      (_ "default")))
          (start-process "opencode-notification" nil "osascript" "-e"
                         (format "display notification %S with title %S"
                                 .message
                                 title))))))))

(defun opencode--toast-show (properties)
  "Show toast notification with PROPERTIES from opencode."
  (funcall opencode-toast-function properties))

(defun opencode--buffer-active-p (buffer)
  "Return non-nil if BUFFER is visible in the selected window of a focused frame."
  (and buffer
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state (window-frame (selected-window)))))

(defun opencode--reload-elisp-file (file &optional _ranges)
  "Reload elisp FILE after LLM edits."
  (when (string-suffix-p ".el" file)
    (condition-case err
        (load-file file)
      (error
       (message "Error reloading %s after LLM edit: %s" file err)))))

(defvar flycheck-checkers)

(defun opencode--fix-parens (file &optional _ranges)
  "Use parinfer to fix parens in FILE after LLM edits."
  (when (and (or (string-suffix-p ".el" file)
                 (string-suffix-p ".lisp" file))
             (require 'parinfer-rust-mode nil t))
    (declare-function parinfer-rust--set-default-state "parinfer-rust-mode")
    (declare-function parinfer-rust--execute "parinfer-rust-mode")
    (defvar parinfer-rust--mode)
    (with-current-buffer (find-file-noselect file)
      (revert-buffer t t t)
      (condition-case nil
          (check-parens)
        (error
         (let ((flycheck-checkers (delq 'parinfer-rust
                                        (and (boundp 'flycheck-checkers)
                                             flycheck-checkers))))
           (parinfer-rust--set-default-state)
           (let ((parinfer-rust--mode "indent"))
             (parinfer-rust--execute)))
         (save-buffer))))))

(defun opencode--indent-sensitive-buffer-p ()
  "Return non-nil if the current buffer is indentation-sensitive."
  (derived-mode-p '(python-base-mode
                    haskell-mode
                    haskell-ts-mode
                    fsharp-mode
                    fsharp-ts-mode
                    yaml-mode
                    yaml-ts-mode
                    nim-mode
                    nim-ts-mode)))

(provide 'opencode-common)
;;; opencode-common.el ends here

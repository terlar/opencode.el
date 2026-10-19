;;; opencode-sessions.el --- Convert opencode session to markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Convert opencode session to markdown

;;; Code:

(require 'json)
(require 'opencode-common)

(defun opencode--conversation-json (value)
  "Return VALUE encoded as pretty JSON."
  (let ((json-encoding-pretty-print t))
    (json-encode value)))

(defun opencode--conversation-part-type (part)
  "Return PART's type as a string."
  (let ((type (alist-get 'type part)))
    (cond
     ((stringp type) type)
     ((symbolp type) (symbol-name type)))))

(defun opencode--conversation-format-text-part (part)
  "Return Markdown for text PART."
  (let ((text (alist-get 'text part)))
    (when (and text (not (string-empty-p text)))
      (if (alist-get 'synthetic part)
          (concat "### Synthetic context\n\n"
                  (opencode--fenced-block text "text"))
        text))))

(defun opencode--conversation-format-reasoning-part (part)
  "Return Markdown for reasoning PART."
  (when-let ((text (alist-get 'text part)))
    (unless (string-empty-p text)
      (concat "### Reasoning\n\n"
              (opencode--fenced-block text "text")))))

(defun opencode--conversation-format-tool-part (part)
  "Return Markdown for tool call PART."
  (let-alist part
    (string-join
     (delq nil
           (list
            (format "### Tool call: %s" (or .tool "unknown"))
            (when .callID
              (format "Call ID: `%s`" .callID))
            (when .state.status
              (format "Status: `%s`" .state.status))
            (when .state.title
              (format "Title: %s" .state.title))
            (when .state.input
              (concat "Input:\n\n"
                      (opencode--fenced-block
                       (opencode--conversation-json .state.input)
                       "json")))
            (when .state.output
              (concat "Output:\n\n"
                      (opencode--fenced-block .state.output "text")))))
     "\n\n")))

(defun opencode--conversation-format-file-part (part)
  "Return Markdown for file context PART."
  (string-join
   (delq nil
         (list
          "### Context file"
          (when-let ((filename (alist-get 'filename part)))
            (format "- Filename: `%s`" filename))
          (when-let ((mime (alist-get 'mime part)))
            (format "- MIME: `%s`" mime))
          (when-let ((url (alist-get 'url part)))
            (format "- URL: `%s`" url))))
   "\n"))

(defun opencode--conversation-format-agent-part (part)
  "Return Markdown for agent context PART."
  (when-let ((name (alist-get 'name part)))
    (format "### Context agent\n\n@%s" name)))

(defun opencode--conversation-format-part (part)
  "Return Markdown for a supported conversation PART."
  (pcase (opencode--conversation-part-type part)
    ("text" (opencode--conversation-format-text-part part))
    ("reasoning" (opencode--conversation-format-reasoning-part part))
    ("tool" (opencode--conversation-format-tool-part part))
    ("file" (opencode--conversation-format-file-part part))
    ("agent" (opencode--conversation-format-agent-part part))))

(defun opencode--conversation-format-message (message)
  "Return Markdown for MESSAGE"
  (let* ((role (or (map-nested-elt message '(info role)) "message"))
         (role (if (stringp role) role (format "%s" role)))
         (title (format "## %s" (capitalize role)))
         (parts (mapcar #'opencode--conversation-format-part
                        (alist-get 'parts message))))
    (when-let ((parts (delq nil parts)))
      (string-join (cons title parts) "\n\n"))))

(defun opencode--conversation-to-markdown (messages)
  "Return MESSAGES as a Markdown conversation transcript."
  (let ((formatted-messages
         (delq nil
               (mapcar #'opencode--conversation-format-message messages))))
    (concat
     (string-join
      (list "# OpenCode Conversation"
            (if formatted-messages
                (string-join formatted-messages "\n\n")
              "_No supported message parts._"))
      "\n\n")
     "\n")))


(provide 'opencode-convert)
;;; opencode-convert.el ends here

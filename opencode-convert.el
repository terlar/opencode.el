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

(defun opencode--conversation-format-tool-item (item)
  "Return Markdown for tool call content ITEM."
  (let-alist item
    (let ((output (cl-loop for content in .state.content
                           when (equal "text" (alist-get 'type content))
                           collect (alist-get 'text content))))
      (string-join
       (delq nil
             (list
              (format "### Tool call: %s" (or .name "unknown"))
              (when .id
                (format "Call ID: `%s`" .id))
              (when .state.status
                (format "Status: `%s`" .state.status))
              (when .state.input
                (concat "Input:\n\n"
                        (if (stringp .state.input)
                            (opencode--fenced-block .state.input "text")
                          (opencode--fenced-block
                           (opencode--conversation-json .state.input)
                           "json"))))
              (when output
                (concat "Output:\n\n"
                        (opencode--fenced-block (string-join output "\n") "text")))
              (when .state.error.message
                (concat "Error:\n\n"
                        (opencode--fenced-block .state.error.message "text")))))
       "\n\n"))))

(defun opencode--conversation-message-parts (message)
  "Return the Markdown of the supported parts of MESSAGE."
  (let-alist message
    (pcase .type
      ("user"
       (append
        (list .text)
        (mapcar (lambda (file)
                  (let-alist file
                    (string-join
                     (delq nil
                           (list "### Context file"
                                 (when .name (format "- Filename: `%s`" .name))
                                 (when .mime (format "- MIME: `%s`" .mime))
                                 (when .source.uri (format "- URL: `%s`" .source.uri))))
                     "\n")))
                .files)
        (mapcar (lambda (agent)
                  (format "### Context agent\n\n@%s" (alist-get 'name agent)))
                .agents)))
      ("assistant"
       (mapcar (lambda (item)
                 (let-alist item
                   (pcase .type
                     ("tool" (opencode--conversation-format-tool-item item))
                     ((guard (or (null .text) (string-empty-p .text))) nil)
                     ("text" .text)
                     ("reasoning" (concat "### Reasoning\n\n"
                                          (opencode--fenced-block .text "text"))))))
               .content))
      ("shell"
       (list (concat "`$ " .command "`")
             (when .output.output
               (opencode--fenced-block .output.output "text"))))
      ("synthetic"
       (list (concat "### Synthetic context\n\n"
                     (opencode--fenced-block .text "text"))))
      ("compaction"
       (list .summary)))))

(defun opencode--conversation-to-markdown (messages)
  "Return MESSAGES as a Markdown conversation transcript."
  (let ((formatted-messages
         (cl-loop for message in messages
                  for parts = (delq nil (opencode--conversation-message-parts message))
                  when parts
                  collect (string-join
                           (cons (format "## %s" (capitalize (alist-get 'type message)))
                                 parts)
                           "\n\n"))))
    (format "# OpenCode Conversation\n\n%s\n"
            (if formatted-messages
                (string-join formatted-messages "\n\n")
              "_No supported message parts._"))))

(provide 'opencode-convert)
;;; opencode-convert.el ends here

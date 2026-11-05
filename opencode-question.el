;;; opencode-question.el --- Question selection UI for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: internal

;;; Commentary:

;; Provides a transient-based UI for answering questions from the opencode
;; server.  Supports single-select, multiple-select, and custom typed answers.
;;
;; Each option is a numbered key in the transient popup.  Press the key
;; to toggle the option.  Confirm with C-c C-c, reject with q.
;;
;; Questions arrive as forms of the question tool, and the transient
;; directly calls the server API on confirm or reject,
;; avoiding `recursive-edit' which conflicts with transient key handling.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)

(declare-function opencode--log-event "opencode")

;;; Per-question display state (global, since transient is not buffer-local)

(defvar opencode-question--marked nil
  "Vector of booleans tracking which options are marked.")

(defvar opencode-question--labels nil
  "Vector of label strings for the options.")

(defvar opencode-question--values nil
  "Vector of the values to answer with for the options.")

(defvar opencode-question--descriptions nil
  "Vector of description strings for the options.")

(defvar opencode-question--multiple nil
  "Non-nil if the current question allows multiple selections.")

(defvar opencode-question--custom-enabled nil
  "Non-nil if the current question allows a custom typed answer.")

(defvar opencode-question--custom-text nil
  "Custom text entered by the user, or nil if none.")

(defvar opencode-question--question-text nil
  "The full question text currently being displayed.")

;;; Batch state (tracks the full question sequence and accumulated answers)

(defvar opencode-question--session-id nil
  "The session ID of the form being answered.")

(defvar opencode-question--form-id nil
  "The form ID for replying or rejecting via the server API.")

(defvar opencode-question--questions nil
  "The full vector of question objects being asked.")

(defvar opencode-question--current-index 0
  "Index of the current question being displayed.")

(defvar opencode-question--answers nil
  "Accumulated list of answer lists, one per answered question.")

(defvar opencode-question--handled nil
  "Non-nil when confirm or reject has been processed for the current transient.")

;;; Helpers

(defun opencode-question--format-option (idx)
  "Format option IDX as a checkbox string with label and description."
  (let ((marked (aref opencode-question--marked idx))
        (label (aref opencode-question--labels idx))
        (desc (aref opencode-question--descriptions idx)))
    (concat (if marked "[x]" "[ ]") " " label
            (when desc
              (concat "  " (propertize desc 'face 'shadow))))))

(defun opencode-question--toggle-option (idx)
  "Toggle option at IDX, respecting single/multi-select mode."
  (if opencode-question--multiple
      (aset opencode-question--marked idx
            (not (aref opencode-question--marked idx)))
    ;; Single-select: radio button
    (let ((cur (aref opencode-question--marked idx)))
      (dotimes (i (length opencode-question--marked))
        (aset opencode-question--marked i nil))
      (unless cur
        (aset opencode-question--marked idx t)
        (setq opencode-question--custom-text nil)))))

(defun opencode-question--index-to-key (i)
  "Convert numeric index I to a transient key string."
  (cond ((< i 9) (string (+ ?1 i)))
        ((< i 35) (string (+ ?a (- i 9))))
        (t (format "M-%d" (- i 35)))))

(defun opencode-question--answer ()
  "Return the form answer for the accumulated answers.
A multiselect field takes an array, and a string field a scalar."
  (cl-loop for selected in (reverse opencode-question--answers)
           for question across opencode-question--questions
           collect (cons (intern (alist-get 'key question))
                         (if (alist-get 'multiple question)
                             (vconcat selected)
                           (car selected)))))

(defun opencode-question--reject ()
  "Cancel the form being answered."
  (opencode-api-cancel-form (opencode-question--session-id opencode-question--form-id)
      _result
    nil))

(defun opencode-question--collect ()
  "Collect currently selected labels and custom text into a list."
  (let ((selected (cl-loop for i below (length opencode-question--labels)
                           when (aref opencode-question--marked i)
                           collect (aref opencode-question--values i))))
    (when opencode-question--custom-text
      (setq selected (append selected (list opencode-question--custom-text))))
    selected))

;;; Custom suffix class for checkbox options

(defclass opencode-question-option (transient-suffix)
  ((option-index :initarg :option-index :initform 0)
   (transient :initform t))
  "A togglable checkbox option suffix for opencode questions.")

(cl-defmethod transient-format-description ((obj opencode-question-option))
  "Format OBJ as a checkbox line."
  (opencode-question--format-option (oref obj option-index)))

;;; Custom suffix class for typed answer

(defclass opencode-question-custom (transient-suffix)
  ((transient :initform t))
  "A custom text input option suffix for opencode questions.")

(cl-defmethod transient-format-description ((_obj opencode-question-custom))
  "Format the custom answer line with current text."
  (concat (if opencode-question--custom-text "[x]" "[ ]")
          " Type your own answer"
          (when opencode-question--custom-text
            (concat "  "
                    (propertize opencode-question--custom-text
                                'face 'font-lock-string-face)))))

;;; Suffix commands

(defun opencode-question--close-on-multiple ()
  "Exit transient in single-select, stay in multi-select."
  (if opencode-question--multiple
      (transient--do-stay)
    (transient--do-exit)))

(transient-define-suffix opencode-question--do-toggle ()
  "Toggle the checkbox option associated with this suffix."
  :class 'opencode-question-option
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (opencode-question--toggle-option
   (oref (transient-suffix-object) option-index))
  (unless opencode-question--multiple
    (opencode-question--do-confirm)))

(transient-define-suffix opencode-question--do-custom ()
  "Type a custom answer."
  :class 'opencode-question-custom
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (let ((text (read-string "Custom answer: "
                           opencode-question--custom-text)))
    (if (string-empty-p text)
        (setq opencode-question--custom-text nil)
      (setq opencode-question--custom-text text)
      (unless opencode-question--multiple
        ;; Single-select: clear all option marks
        (dotimes (i (length opencode-question--marked))
          (aset opencode-question--marked i nil))
        (opencode-question--do-confirm)))))

(defun opencode-question--confirm-pre-command ()
  "Pre-command function for confirm: exit if selection is non-empty, else stay."
  (if (opencode-question--collect)
      transient--exit
    transient--stay))

(transient-define-suffix opencode-question--do-confirm ()
  "Confirm the current selection and reply to the server."
  :transient #'opencode-question--confirm-pre-command
  (interactive)
  (let ((selected (opencode-question--collect)))
    (if (not selected)
        (message "No options selected")
      (setq opencode-question--handled t)
      (push selected opencode-question--answers)
      (if (< (1+ opencode-question--current-index)
             (length opencode-question--questions))
          ;; More questions remain; advance after transient fully exits.
          (progn
            (cl-incf opencode-question--current-index)
            (run-at-time 0 nil #'opencode-question--show-current))
        ;; All questions answered; send reply to server.
        (opencode-api-reply-form (opencode-question--session-id opencode-question--form-id)
            `((answer . ,(opencode-question--answer)))
            _result
          nil)))))

(transient-define-suffix opencode-question--do-reject ()
  "Reject the question and notify the server."
  :transient nil
  (interactive)
  (setq opencode-question--handled t)
  (opencode-question--reject))

;;; Transient exit hook

(defun opencode-question--on-exit ()
  "Handle transient exit by rejecting if not already confirmed or rejected."
  (remove-hook 'transient-exit-hook #'opencode-question--on-exit)
  (unless opencode-question--handled
    (opencode-question--reject)))

;;; Dynamic children setup

(defun opencode-question--setup-options (_children)
  "Generate checkbox suffix specs from current question state.
_CHILDREN is the statically-defined children, which are ignored."
  (transient-parse-suffixes
   'opencode-question--transient
   (append
    (cl-loop for i below (length opencode-question--labels)
             collect `(,(opencode-question--index-to-key i)
                       opencode-question--do-toggle
                       :option-index ,i))
    (when opencode-question--custom-enabled
      '(("!" opencode-question--do-custom))))))

;;; Transient prefix

(transient-define-prefix opencode-question--transient ()
  "Answer a question from the opencode server."
  [:class transient-column
   :description (lambda () (or opencode-question--question-text "Question"))
   :setup-children opencode-question--setup-options]
  [["Actions"
    ("C-c C-c" "Confirm" opencode-question--do-confirm :if (lambda () opencode-question--multiple))
    ("q"       "Reject"  opencode-question--do-reject)]])

;;; Entry points

(defun opencode-question--show-current ()
  "Set up per-question state and display transient for current question."
  (let* ((q (aref opencode-question--questions
                  opencode-question--current-index))
         (options (alist-get 'options q))
         (n (length options)))
    (setq opencode-question--labels (make-vector n nil)
          opencode-question--values (make-vector n nil)
          opencode-question--descriptions (make-vector n nil)
          opencode-question--marked (make-vector n nil)
          opencode-question--multiple (alist-get 'multiple q)
          opencode-question--custom-enabled (alist-get 'custom q)
          opencode-question--custom-text nil
          opencode-question--question-text (alist-get 'question q)
          opencode-question--handled nil)
    (dotimes (i n)
      (let ((opt (aref options i)))
        (aset opencode-question--labels i (alist-get 'label opt))
        (aset opencode-question--values i (alist-get 'value opt))
        (aset opencode-question--descriptions i (alist-get 'description opt))))
    (add-hook 'transient-exit-hook #'opencode-question--on-exit)
    (opencode-question--transient)))

(defun opencode--prompt-questions (pending)
  "Prompt user to answer the questions of PENDING, then reply or reject its form.
PENDING is a plist with :session-id, :form-id and :questions, a vector of
question objects with `key', `question', `header', `options', `multiple' and
`custom' fields.  Each option has `value', `label' and `description' fields.
Displays a transient popup for each question.
On confirm, replies to the server.  On reject or dismiss, rejects."
  (setq opencode-question--session-id (plist-get pending :session-id)
        opencode-question--form-id (plist-get pending :form-id)
        opencode-question--questions (plist-get pending :questions)
        opencode-question--current-index 0
        opencode-question--answers nil
        opencode-question--handled nil)
  (opencode-question--show-current))

(defun opencode--output-questions (buffer questions)
  "Output QUESTIONS to BUFFER with ❓ prefix for each question."
  (when (and (buffer-live-p buffer)
             (get-buffer-process buffer))
    (with-current-buffer buffer
      (opencode--maybe-insert-block-spacing)
      (opencode--output (opencode--format-questions questions))
      (opencode--output "\n"))))

(defun opencode--queue-questions (buffer pending)
  "Queue the questions PENDING in BUFFER and prompt if active."
  (let ((questions (plist-get pending :questions)))
    (opencode--output-questions buffer questions)
    (if (opencode--buffer-active-p buffer)
        (opencode--prompt-questions pending)
      (opencode--toast-show `((title . "OpenCode Questions")
                              (message . ,(alist-get 'question (aref questions 0)))
                              (variant . "info")))
      (with-current-buffer buffer
        (setq opencode-session-pending-questions pending)))))

(defun opencode--form-questions (form)
  "Return the vector of question objects for the fields of a question FORM."
  (vconcat
   (mapcar (lambda (field)
             (let-alist field
               `((key . ,.key)
                 (question . ,.description)
                 (header . ,.title)
                 (options . ,(vconcat .options))
                 (multiple . ,(equal .type "multiselect"))
                 (custom . ,(not (opencode--json-falsy .custom))))))
           (alist-get 'fields form))))

(defun opencode--cancel-unsupported-form (form)
  "Tell the user FORM can't be displayed, and cancel it so the session won't hang."
  (let-alist form
    (opencode--toast-show `((title . "OpenCode Form")
                            (message . ,(format "Can't display form: %s" .title))
                            (variant . "warning")))
    (opencode-api--request
     'delete (format "/api/session/%s/form/%s?message=%s" .sessionID .id
                     (url-hexify-string "opencode.el can't display this form"))
     :else (lambda (error-msg)
             (opencode--log-event "WARNING FORM CANCEL" error-msg)))))

(defun opencode--form-request (form)
  "Handle FORM, a Form.Info.
Ensures the session buffer exists so pending questions are not dropped,
which fetches and queues its pending forms once it's displayed."
  (let-alist form
    (if (not (equal .metadata.kind "question"))
        (opencode--cancel-unsupported-form form)
      (if-let* ((buffer (gethash .sessionID opencode-session-buffers))
                ((buffer-live-p buffer)))
          (unless (equal .id (plist-get (buffer-local-value
                                         'opencode-session-pending-questions buffer)
                                        :form-id))
            (opencode--queue-questions
             buffer
             (list :session-id .sessionID
                   :form-id .id
                   :questions (opencode--form-questions form))))
        (opencode-api-session (.sessionID)
            session
          (push session opencode-alerted-sessions)
          (opencode-open-session session))))))

(defun opencode--form-settled (session-id form-id)
  "Clear the pending form FORM-ID of SESSION-ID, which was answered or cancelled."
  (opencode--with-session-buffer session-id
    (when (equal form-id (plist-get opencode-session-pending-questions :form-id))
      (setq opencode-session-pending-questions nil))))

(provide 'opencode-question)
;;; opencode-question.el ends here

;;; opencode-flycheck.el --- Integration with flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Integration with flycheck - report warnings and errors to opencode

;;; Code:

(declare-function flycheck-buffer "flycheck")
(declare-function flycheck-mode "flycheck" (&optional arg))
(declare-function flycheck-running-p "flycheck")
(declare-function flycheck-stop "flycheck")
(declare-function flycheck-error-checker "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(declare-function flycheck-error-filename "flycheck" (err))
(declare-function flycheck-error-format "flycheck" (err &optional with-file-name))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function opencode-hook-finished "opencode" ())
(declare-function opencode-report-diagnostic "opencode" (message))

(defvar flycheck-current-errors)
(defvar flycheck-mode)
(defvar flycheck-status-changed-functions)

(defun opencode--flycheck-report-diagnostics (file base-directory errors)
  "Return formatted Flycheck ERRORS for FILE relative to BASE-DIRECTORY."
  (when errors
    (mapconcat (lambda (err)
                 (concat "- "
                         (file-relative-name
                          (or (flycheck-error-filename err) file)
                          base-directory)
                         ":"
                         (number-to-string (or (flycheck-error-line err) 1))
                         (when-let* ((column (flycheck-error-column err)))
                           (format ":%d" column))
                         ": "
                         (symbol-name (flycheck-error-level err))
                         ": "
                         (flycheck-error-format err nil)))
               errors
               "\n")))

(defun opencode--flycheck-fix-prompt (base-directory)
  "Return a prompt to fix the current buffer's Flycheck errors, or nil.
File names are shown relative to BASE-DIRECTORY."
  (when-let* ((report (opencode--flycheck-report-diagnostics
                       (buffer-file-name) base-directory flycheck-current-errors)))
    (concat "Fix the following:\n\n" report)))

(defun opencode--flycheck-report-status (file base-directory status)
  "Return a formatted Flycheck STATUS report for FILE.
BASE-DIRECTORY is used to shorten FILE in the report."
  (when (memq status '(errored interrupted suspicious))
    (format "- %s: warning: Flycheck ended with status `%s'"
            (file-relative-name file base-directory)
            status)))

(defun opencode--flycheck-edited-files (files)
  "Run Flycheck on FILES and report diagnostics for the active hook."
  (when (and files (require 'flycheck nil t))
    (let ((base-directory default-directory)
          (remaining (length files)))
      (cl-labels ((finish (report)
                    (when report
                      (opencode-report-diagnostic report))
                    (cl-decf remaining)
                    (when (zerop remaining)
                      (opencode-hook-finished))))
        (dolist (file files)
          (with-current-buffer (find-file-noselect file)
            (revert-buffer t t t)
            (let* ((flycheck-was-enabled flycheck-mode)
                   (completed nil)
                   after-hook
                   status-hook)
              (setq after-hook
                    (lambda ()
                      (unless completed
                        (setq completed t)
                        (remove-hook 'flycheck-after-syntax-check-hook after-hook t)
                        (remove-hook 'flycheck-status-changed-functions status-hook t)
                        (unless flycheck-was-enabled
                          (flycheck-mode -1))
                        (finish
                         (opencode--flycheck-report-diagnostics
                          file base-directory flycheck-current-errors)))))
              (setq status-hook
                    (lambda (status)
                      (when (and (not completed)
                                 (memq status '(no-checker errored interrupted suspicious)))
                        (setq completed t)
                        (remove-hook 'flycheck-after-syntax-check-hook after-hook t)
                        (remove-hook 'flycheck-status-changed-functions status-hook t)
                        (unless flycheck-was-enabled
                          (flycheck-mode -1))
                        (finish
                         (unless (eq status 'no-checker)
                           (opencode--flycheck-report-status
                            file base-directory status))))))
              (unless flycheck-mode
                (flycheck-mode 1))
              (when (flycheck-running-p)
                (flycheck-stop))
              (add-hook 'flycheck-after-syntax-check-hook after-hook nil t)
              (add-hook 'flycheck-status-changed-functions status-hook nil t)
              (flycheck-buffer)))))
      :opencode-async)))

(provide 'opencode-flycheck)
;;; opencode-flycheck.el ends here

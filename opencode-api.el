;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Wrapper for opencode api

;;; Code:

(require 'opencode-common)
(require 'plz)
(require 'json)
(require 'url-util)

(defvar opencode-api-url nil
  "URL of the opencode server we're connected to.")

(defvar opencode-api-log-max-lines nil
  "Maximum number of lines of opencode api requests and responses to log.
Or nil (default) to turn off logging.")

(defun opencode--auth-header ()
  "Return auth header based on `opencode-server-password'."
  (when opencode-server-password
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (format "opencode:%s" opencode-server-password) t)))))

(defun opencode-api--log (label value)
  "Log VALUE under LABEL to the api log, if logging is on."
  (when opencode-api-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-api-log*")
      (save-excursion
        (goto-char (point-max))
        (insert label ": ")
        (if (stringp value)
            (insert value "\n")
          (pp value (current-buffer)))
        (opencode--truncate-at-max-lines opencode-api-log-max-lines)))))

(defun opencode-api--error-message (path err)
  "Return a message for ERR, the `plz-error' from requesting PATH.
Shows the _tag and message of the server's JSON error body, if it has one."
  (let* ((response (plz-error-response err))
         (body (and response
                    (ignore-errors
                      (json-parse-string (plz-response-body response)
                                         :object-type 'alist))))
         (message (and (consp body) (alist-get 'message body))))
    (if message
        (format "error requesting %s: %s: %s" path (alist-get '_tag body) message)
      (format "error requesting %s: %s" path err))))

(cl-defun opencode-api--request (method path &key (body nil body-p) raw then else)
  "Request PATH with METHOD, sending BODY encoded as JSON if given.
Call THEN with the result in the buffer current now, unless it's been killed.
The result's {data} envelope is removed unless RAW is non-nil.
Call ELSE with the error message if given, in that buffer if it's still live,
otherwise signal it."
  (let ((buffer (current-buffer)))
    (opencode-api--log "REQUEST" path)
    (when body-p
      (opencode-api--log "REQUEST BODY" body))
    (apply
     #'plz method (concat opencode-api-url path)
     :as (lambda () (unless (string-empty-p (buffer-string))
                      (json-parse-buffer :array-type 'list
                                         :object-type 'alist)))
     :headers (delq nil
                    (list '("Content-Type" . "application/json")
                          (cons "x-opencode-directory"
                                (url-hexify-string
                                 (directory-file-name
                                  (expand-file-name default-directory))))
                          (opencode--auth-header)))
     :then (lambda (result)
             (opencode-api--log "RESPONSE" (or result "<empty>"))
             (let ((result (if-let* ((data (and (not raw)
                                                (consp result)
                                                (assq 'data result))))
                               (cdr data)
                             result)))
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when then
                       (funcall then result)))
                 (message "Buffer closed so opencode api response handler not run"))))
     :else (lambda (err)
             (let ((error-msg (opencode-api--error-message path err)))
               (opencode-api--log "ERROR" error-msg)
               (cond
                (else (if (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (funcall else error-msg))
                        (funcall else error-msg)))
                (opencode--event-subscription (error "%s" error-msg))
                (t (error "Not connected to opencode")))))
     (when body-p
       (list :body (json-encode body))))))

(defun opencode-api--all-messages (session-id callback)
  "Call CALLBACK with all of SESSION-ID's messages, oldest first.
The server returns them newest first, a page at a time."
  (let (messages)
    (cl-labels ((fetch (cursor)
                  (opencode-api--request
                   'get (format "/api/session/%s/message?limit=200%s"
                                session-id
                                (if cursor
                                    (concat "&cursor=" (url-hexify-string cursor))
                                  ""))
                   :raw t
                   :then (lambda (result)
                           (let ((page (alist-get 'data result))
                                 (next (map-nested-elt result '(cursor next))))
                             (setq messages (append (reverse page) messages))
                             (if (and page (not (opencode--json-falsy next)))
                                 (fetch next)
                               (funcall callback messages)))))))
      (fetch nil))))

(eval-and-compile
  (defun opencode-api--expand (method path args data return-var body)
    "Return a request with METHOD for PATH formatted with ARGS, sending DATA.
BODY is run with the result bound to RETURN-VAR.  If BODY starts with
:else HANDLER, HANDLER is called with the error message on failure."
    (let ((else (when (eq (car body) :else)
                  (prog1 (cadr body) (setq body (cddr body))))))
      `(opencode-api--request
        ',method ,(if args `(format ,path ,@args) path)
        ,@(when data `(:body ,data))
        :then (lambda (,return-var) ,@body)
        ,@(when else `(:else ,else))))))

(defmacro opencode-api--define-wrappers (&rest endpoints)
  "Define a macro for each of ENDPOINTS, which are ([METHOD] [nodata] NAME PATH).
METHOD defaults to get.  The macro opencode-api-NAME takes the ARGS to format
PATH with if it has any, the DATA to send if it's a post or patch that isn't
nodata, then a variable to bind the result to and a body to run.  The body
may start with :else HANDLER to handle errors."
  `(progn
     ,@(mapcar
        (lambda (endpoint)
          (let* ((method (if (memq (car endpoint) '(get patch post delete))
                             (pop endpoint)
                           'get))
                 (nodata (when (eq (car endpoint) 'nodata) (pop endpoint)))
                 (path (cadr endpoint))
                 (has-args (string-search "%" path))
                 (has-data (and (memq method '(post patch)) (not nodata))))
            `(defmacro ,(intern (format "opencode-api-%s" (car endpoint)))
                 (,@(when has-args '(args)) ,@(when has-data '(data))
                  return-var &rest body)
               ,(format "Wrapper for opencode %s endpoint." path)
               (declare (indent ,(cond ((and has-args has-data) 3)
                                       ((or has-args has-data) 2)
                                       (t 'defun))))
               (opencode-api--expand ',method ,path
                                     ,(when has-args 'args) ,(when has-data 'data)
                                     return-var body))))
        endpoints)))

(opencode-api--define-wrappers
 (info "/api/info")
 (location "/api/location")
 (projects "/api/project")
 (models "/api/model")
 (default-model "/api/model/default")
 (agents "/api/agent")
 (commands "/api/command")
 (mcps "/api/mcp")
 (post nodata enable-mcp "/api/experimental/mcp/%s/connect")
 (post nodata disable-mcp "/api/experimental/mcp/%s/disconnect")
 (session "/api/session/%s")
 (sessions-for-project "/api/session?project=%s&archived=false&limit=200")
 (root-sessions-for-project
   "/api/session?project=%s&archived=false&limit=200&parentID=null")
 (session-children "/api/session?parentID=%s")
 (sessions-active "/api/session/active")
 (post create-session "/api/session")
 (patch rename-session "/api/session/%s")
 (delete delete-session "/api/session/%s")
 (post fork-session "/api/session/%s/fork")
 (post compact-session "/api/session/%s/compact")
 (post nodata interrupt-session "/api/session/%s/interrupt")
 (post session-prompt "/api/session/%s/prompt")
 (post session-command "/api/session/%s/command")
 (post session-shell "/api/session/%s/shell")
 (post session-model "/api/session/%s/model")
 (post session-agent "/api/session/%s/agent")
 (post revert-message "/api/session/%s/revert/stage")
 (delete unrevert-all "/api/session/%s/revert")
 (session-permissions "/api/session/%s/permission")
 (post respond-permission-request "/api/session/%s/permission/%s/reply")
 (session-forms "/api/session/%s/form")
 (post reply-form "/api/session/%s/form/%s/reply")
 (delete cancel-form "/api/session/%s/form/%s")
 (delete cancel-form-with-message "/api/session/%s/form/%s?message=%s"))

(provide 'opencode-api)
;;; opencode-api.el ends here

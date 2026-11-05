# AGENTS.md - opencode.el

Emacs frontend for OpenCode LLM coding assistant server.

# Tool use

Interact with the user's emacs process to perform introspection, find
information about functions, values of variables etc. Use it to run and verify
that code you write is working correctly.

## Verification Commands

DO NOT byte-compile to check for warnings. After you are finished you will be
presented all warnings and errors from emacs' flycheck, including byte compiler
warnings.
DO NOT run all tests, they will also be automatically run after you are finished
and you will be notified if any failed.

DO use emacs_eval_elisp to perform runtime verification that the code you edited
works as expected.

### Async HTTP Pattern

This codebase uses macro-generated async API calls:

```elisp
;; The macro opencode-api-session expands to async plz call
(opencode-api-session (session-id)
    result-var
  ;; This body runs in callback with result-var bound
  (do-something-with result-var))
```

### Logging

- Log warnings to dedicated buffer: `*opencode-event-log*`
  use `(opencode--log-event "WARNING TYPE" "msg")`

### Opencode 

The OpenAPI spec for the OpenCode server is in opencode-openapi.json.

The source code to OpenCode itself is in ~/opencode, if you need to look at it's
source code to find how it does something, use the explore subagent.

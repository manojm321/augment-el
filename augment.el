;;; augment.el --- Emacs integration for Auggie CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Augment Integration
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: ai, augment, cli, org-mode
;; URL: https://github.com/augment-code/augment-el

;;; Commentary:

;; This package provides an Emacs integration for the Auggie CLI,
;; allowing users to interact with Augment Agent directly from within Emacs.
;; Features include:
;; - Session management and selection
;; - Org-mode formatted conversations
;; - Interactive shell-like interface
;; - Full org-mode integration (folding, export, etc.)

;;; Code:

(require 'org)
(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup augment nil
  "Emacs integration for Auggie CLI."
  :group 'external
  :prefix "augment-")

(defcustom augment-buffer-name-max-length 50
  "Maximum length for augment buffer names."
  :type 'integer
  :group 'augment)

(defcustom augment-auto-scroll t
  "Whether to auto-scroll to bottom after responses."
  :type 'boolean
  :group 'augment)



(defcustom augment-new-session-message "You are responding within Emacs using the Augment extension. The user is interacting with you through an Emacs org-mode buffer.

IMPORTANT: Always format your responses using org-mode syntax for proper display in Emacs:

** Formatting Requirements:

1. Use proper headlines (*, **, ***) for structure and organization
2. Format code blocks with #+BEGIN_SRC language and #+END_SRC
3. Use emphasis markup: *bold*, /italic/, =code=, ~verbatim~
4. Create lists with - or + for unordered lists, 1. for ordered lists
5. Use #+BEGIN_QUOTE and #+END_QUOTE for quotes
6. Format tables using org-table syntax with | separators
7. Include links as [[url][description]] or [[file:path][description]]
8. Use #+BEGIN_EXAMPLE and #+END_EXAMPLE for literal text blocks
9. Structure your response with clear headlines and subheadings
10. Never use markdown syntax - always use org-mode equivalents

** Examples of Correct Formatting:

- Code blocks: #+BEGIN_SRC python instead of ```python
- Bold text: *bold* instead of **bold**
- Italic text: /italic/ instead of _italic_
- Inline code: =code= instead of `code`
- Headers: * Header instead of # Header
- Subheaders: ** Subheader instead of ## Subheader

** Context:
- You are running inside Emacs via the augment.el package
- The user can see your response in real-time as you type
- Your response will be displayed in an org-mode buffer with proper syntax highlighting
- The user can fold/unfold sections, navigate headlines, and export your response

Always ensure your response is valid org-mode syntax that renders beautifully in Emacs org-mode."
  "Initial message sent when creating a new augment session.
This message is sent with the -q flag to establish the session and set up
org-mode formatting preferences."
  :type 'string
  :group 'augment)

(defcustom augment-enable-debug nil
  "Whether to enable debug logging of auggie commands."
  :type 'boolean
  :group 'augment)

;;; Variables

(defvar-local augment-session-id nil
  "Current session ID for this buffer.")

(defvar-local augment-session-data nil
  "Full session metadata for this buffer.")



(defvar-local augment-input-start nil
  "Marker for start of input area.")



(defvar-local augment-current-process nil
  "Current running auggie process for this buffer, if any.")

(defvar-local augment-input-history nil
  "List of previous user inputs for this session.")

(defvar-local augment-input-history-index nil
  "Current index in input history, nil means not navigating history.")

;;; Hooks

(defvar augment-mode-hook nil
  "Hook run when entering augment-mode.")

(defvar augment-response-hook nil
  "Hook run after displaying response.")

(defvar augment-send-hook nil
  "Hook run before sending message.")



;;; Utility Functions

(defun augment-format-timestamp (iso-date)
  "Format ISO-DATE timestamp for display."
  (if iso-date
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (date-to-time iso-date))
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun augment-truncate-string (string max-length)
  "Truncate STRING to MAX-LENGTH, adding ellipsis if needed."
  (if (> (length string) max-length)
      (concat (substring string 0 (- max-length 3)) "...")
    string))

(defun augment-check-auggie-available ()
  "Check if auggie CLI is available."
  (executable-find "auggie"))

(defun augment-validate-session-id (session-id)
  "Validate SESSION-ID format."
  (and session-id
       (stringp session-id)
       (string-match-p "^[0-9a-f-]+$" session-id)))

(defun augment-cleanup-response (response)
  "Clean up RESPONSE text if needed."
  (when response
    (string-trim response)))

(defun augment-debug-log (message &optional session-id)
  "Log MESSAGE to debug buffer with optional SESSION-ID prefix."
  (when augment-enable-debug
    (let ((debug-buffer (get-buffer-create "*Augment Debug*"))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
          (prefix (if session-id
                     (format "[%s] " (substring session-id 0 8))
                   "")))
      (with-current-buffer debug-buffer
        (goto-char (point-max))
        (insert (format "%s %s%s\n" timestamp prefix message))))))

(defun augment-handle-error (error-msg)
  "Handle error with ERROR-MSG, preserving user input."
  (augment-debug-log (format "ERROR: %s" error-msg) augment-session-id)
  (message "Augment error: %s" error-msg)
  (insert (format "Error: %s" error-msg)))

;;; Input History

(defun augment-load-input-history ()
  "Load input history from session data."
  (when augment-session-id
    (let ((session-data (augment-get-session-data augment-session-id)))
      (when session-data
        (let ((user-messages (alist-get 'userMessages session-data)))
          ;; Convert vector to list if needed
          (setq augment-input-history (if (vectorp user-messages)
                                          (append user-messages nil)
                                        user-messages))
          (setq augment-input-history-index nil))))))

(defun augment-history-previous ()
  "Navigate to previous input in history."
  (interactive)
  (unless (eq major-mode 'augment-mode)
    (error "Not in an augment buffer"))

  ;; Load history if not already loaded
  (unless augment-input-history
    (augment-load-input-history))

  (when augment-input-history
    (let ((current-input (augment-get-current-input)))
      ;; If we're not navigating history yet, start from the end
      (when (null augment-input-history-index)
        (setq augment-input-history-index (length augment-input-history)))

      ;; Move to previous item if possible
      (when (> augment-input-history-index 0)
        (setq augment-input-history-index (1- augment-input-history-index))
        (let ((history-item (nth augment-input-history-index augment-input-history)))
          (augment-replace-current-input history-item))))))

(defun augment-history-next ()
  "Navigate to next input in history."
  (interactive)
  (unless (eq major-mode 'augment-mode)
    (error "Not in an augment buffer"))

  (when (and augment-input-history augment-input-history-index)
    ;; Move to next item if possible
    (if (< augment-input-history-index (1- (length augment-input-history)))
        (progn
          (setq augment-input-history-index (1+ augment-input-history-index))
          (let ((history-item (nth augment-input-history-index augment-input-history)))
            (augment-replace-current-input history-item)))
      ;; If at the end, clear input and reset index
      (setq augment-input-history-index nil)
      (augment-replace-current-input ""))))

(defun augment-replace-current-input (new-input)
  "Replace current input with NEW-INPUT."
  (when augment-input-start
    (save-excursion
      (goto-char augment-input-start)
      (delete-region (point) (point-max))
      (insert new-input))))

;;; Session Management

(defun augment-get-session-data (session-id)
  "Get detailed session data for SESSION-ID from auggie."
  (unless (augment-check-auggie-available)
    (error "Auggie CLI not found. Please install auggie first"))

  (let ((sessions (augment-list-sessions)))
    (seq-find (lambda (session)
                (string= (alist-get 'sessionId session) session-id))
              sessions)))

(defun augment-get-current-exchange-count ()
  "Get the current exchange count from the session data."
  (if augment-session-id
      (let ((session-data (augment-get-session-data augment-session-id)))
        (if session-data
            (alist-get 'exchangeCount session-data)
          0))
    0))

(defun augment-list-sessions ()
  "Retrieve list of auggie sessions."
  (unless (augment-check-auggie-available)
    (error "Auggie CLI not found. Please install auggie first"))

  (augment-debug-log "COMMAND: auggie session list --json")
  (let ((output (shell-command-to-string "auggie session list --json")))
    (augment-debug-log (format "SESSION LIST OUTPUT: %s" (if output (substring output 0 (min 500 (length output))) "nil")))
    (condition-case err
        (let ((sessions (json-parse-string output :object-type 'alist :array-type 'list)))
          (if (listp sessions)
              (progn
                (augment-debug-log (format "PARSED %d sessions" (length sessions)))
                sessions)
            (error "Invalid session list format")))
      (error
       (augment-debug-log (format "SESSION LIST ERROR: %s" (error-message-string err)))
       (message "Failed to parse session list: %s" (error-message-string err))
       nil))))

(defun augment-format-session-for-completion (session)
  "Format SESSION data for completion display."
  (let* ((session-id (alist-get 'sessionId session))
         (first-message (alist-get 'firstUserMessage session))
         (truncated-message (augment-truncate-string first-message 50))
         (short-id (if (and session-id (>= (length session-id) 8))
                       (substring session-id 0 8)
                     (or session-id "unknown"))))
    (format "%s: %s"
            short-id
            truncated-message)))

(defun augment-completing-read (prompt choices)
  "Use Helm if available, otherwise fall back to completing-read."
  (if (and (fboundp 'helm-comp-read) (boundp 'helm-mode) helm-mode)
      ;; Helm returns the key (string), not the value
      (helm-comp-read prompt (mapcar #'car choices) :must-match nil)
    (completing-read prompt choices nil nil)))

(defun augment-select-session ()
  "Present completion interface for session selection."
  (let* ((sessions (augment-list-sessions))
         (session-choices (mapcar (lambda (session)
                                   (cons (augment-format-session-for-completion session)
                                         session))
                                 sessions))
         ;; Add "New Session" option at the top
         (all-choices (cons '("🆕 New Session" . :new-session) session-choices))
         (choice (augment-completing-read "Select session: " all-choices)))
    (cond
     ((string-empty-p choice) nil)
     ((string= choice "🆕 New Session") :new-session)
     (t (cdr (assoc choice all-choices))))))

;;; Buffer Management

(defun augment-rename-buffer (new-name)
  "Rename the current augment buffer to NEW-NAME.
The new name will be formatted as *Augment SESSION-ID: NEW-NAME*."
  (interactive "sNew buffer name: ")
  (unless (eq major-mode 'augment-mode)
    (error "Not in an augment buffer"))
  (unless augment-session-id
    (error "No session ID found in current buffer"))

  (let* ((short-id (if (>= (length augment-session-id) 8)
                       (substring augment-session-id 0 8)
                     augment-session-id))
         (formatted-name (format "*Augment %s: %s*" short-id new-name)))
    (rename-buffer formatted-name t)
    (message "Buffer renamed to: %s" formatted-name)))

(defun augment-create-buffer-name (first-message)
  "Create buffer name from FIRST-MESSAGE, including short session ID if available."
  (let* ((truncated (augment-truncate-string first-message
                                            augment-buffer-name-max-length))
         (short-id (if (and augment-session-id (>= (length augment-session-id) 8))
                       (substring augment-session-id 0 8)
                     nil)))
    (if short-id
        (format "*Augment %s: %s*" short-id truncated)
      (format "*Augment: %s*" truncated))))

(defun augment-insert-header (session-data)
  "Insert org-mode header with SESSION-DATA metadata."
  (let* ((first-message (alist-get 'firstUserMessage session-data))
         (session-id (alist-get 'sessionId session-data))
         (exchange-count (alist-get 'exchangeCount session-data))
         (created (alist-get 'created session-data))
         (modified (alist-get 'modified session-data))
         (workspace (alist-get 'workspaceRoot session-data)))
    (insert (format "#+TITLE: Augment Session: %s\n" first-message))
    (insert (format "#+SESSION_ID: %s\n" session-id))
    (insert (format "#+EXCHANGES: %d\n" exchange-count))
    (insert (format "#+CREATED: %s\n" created))
    (insert (format "#+MODIFIED: %s\n" modified))
    (insert (format "#+WORKSPACE: %s\n\n" workspace))))

(defun augment-setup-structure ()
  "Create initial org-mode structure with simplified format."
  (let ((current-exchange (augment-get-current-exchange-count)))
    (insert (format "* [%d] [%s]\n> "
                    current-exchange
                    (format-time-string "%Y-%m-%d %H:%M:%S")))
    (setq augment-input-start (point-marker))
    (set-marker-insertion-type augment-input-start nil)))

(defun augment-create-shell-buffer (session-id first-message)
  "Create new augment buffer with SESSION-ID and FIRST-MESSAGE."
  (let* ((buffer-name (augment-create-buffer-name first-message))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (augment-mode)
      (setq augment-session-id session-id)
      (erase-buffer)
      ;; Will be populated when session data is available
      )
    buffer))

(defun augment-setup-buffer (session-data)
  "Initialize buffer with SESSION-DATA and org-mode structure."
  (setq augment-session-data session-data)
  (erase-buffer)
  (augment-insert-header session-data)
  (augment-setup-structure)
  (augment-load-input-history)
  (run-hooks 'augment-setup-hook)
  (goto-char (point-max)))

;;; Major Mode

(defvar augment-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Set up parent keymap relationship properly
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "RET") 'augment-send-input)
    (define-key map (kbd "C-c C-i") 'augment-clear-input)
    (define-key map (kbd "C-c C-c") 'augment-interrupt-response)
    (define-key map (kbd "C-c C-d") 'augment-show-debug-buffer)
    (define-key map (kbd "C-c C-l") 'augment-clear-history)
    (define-key map (kbd "C-c C-q") 'augment-quit)
    (define-key map (kbd "C-c C-r") 'augment-rename-buffer)
    (define-key map (kbd "M-p") 'augment-history-previous)
    (define-key map (kbd "M-n") 'augment-history-next)
    map)
  "Keymap for augment-mode.")

(define-derived-mode augment-mode org-mode "Augment"
  "Major mode for Augment shell interaction.

Key bindings:
\\[augment-send-input] - Send input to auggie
\\[augment-clear-input] - Clear current input
\\[augment-interrupt-response] - Interrupt current response
\\[augment-show-debug-buffer] - Show debug buffer
\\[augment-clear-history] - Clear session history
\\[augment-quit] - Quit augment session
\\[augment-rename-buffer] - Rename buffer
\\[augment-history-previous] - Previous input in history
\\[augment-history-next] - Next input in history

\\{augment-mode-map}"
  (setq-local org-startup-folded nil)
  (setq-local org-startup-truncated nil)
  (run-hooks 'augment-mode-hook))

;;; Interactive Functions

(defun augment-get-current-input ()
  "Get current user input from buffer."
  (when augment-input-start
    (let* ((input-end (point-max))
           (input-text (buffer-substring-no-properties augment-input-start input-end)))
      ;; The input starts right after the marker, no need to parse prompt
      (string-trim input-text))))

(defun augment-clear-input ()
  "Clear current input area."
  (interactive)
  (when augment-input-start
    (delete-region augment-input-start (point-max))
    (goto-char augment-input-start)))

(defun augment-clear-history ()
  "Clear conversation history but keep session metadata."
  (interactive)
  (when (yes-or-no-p "Clear conversation history? ")
    (save-excursion
      (goto-char (point-min))
      ;; Find the first exchange headline (starts with "* [")
      (when (re-search-forward "^\\* \\[" nil t)
        (let ((start (line-beginning-position)))
          ;; Delete everything from first exchange to end of buffer
          (delete-region start (point-max))
          ;; Prepare for first input (exchange count will be read from JSON)
          (let ((current-exchange (augment-get-current-exchange-count)))
            (insert (format "* [%d] [" current-exchange))
            (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
            (insert "]\n> "))
          (setq augment-input-start (point-marker))
          (set-marker-insertion-type augment-input-start nil))))
    (goto-char (point-max))))

(defun augment-interrupt-response ()
  "Interrupt the current auggie response."
  (interactive)
  (if augment-current-process
      (progn
        (augment-debug-log "INTERRUPTING PROCESS" augment-session-id)
        (when (process-live-p augment-current-process)
          (kill-process augment-current-process))
        (setq augment-current-process nil)
        (message "Response interrupted")
        ;; Clean up any partial response and prepare for next input
        (augment-prepare-next-input))
    (message "No response in progress")))

(defun augment-quit ()
  "Quit and close augment buffer."
  (interactive)
  (when (yes-or-no-p "Close augment session? ")
    ;; Kill any running process first
    (when (and augment-current-process (process-live-p augment-current-process))
      (kill-process augment-current-process))
    (kill-buffer)))

(defun augment-send-input ()
  "Send current input to auggie."
  (interactive)
  (let ((input (augment-get-current-input)))
    (when (and input (not (string-match-p "^\\s-*$" input)))
      (let ((trimmed-input (string-trim input)))
        (run-hooks 'augment-send-hook)
        (augment-send-message trimmed-input)))))

;;; Communication

(defun augment-strip-ansi-codes (text)
  "Remove ANSI escape sequences from TEXT."
  (when text
    (let ((ansi-regex "\033\\[[0-9;]*[a-zA-Z]"))
      (replace-regexp-in-string ansi-regex "" text))))

(defun augment-build-command (message &optional use-quick-prompt)
  "Build auggie command with MESSAGE. Use -q flag if USE-QUICK-PROMPT is non-nil."
  (let ((base-command (if augment-session-id
                          (format "NO_COLOR=1 auggie --print --resume %s"
                                  (shell-quote-argument augment-session-id))
                        "NO_COLOR=1 auggie --print")))
    (format "%s%s %s"
            base-command
            (if use-quick-prompt " -q" "")
            (shell-quote-argument message))))

(defun augment-send-message (message &optional use-quick-prompt)
  "Send MESSAGE to auggie CLI. Use -q flag if USE-QUICK-PROMPT is non-nil."
  (unless (augment-check-auggie-available)
    (augment-handle-error "Auggie CLI not found")
    (return))

  (when (and augment-session-id
             (not (augment-validate-session-id augment-session-id)))
    (augment-handle-error "Invalid session ID")
    (return))

  (let ((command (augment-build-command message use-quick-prompt)))

    ;; Debug log the command
    (augment-debug-log (format "COMMAND: %s" command) augment-session-id)
    (augment-debug-log (format "INPUT: %s" message) augment-session-id)

    ;; Clear input and show we're processing
    (insert "\nProcessing...")
    (redisplay)

    ;; Execute command with streaming
    (augment-start-streaming-command command message)))

(defun augment-start-streaming-command (command message)
  "Start streaming COMMAND execution and display output for MESSAGE."
  (let* ((process-name "augment-process")
         (buffer (current-buffer))
         (output-buffer "")
         (process (start-process-shell-command process-name nil command)))

    ;; Track the current process for interruption
    (setq augment-current-process process)
    (augment-debug-log (format "STARTED PROCESS: %s" command) augment-session-id)

    ;; Set up process filter for streaming output
    (set-process-filter
     process
     (lambda (proc string)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq output-buffer (concat output-buffer string))

           ;; Remove "Processing..." if still there
           (when (and augment-input-start
                      (save-excursion
                        (goto-char augment-input-start)
                        (looking-at "Processing\\.\\.\\.")))
             (augment-clear-input))

           ;; Display the accumulated output so far
           (augment-display-streaming-response string)))))

    ;; Set up process sentinel for completion
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           ;; Clear the process reference
           (setq augment-current-process nil)
           (cond
            ((string-match "finished" event)
             (augment-debug-log "PROCESS COMPLETED" augment-session-id)

             ;; Handle session creation if this was a new session
             (when (and (boundp 'augment-session-creation-callback)
                       augment-session-creation-callback
                       (string= augment-session-id "creating..."))
               (let ((sessions (augment-list-sessions)))
                 (when sessions
                   (let* ((most-recent-session (car (last sessions)))
                          (real-session-id (alist-get 'sessionId most-recent-session)))
                     (setq augment-session-id real-session-id)
                     (funcall augment-session-creation-callback real-session-id)
                     (setq augment-session-creation-callback nil)))))

             (augment-finalize-exchange message output-buffer))
            ((string-match "\\(exited\\|failed\\|killed\\|interrupt\\)" event)
             (augment-debug-log (format "PROCESS ENDED: %s" event) augment-session-id)
             (if (string-match "\\(killed\\|interrupt\\)" event)
                 (message "Response interrupted")
               (augment-handle-error "Command failed"))))))))))

(defun augment-display-streaming-response (new-text)
  "Display NEW-TEXT as streaming response."
  ;; Strip ANSI codes and append to the current response area
  (let ((clean-text (augment-strip-ansi-codes new-text)))
    (save-excursion
      (goto-char (point-max))
      (insert clean-text))

    ;; Auto-scroll if enabled
    (when augment-auto-scroll
      (goto-char (point-max)))))

(defun augment-finalize-exchange (message full-output)
  "Finalize exchange with MESSAGE and FULL-OUTPUT."
  (if (or (not full-output) (string= (string-trim full-output) ""))
      (augment-handle-error "No response from auggie")

    ;; Clear any partial streaming output and display properly formatted exchange
    (let ((cleaned-output (augment-cleanup-response full-output)))
      (augment-display-exchange message cleaned-output)

      ;; Prepare for next input
      (augment-prepare-next-input))))

(defun augment-display-exchange (user-message assistant-response)
  "Display USER-MESSAGE and ASSISTANT-RESPONSE in org-mode structure."
  (save-excursion
    ;; Go to the current input area and replace it with the completed exchange
    (when augment-input-start
      (goto-char augment-input-start)
      ;; Move to beginning of the current headline
      (beginning-of-line)
      (when (looking-at "^\\* \\[")
        ;; Replace the current input line with the user message
        (let ((start (point)))
          (forward-line 1)
          (when (looking-at "^> ")
            (forward-line 1)
            ;; Delete any existing input after the prompt
            (delete-region (point) augment-input-start)
            ;; Insert the user message
            (insert user-message "\n\n")
            ;; Insert the assistant response
            (insert assistant-response)
            (insert "\n\n"))))))

  (run-hooks 'augment-response-hook)
  (when augment-auto-scroll
    (goto-char (point-max))))

(defun augment-prepare-next-input ()
  "Prepare input area for next message."
  (goto-char (point-max))
  (let ((current-exchange (augment-get-current-exchange-count)))
    (insert (format "\n* [%d] [%s]\n> "
                    current-exchange
                    (format-time-string "%Y-%m-%d %H:%M:%S")))
    (setq augment-input-start (point-marker))
    (set-marker-insertion-type augment-input-start nil)))

;;; Session Management Utilities

(defun augment-get-session-buffer (session-id)
  "Get buffer for SESSION-ID if it exists."
  (cl-find-if (lambda (buffer)
                (with-current-buffer buffer
                  (and (eq major-mode 'augment-mode)
                       (string= augment-session-id session-id))))
              (buffer-list)))



(defun augment-list-active-sessions ()
  "List all active augment session buffers."
  (cl-remove-if-not (lambda (buffer)
                      (with-current-buffer buffer
                        (eq major-mode 'augment-mode)))
                    (buffer-list)))



;;; Buffer Cleanup

(defun augment-cleanup-markers ()
  "Clean up markers and processes when buffer is killed."
  (when augment-input-start
    (set-marker augment-input-start nil))

  ;; Kill any running process
  (when (and augment-current-process (process-live-p augment-current-process))
    (kill-process augment-current-process)
    (setq augment-current-process nil)))

(add-hook 'kill-buffer-hook #'augment-cleanup-markers)

(defun augment-show-debug-buffer ()
  "Show the augment debug buffer."
  (interactive)
  (let ((debug-buffer (get-buffer "*Augment Debug*")))
    (if debug-buffer
        (progn
          (switch-to-buffer-other-window debug-buffer)
          (goto-char (point-max))
          (message "Debug buffer shown. Enable debug logging with: (setq augment-enable-debug t)"))
      (message "No debug buffer found. Enable debug logging with: (setq augment-enable-debug t)"))))

;;; Main Entry Point

(defun augment-create-new-session ()
  "Create a new augment session with immediate buffer creation."
  (let ((initial-message augment-new-session-message))

    ;; Create buffer immediately with temporary name
    (let* ((temp-name "*Augment: Creating Session...*")
           (buffer (get-buffer-create temp-name)))

      (with-current-buffer buffer
        (augment-mode)
        (erase-buffer)
        (insert "#+TITLE: Creating Augment Session...\n")
        (insert "#+SESSION_ID: pending\n\n")
        (insert "* [1] [")
        (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
        (insert "]\n> ")
        (insert initial-message)
        (insert "\n\n⏳ Creating session...")
        (setq augment-input-start (point-marker))
        (set-marker-insertion-type augment-input-start nil)
        (setq augment-session-id "creating...")

        ;; Set up session creation callback
        (setq-local augment-session-creation-callback
                    (lambda (session-id)
                      ;; Update buffer properties
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "#\\+TITLE: Creating Augment Session\\.\\.\\." nil t)
                          (replace-match (format "#+TITLE: Augment Session %s" (substring session-id 0 8))))
                        (goto-char (point-min))
                        (when (re-search-forward "#\\+SESSION_ID: pending" nil t)
                          (replace-match (format "#+SESSION_ID: %s" session-id))))

                      ;; Rename buffer
                      (let ((new-name (augment-create-buffer-name initial-message)))
                        (rename-buffer new-name t))

                      ;; Load input history
                      (augment-load-input-history)

                      (message "Session created: %s" (substring session-id 0 8)))))

      ;; Switch to buffer immediately
      (switch-to-buffer buffer)

      ;; Use existing streaming infrastructure
      (let ((command (format "NO_COLOR=1 auggie --print -q %s"
                            (shell-quote-argument initial-message))))
        (augment-start-streaming-command command initial-message)))))

;;;###autoload
(defun augment ()
  "Launch Augment shell interface."
  (interactive)
  (let ((session (augment-select-session)))
    (cond
     ;; User selected "New Session" or no selection (empty)
     ((or (eq session :new-session) (null session))
      (augment-create-new-session))
     ;; Resume existing session
     (t
      (let* ((session-id (alist-get 'sessionId session))
             (first-message (alist-get 'firstUserMessage session))
             (buffer (augment-create-shell-buffer session-id first-message)))
        (with-current-buffer buffer
          (augment-setup-buffer session))
        (switch-to-buffer buffer))))))

(provide 'augment)

;;; augment.el ends here

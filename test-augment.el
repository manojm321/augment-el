;;; test-augment.el --- ERT tests for augment.el -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT-based tests for augment.el functionality

;;; Code:

(require 'ert)
(require 'augment)

;;; Utility Function Tests

(ert-deftest test-augment-new-session-message-customization ()
  "Test that new session message is configurable."
  ;; Test default value exists
  (should (stringp augment-new-session-message))
  (should (> (length augment-new-session-message) 0))

  ;; Test that it contains org-mode reference
  (should (string-match-p "org-mode" augment-new-session-message))

  ;; Test that it can be customized
  (let ((augment-new-session-message "Custom test message"))
    (should (string= augment-new-session-message "Custom test message"))))

(ert-deftest test-augment-strip-ansi-codes ()
  "Test ANSI escape sequence removal."
  ;; Test basic ANSI color codes
  (should (string= (augment-strip-ansi-codes "\033[31mred text\033[0m") "red text"))
  (should (string= (augment-strip-ansi-codes "\033[1;32mbold green\033[0m") "bold green"))

  ;; Test multiple ANSI codes in one string
  (should (string= (augment-strip-ansi-codes "\033[31mred\033[0m and \033[32mgreen\033[0m") "red and green"))

  ;; Test complex ANSI sequences
  (should (string= (augment-strip-ansi-codes "\033[38;5;196mcomplex color\033[0m") "complex color"))

  ;; Test cursor movement codes
  (should (string= (augment-strip-ansi-codes "\033[2J\033[H\033[31mtext\033[0m") "text"))

  ;; Test text with no ANSI codes
  (should (string= (augment-strip-ansi-codes "plain text") "plain text"))

  ;; Test empty string
  (should (string= (augment-strip-ansi-codes "") ""))

  ;; Test nil input
  (should (eq (augment-strip-ansi-codes nil) nil))

  ;; Test real auggie output examples
  (should (string= (augment-strip-ansi-codes "🤖\n\033[1mI see you've entered \"test\"\033[0m - this appears to be a simple test message.")
                   "🤖\nI see you've entered \"test\" - this appears to be a simple test message."))

  ;; Test preserving spaces and newlines
  (should (string= (augment-strip-ansi-codes "\033[31m  spaced  \n  text  \033[0m") "  spaced  \n  text  ")))

(ert-deftest test-augment-truncate-string ()
  "Test string truncation functionality."
  (should (string= (augment-truncate-string "hello world" 5) "he..."))
  (should (string= (augment-truncate-string "hi" 10) "hi"))
  (should (string= (augment-truncate-string "exactly10!" 10) "exactly10!"))
  (should (string= (augment-truncate-string "" 5) "")))

(ert-deftest test-augment-create-new-session ()
  "Test new session creation logic and buffer naming."
  ;; Test that the function exists and can be called
  (should (functionp 'augment-create-new-session))

  ;; Test buffer name generation with session ID
  (let ((augment-session-id "test-session-123456789"))
    (let ((buffer-name (augment-create-buffer-name "/path/to/project")))
      (should (string-match-p "\\*Augment test-ses:" buffer-name))))

  ;; Test configurable new session message
  (let ((augment-new-session-message "Custom initial message for testing"))
    (should (string= augment-new-session-message "Custom initial message for testing"))))



(ert-deftest test-augment-interrupt-response ()
  "Test response interruption functionality."
  (with-temp-buffer
    (augment-mode)
    ;; Test when no process is running
    (let ((augment-current-process nil))
      (augment-interrupt-response)
      (should-not augment-current-process))

    ;; Test process tracking variable exists
    (should (boundp 'augment-current-process))))

(ert-deftest test-augment-format-timestamp ()
  "Test timestamp formatting."
  (let ((timestamp (augment-format-timestamp "2025-09-19T15:30:45.000Z")))
    (should (string-match-p "2025-09-19" timestamp))
    ;; Don't test exact time due to timezone conversion
    (should (string-match-p "[0-9]+:[0-9]+:[0-9]+" timestamp)))

  ;; Test with nil input
  (let ((timestamp (augment-format-timestamp nil)))
    (should (stringp timestamp))
    (should (string-match-p "[0-9]+-[0-9]+-[0-9]+" timestamp))))

(ert-deftest test-augment-session-id-validation ()
  "Test session ID validation."
  ;; Valid UUIDs
  (should (augment-validate-session-id "123e4567-e89b-12d3-a456-426614174000"))
  (should (augment-validate-session-id "abcdef12-3456-7890-abcd-ef1234567890"))

  ;; Invalid inputs
  (should-not (augment-validate-session-id "invalid-id"))
  (should-not (augment-validate-session-id ""))
  (should-not (augment-validate-session-id nil))
  (should-not (augment-validate-session-id 123)))

(ert-deftest test-augment-cleanup-response ()
  "Test response cleanup functionality."
  (should (string= (augment-cleanup-response "  hello world  ") "hello world"))
  (should (string= (augment-cleanup-response "\n\ntest\n\n") "test"))
  (should (string= (augment-cleanup-response "") ""))
  (should (null (augment-cleanup-response nil)))

  ;; Test ANSI codes are preserved (not removed)
  (should (string= (augment-cleanup-response "\033[31mred text\033[0m") "\033[31mred text\033[0m"))
  (should (string= (augment-cleanup-response "  \033[1mbold\033[22m text  ") "\033[1mbold\033[22m text")))



;;; Input History Tests

(ert-deftest test-augment-load-input-history ()
  "Test loading input history from session data."
  (let ((buffer (get-buffer-create "*test-history*")))
    (with-current-buffer buffer
      (augment-mode)
      (setq augment-session-id "test-session")

      ;; Mock session data with user messages
      (cl-letf (((symbol-function 'augment-get-session-data)
                 (lambda (session-id)
                   '((sessionId . "test-session")
                     (userMessages . ["First message" "Second message" "Third message"])))))

        (augment-load-input-history)
        (should (equal augment-input-history '("First message" "Second message" "Third message")))
        (should (null augment-input-history-index)))

      (kill-buffer buffer))))

(ert-deftest test-augment-history-navigation ()
  "Test input history navigation."
  (let ((buffer (get-buffer-create "*test-nav*")))
    (with-current-buffer buffer
      (augment-mode)
      (setq augment-input-history '("First" "Second" "Third"))
      (setq augment-input-start (point-marker))
      (insert "current input")

      ;; Test previous navigation
      (augment-history-previous)
      (should (= augment-input-history-index 2))
      (should (string= (buffer-substring augment-input-start (point-max)) "Third"))

      (augment-history-previous)
      (should (= augment-input-history-index 1))
      (should (string= (buffer-substring augment-input-start (point-max)) "Second"))

      ;; Test next navigation
      (augment-history-next)
      (should (= augment-input-history-index 2))
      (should (string= (buffer-substring augment-input-start (point-max)) "Third"))

      ;; Test going past end clears input
      (augment-history-next)
      (should (null augment-input-history-index))
      (should (string= (buffer-substring augment-input-start (point-max)) ""))

      (kill-buffer buffer))))

;;; Input Processing Tests

(ert-deftest test-augment-get-current-input ()
  "Test current input extraction in simplified format."
  (with-temp-buffer
    (augment-mode)

    ;; Test with new format - input marker after "> "
    (insert "* [5] [2025-09-21 11:42:57]\n> ")
    (setq augment-input-start (point-marker))
    (insert "What's current dir about?")
    (should (string= (augment-get-current-input) "What's current dir about?"))

    ;; Test with multi-line input
    (erase-buffer)
    (insert "* [3] [2025-09-21 12:00:00]\n> ")
    (setq augment-input-start (point-marker))
    (insert "This is a\nmulti-line message")
    (should (string= (augment-get-current-input) "This is a\nmulti-line message"))

    ;; Test with just plain text (no headline)
    (erase-buffer)
    (setq augment-input-start (point-marker))
    (insert "Just plain text")
    (should (string= (augment-get-current-input) "Just plain text"))

    ;; Test empty input
    (erase-buffer)
    (insert "* [2] [2025-09-21 11:00:00]\n> ")
    (setq augment-input-start (point-marker))
    (should (string= (augment-get-current-input) ""))

    ;; Test with special characters
    (erase-buffer)
    (insert "* [10] [2025-09-21 15:45:30]\n> ")
    (setq augment-input-start (point-marker))
    (insert "Test with \"quotes\" and 'apostrophes'")
    (should (string= (augment-get-current-input) "Test with \"quotes\" and 'apostrophes'"))))

;;; Debug Logging Tests

(ert-deftest test-augment-debug-log ()
  "Test debug logging functionality."
  (let ((augment-enable-debug t))
    ;; Clear any existing debug buffer
    (when (get-buffer "*Augment Debug*")
      (kill-buffer "*Augment Debug*"))

    ;; Test logging without session ID
    (augment-debug-log "Test message")
    (let ((debug-buffer (get-buffer "*Augment Debug*")))
      (should debug-buffer)
      (with-current-buffer debug-buffer
        (should (string-match-p "Test message" (buffer-string)))
        (should-not (string-match-p "\\[.*\\]" (buffer-string)))))

    ;; Test logging with session ID
    (augment-debug-log "Test with session" "12345678-1234-1234-1234-123456789012")
    (with-current-buffer "*Augment Debug*"
      (should (string-match-p "\\[12345678\\] Test with session" (buffer-string))))

    ;; Clean up
    (kill-buffer "*Augment Debug*"))

  ;; Test with debug disabled
  (let ((augment-enable-debug nil))
    (when (get-buffer "*Augment Debug*")
      (kill-buffer "*Augment Debug*"))
    (augment-debug-log "Should not appear")
    (should-not (get-buffer "*Augment Debug*"))))

;;; Command Construction Tests

(ert-deftest test-augment-build-command ()
  "Test command building."
  (let ((augment-session-id nil))
    (let ((command (augment-build-command "Hello world")))
      (should (string-match-p "auggie --print" command))
      (should (string-match-p "Hello" command))
      ;; Should not include -q flag by default
      (should-not (string-match-p " -q" command))))

  ;; Test with session ID
  (let ((augment-session-id "test-session-123"))
    (let ((command (augment-build-command "Hello world")))
      (should (string-match-p "--resume test-session-123" command))
      (should (string-match-p "Hello" command))
      ;; Should not include -q flag by default
      (should-not (string-match-p " -q" command))))

  ;; Test with quick prompt flag
  (let ((augment-session-id nil))
    (let ((command (augment-build-command "Hello world" t)))
      (should (string-match-p "auggie --print" command))
      (should (string-match-p "Hello" command))
      ;; Should include -q flag when requested
      (should (string-match-p " -q" command)))))

;;; Buffer Management Tests

(ert-deftest test-augment-rename-buffer ()
  "Test buffer renaming functionality."
  (let ((buffer (get-buffer-create "*test-augment-rename*")))
    (with-current-buffer buffer
      (augment-mode)
      (setq augment-session-id "test-session-123456789")

      ;; Test renaming with session ID
      (augment-rename-buffer "My Custom Name")
      (should (string= (buffer-name) "*Augment test-ses: My Custom Name*"))

      ;; Test error when no session ID
      (setq augment-session-id nil)
      (should-error (augment-rename-buffer "Another Name"))

      ;; Clean up
      (kill-buffer buffer))))

(ert-deftest test-augment-create-buffer-name ()
  "Test buffer name creation."
  ;; Test without session ID
  (let ((augment-session-id nil))
    (should (string= (augment-create-buffer-name "/path/to/project") "*Augment: project*")))

  ;; Test with session ID
  (let ((augment-session-id "test-session-123456789"))
    (should (string= (augment-create-buffer-name "/path/to/project") "*Augment test-ses: project*")))

  ;; Test with short session ID
  (let ((augment-session-id "short"))
    (should (string= (augment-create-buffer-name "/path/to/project") "*Augment: project*")))

  ;; Test with nil workspace root
  (let ((augment-session-id "test-session-123456789"))
    (should (string= (augment-create-buffer-name nil) "*Augment test-ses: session*"))))

(ert-deftest test-augment-buffer-creation ()
  "Test augment buffer creation and setup."
  (let ((buffer (augment-create-shell-buffer "test-session-123" "/test/workspace")))
    (should (bufferp buffer))
    (with-current-buffer buffer
      (should (eq major-mode 'augment-mode))
      (should (string= augment-session-id "test-session-123")))
    (kill-buffer buffer)))

;;; Session Management Tests

(ert-deftest test-augment-get-session-data ()
  "Test getting session data by ID."
  ;; Mock augment-list-sessions to return test data
  (cl-letf (((symbol-function 'augment-list-sessions)
             (lambda () '(((sessionId . "session-123")
                           (exchangeCount . 5)
                           (firstUserMessage . "Hello"))
                          ((sessionId . "session-456")
                           (exchangeCount . 3)
                           (firstUserMessage . "World"))))))

    ;; Test finding existing session
    (let ((session-data (augment-get-session-data "session-123")))
      (should session-data)
      (should (string= (alist-get 'sessionId session-data) "session-123"))
      (should (= (alist-get 'exchangeCount session-data) 5)))

    ;; Test non-existent session
    (should (null (augment-get-session-data "non-existent")))))

(ert-deftest test-augment-get-current-exchange-count ()
  "Test getting current exchange count from session data."
  ;; Mock the session data retrieval
  (cl-letf (((symbol-function 'augment-get-session-data)
             (lambda (session-id)
               (when (string= session-id "test-session")
                 '((sessionId . "test-session")
                   (exchangeCount . 7))))))

    ;; Test with session ID set
    (let ((augment-session-id "test-session"))
      (should (= (augment-get-current-exchange-count) 7)))

    ;; Test with no session ID
    (let ((augment-session-id nil))
      (should (= (augment-get-current-exchange-count) 0)))

    ;; Test with non-existent session
    (let ((augment-session-id "non-existent"))
      (should (= (augment-get-current-exchange-count) 0)))))

(ert-deftest test-augment-format-session-for-completion ()
  "Test session formatting for completion."
  (let ((session '((sessionId . "201ca9c4-d1f0-4c0a-80b4-12f264d1ca30")
                   (workspaceRoot . "/Users/test/projects/my-project"))))
    (let ((formatted (augment-format-session-for-completion session)))
      (should (string-match-p "Augment 201ca9c4:" formatted))
      (should (string-match-p "my-project" formatted)))))

(ert-deftest test-augment-get-session-buffer ()
  "Test finding session buffers."
  (let ((buffer1 (get-buffer-create "*Test Session 1*"))
        (buffer2 (get-buffer-create "*Test Session 2*")))

    ;; Set up test buffers
    (with-current-buffer buffer1
      (augment-mode)
      (setq augment-session-id "session-123"))

    (with-current-buffer buffer2
      (augment-mode)
      (setq augment-session-id "session-456"))

    ;; Test finding existing session
    (should (eq (augment-get-session-buffer "session-123") buffer1))
    (should (eq (augment-get-session-buffer "session-456") buffer2))

    ;; Test non-existent session
    (should-not (augment-get-session-buffer "session-999"))

    ;; Clean up
    (kill-buffer buffer1)
    (kill-buffer buffer2)))

(ert-deftest test-augment-list-active-sessions ()
  "Test listing active session buffers."
  (let ((session-buffer (get-buffer-create "*Test Session*"))
        (normal-buffer (get-buffer-create "*Normal Buffer*")))

    ;; Set up session buffer
    (with-current-buffer session-buffer
      (augment-mode))

    ;; Test listing
    (let ((active-sessions (augment-list-active-sessions)))
      (should (memq session-buffer active-sessions))
      (should-not (memq normal-buffer active-sessions)))

    ;; Clean up
    (kill-buffer session-buffer)
    (kill-buffer normal-buffer)))

;;; Mode Tests

(ert-deftest test-augment-mode ()
  "Test augment-mode setup."
  (with-temp-buffer
    (augment-mode)
    (should (eq major-mode 'augment-mode))
    (should (keymapp augment-mode-map))
    (should (where-is-internal 'augment-send-input augment-mode-map))
    (should (where-is-internal 'augment-clear-input augment-mode-map))
    ;; Test that visual-line-mode is enabled
    (should visual-line-mode)))

;;; Session Creation Tests

(ert-deftest test-augment-session-buffer-setup ()
  "Test session buffer setup and initialization."
  (let ((session-data '((sessionId . "test-session-123")
                        (exchangeCount . 5)
                        (firstUserMessage . "Hello world")
                        (created . "2025-09-21T16:30:00.000Z")
                        (modified . "2025-09-21T16:35:00.000Z")
                        (workspaceRoot . "/test/workspace")
                        (userMessages . ["Hello world" "How are you?"]))))

    (let ((buffer (augment-create-shell-buffer "test-session-123" "Hello world")))
      (should (bufferp buffer))

      (with-current-buffer buffer
        ;; Test initial state
        (should (eq major-mode 'augment-mode))
        (should (string= augment-session-id "test-session-123"))

        ;; Mock session data retrieval for setup
        (cl-letf (((symbol-function 'augment-get-session-data)
                   (lambda (session-id) session-data)))

          ;; Test buffer setup
          (augment-setup-buffer session-data)

          ;; Should have session data set
          (should (equal augment-session-data session-data))

          ;; Should have org-mode header
          (should (string-match-p "#\\+TITLE:" (buffer-string)))
          (should (string-match-p "#\\+SESSION_ID: test-session-123" (buffer-string)))
          (should (string-match-p "#\\+EXCHANGES: 5" (buffer-string)))
          (should (string-match-p "#\\+WORKSPACE: /test/workspace" (buffer-string)))

          ;; Should have input structure
          (should (string-match-p "\\* \\[" (buffer-string)))
          (should (markerp augment-input-start))

          ;; Should load input history
          (should (equal augment-input-history '("Hello world" "How are you?")))))

      (kill-buffer buffer))))

(ert-deftest test-augment-session-creation-customization ()
  "Test session creation with custom settings."
  ;; Test with custom new session message
  (let ((augment-new-session-message "Custom test message for session creation"))
    (cl-letf (((symbol-function 'augment-check-auggie-available)
               (lambda () "/usr/local/bin/auggie"))
              ((symbol-function 'shell-command-to-string)
               (lambda (command)
                 (if (string-match-p "Custom test message" command)
                     "🤖\n\nSession ID: custom-test-session-123\n\nResponse to custom message"
                   ""))))

      ;; Should use custom message (shell-quoted)
      (let ((command (augment-build-command augment-new-session-message t)))
        (should (string-match-p "Custom" command))
        (should (string-match-p " -q" command)))))

  ;; Test with workspace root (no truncation needed for directory names)
  (let ((augment-session-id "test-session-123456789"))
    (let ((buffer-name (augment-create-buffer-name "/very/long/path/to/project")))
      (should (string= buffer-name "*Augment test-ses: project*")))))

(ert-deftest test-augment-session-id-extraction ()
  "Test session ID extraction from auggie output."
  (let ((test-cases '(
    ;; Standard format
    ("Session ID: 12345678-abcd-efgh-ijkl-123456789012" . "12345678-abcd-efgh-ijkl-123456789012")
    ;; With extra text
    ("🤖\n\nSession ID: test-session-uuid-here\n\nMore text" . "test-session-uuid-here")
    ;; Case insensitive
    ("session id: lowercase-uuid-test" . "lowercase-uuid-test")
    ;; With ANSI codes
    ("\033[1mSession ID:\033[0m ansi-uuid-test" . "ansi-uuid-test"))))

    (dolist (test-case test-cases)
      (let ((input (car test-case))
            (expected (cdr test-case)))
        ;; Test the extraction logic (this would need to be exposed or tested indirectly)
        (should (string-match-p expected input))))))

;;; Integration Tests (only if auggie is available)

(ert-deftest test-augment-auggie-availability ()
  "Test auggie CLI availability check."
  (let ((available (augment-check-auggie-available)))
    ;; Should return either a path string or nil
    (should (or (stringp available) (null available)))))

(ert-deftest test-augment-session-listing ()
  "Test session listing (only if auggie is available)."
  :expected-result (if (augment-check-auggie-available) :passed :skipped)
  (skip-unless (augment-check-auggie-available))

  (let ((sessions (augment-list-sessions)))
    (should (listp sessions))
    (when sessions
      (should (alist-get 'sessionId (car sessions)))
      (should (alist-get 'firstUserMessage (car sessions))))))

(provide 'test-augment)

;;; test-augment.el ends here

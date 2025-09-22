# Augment.el - Emacs Integration for Auggie CLI

An Emacs package that provides seamless integration with the Auggie CLI, allowing you to interact with Augment Agent directly from within Emacs using org-mode formatted conversations.

## Features

- **Session Management**: Browse and resume existing auggie sessions with completion interface
- **Org-Mode Integration**: Conversations formatted as org-mode documents with proper structure
- **Interactive Shell**: Type messages and get responses with RET, similar to a chat interface
- **Input History**: Navigate through previous messages with M-p/M-n (like shell history)
- **Streaming Responses**: Real-time display of responses as they arrive
- **Process Management**: Interrupt long-running responses with C-c C-c
- **Buffer Renaming**: Rename session buffers for better organization
- **Debug Logging**: Optional debug mode for troubleshooting

## Prerequisites

1. **Auggie CLI**: Install the auggie command-line tool
   ```bash
   npm install -g @augment/cli
   ```

2. **Emacs**: Version 26.1 or later with org-mode 9.0+

3. **Authentication**: Ensure you're logged in to auggie
   ```bash
   auggie login
   ```

## Installation

### Manual Installation

1. Clone or download this repository to your Emacs configuration directory:
   ```bash
   cd ~/.emacs.d/lisp
   git clone <repository-url> augment-el
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/augment-el")
   (require 'augment)
   ```

### Using use-package

```elisp
(use-package augment
  :load-path "~/.emacs.d/lisp/augment-el"
  :commands (augment)
  :bind ("C-c a" . augment))
```

## Usage

### Starting a Session

1. **Launch Augment**: `M-x augment`
2. **Select Session**: Choose from existing sessions or select "🆕 New Session"
3. **Start Chatting**: Type your message and press RET to send

### Key Bindings

- `RET` - Send current input to auggie
- `C-c C-i` - Clear current input
- `C-c C-c` - Interrupt current response
- `C-c C-l` - Clear conversation history (keep session metadata)
- `C-c C-q` - Quit and close buffer
- `C-c C-r` - Rename buffer
- `M-p` - Previous input in history
- `M-n` - Next input in history
- `TAB` - Org-mode tab cycling (fold/unfold sections)
- `S-TAB` - Global org visibility cycling

### Buffer Structure

Each augment buffer is an org-mode document with:

```org
#+TITLE: Augment Session: [first message]
#+SESSION_ID: [uuid]
#+EXCHANGES: [count]
#+CREATED: [timestamp]
#+MODIFIED: [timestamp]
#+WORKSPACE: [path]

* [1] [2025-09-21 15:30:45]
> Your first message here

Assistant response with org-mode formatting:
- *Bold text*
- /Italic text/
- =Code snippets=

#+BEGIN_SRC python
def example():
    return "code blocks with syntax highlighting"
#+END_SRC

* [2] [2025-09-21 15:31:20]
> [your next message here]
```

The structure uses a simplified format where each exchange is a top-level headline with the exchange number and timestamp, followed by the user input after "> " and the assistant response.

## Detailed Features

### Session Selection Interface

When you run `M-x augment`, you get a completion interface showing:
- "🆕 New Session" option at the top
- Existing sessions formatted as: `[short-id]: [first-message]`
- Supports both standard `completing-read` and Helm integration

### Automatic Org-Mode Formatting

New sessions automatically send an initial message requesting org-mode formatting:
```
Please format all your responses using org-mode syntax including proper headlines,
code blocks with #+BEGIN_SRC, emphasis markup, and other org-mode conventions.
Make sure files or dir are printed as links using [[file:path][filename]]
This will ensure your responses display beautifully in this Emacs org-mode buffer.
```

### Streaming Response Display

- Responses appear in real-time as auggie generates them
- ANSI escape codes are automatically stripped for clean display
- Auto-scrolling keeps the latest content visible
- Loading indicators show when processing

### Buffer Management Features

- **Smart Naming**: Buffers named with session ID prefix and first message
- **Buffer Renaming**: Use `C-c C-r` to rename buffers with custom names
- **Session Switching**: `M-x augment-switch-to-session` to switch between active sessions
- **Cleanup**: Automatic cleanup of markers and processes when buffers are killed

## Customization

### Variables

```elisp
;; Maximum length for buffer names
(setq augment-buffer-name-max-length 50)

;; Auto-scroll to bottom after responses
(setq augment-auto-scroll t)

;; Preserve input on errors
(setq augment-preserve-input-on-error t)

;; Initial message sent when creating new sessions
(setq augment-new-session-message "Please format all your responses using org-mode syntax...")

;; Enable debug logging
(setq augment-enable-debug t)
```

### Hooks

```elisp
;; Run when entering augment-mode
(add-hook 'augment-mode-hook 'my-augment-setup)

;; Run after displaying response
(add-hook 'augment-response-hook 'my-response-handler)

;; Run before sending message
(add-hook 'augment-send-hook 'my-send-handler)

;; Run after setting up org structure
(add-hook 'augment-org-setup-hook 'my-org-setup)
```

## Advanced Usage

### Multiple Sessions

- Each session runs in its own buffer
- Switch between sessions with `M-x augment-switch-to-session`
- Session buffers are named after the first message with session ID prefix
- Use `C-c C-r` to rename buffers for better organization

### Org-Mode Features

Since conversations are org-mode documents, you can:

- **Export**: `C-c C-e` to export to HTML, PDF, etc.
- **Search**: Use org-mode search and filtering
- **Fold**: Hide/show conversation sections with TAB
- **Links**: Click on links in responses
- **Copy**: Copy formatted text with org markup

### Session Management

- **Resume**: Select existing sessions from the completion interface
- **History**: Browse previous conversations in org format
- **Input History**: Navigate through previous messages with M-p/M-n
- **Cleanup**: Use `C-c C-l` to clear history while keeping metadata
- **Process Control**: Interrupt responses with `C-c C-c`

### Input History

The package maintains a history of your input messages for each session:

- **M-p** - Navigate to previous input
- **M-n** - Navigate to next input (or clear input if at end)
- History is automatically loaded from session data when resuming sessions
- History navigation works like shell command history

## Troubleshooting

### Common Issues

1. **"Auggie CLI not found"**
   - Install auggie: `npm install -g @augment/cli`
   - Ensure it's in your PATH

2. **"Failed to parse session list"**
   - Check auggie authentication: `auggie login`
   - Verify auggie is working: `auggie session list`

3. **"No response from auggie"**
   - Check network connection
   - Verify auggie credentials
   - Try running auggie directly in terminal

4. **Formatting Issues**
   - Check the `augment-new-session-message` variable
   - Verify auggie is responding with org-mode formatting

5. **Process Issues**
   - Use `C-c C-c` to interrupt stuck responses
   - Check for running processes with `M-x list-processes`

### Debug Mode

Enable debug logging to troubleshoot issues:
```elisp
(setq augment-enable-debug t)
```

This creates a `*Augment Debug*` buffer with detailed logging of commands and responses.

## Implementation Details

### Architecture

The package is built around several key components:

1. **Session Management**: Uses `auggie session list --json` to retrieve session metadata
2. **Buffer Management**: Each session gets its own buffer in `augment-mode` (derived from `org-mode`)
3. **Process Management**: Handles streaming responses from auggie CLI with process filters
4. **Input History**: Maintains per-session input history loaded from session data

### Key Functions

- `augment()` - Main entry point that presents session selection
- `augment-create-new-session()` - Creates new sessions with initial org-mode formatting prompt
- `augment-send-message()` - Sends messages to auggie CLI with streaming response handling
- `augment-display-streaming-response()` - Real-time display of responses as they arrive
- `augment-load-input-history()` - Loads input history from session metadata

### Buffer Structure

Buffers use a simplified org-mode structure:
- Header with session metadata (title, session ID, exchange count, etc.)
- Each exchange as a top-level headline with timestamp
- User input follows "> " prompt
- Assistant responses are inserted directly with org-mode formatting

### Process Handling

- Uses `start-process-shell-command` for non-blocking execution
- Process filters handle streaming output with ANSI code stripping
- Process sentinels handle completion and error states
- Supports interruption with `C-c C-c`

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with your auggie setup
5. Submit a pull request

## License

This project is licensed under the MIT License.

## Support

- Report issues on GitHub
- Check auggie CLI documentation
- Emacs org-mode documentation for advanced features

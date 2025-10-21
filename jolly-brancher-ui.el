;;; jolly-brancher-ui.el --- UI components for jolly-brancher -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains UI components, display functions, and mode definitions
;; for the jolly-brancher package.

;;; Code:

(require 'jolly-brancher-core)
(require 'jolly-brancher-jql)
(require 'jolly-brancher-utils)

;; UI Functions

(defun jolly-brancher--format-ticket-line (line)
  "Format a ticket LINE with proper spacing and alignment."
  ;; Just return the line as is to preserve the exact format
  line)

(defun jolly-brancher--process-ticket-output (output)
  "Process the raw ticket OUTPUT to improve formatting."
  (let ((lines (split-string output "\n" t))
        (formatted-lines '())
        (in-header t))
    ;; Process each line
    (dolist (line lines)
      (cond
       ;; Keep header lines as is
       ((or in-header
            (string-prefix-p "Repository:" line)
            (string-prefix-p "JQL Query:" line)
            (string-prefix-p "Filter:" line)
            (string-prefix-p "Status:" line)
            (string-prefix-p "Project:" line)
            (string-prefix-p "Assignee:" line)
            (string-empty-p line))
        (push line formatted-lines)
        ;; Empty line after header info marks end of header
        (when (string-empty-p line)
          (setq in-header nil)))
       
       ;; Format ticket lines
       ((string-match "^[A-Z]+-[0-9]+" line)
        (let ((formatted (jolly-brancher--format-ticket-line line)))
          (when formatted
            (push formatted formatted-lines))))
       
       ;; Keep other lines as is
       (t (push line formatted-lines))))
    
    ;; Return the formatted output
    (string-join (nreverse formatted-lines) "\n")))

(defun jolly-brancher--display-tickets (cmd repo-path)
  "Display tickets using CMD in a buffer for REPO-PATH."
  (let ((buffer (get-buffer-create "*jolly-brancher-tickets*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (raw-output (shell-command-to-string cmd)))
        ;; Clear buffer and set mode
        (erase-buffer)
        (jolly-brancher-tickets-mode)
        
        ;; Set local variables
        (setq-local jolly-brancher--list-command cmd
                    jolly-brancher--list-repo-path repo-path
                    jolly-brancher--current-repo repo-path
                    jolly-brancher--current-jql nil
                    jolly-brancher--current-created-within nil)
        
        ;; Process and insert the formatted output
        (insert (jolly-brancher--process-ticket-output raw-output))
        
        ;; Ensure font-lock is applied with our custom setup
        (jolly-brancher-setup-font-lock)
        (font-lock-ensure)
        
        ;; Move to beginning of buffer
        (goto-char (point-min))))
    
    ;; Display the buffer
    (pop-to-buffer buffer)))

;; Font-lock and faces

(defface jolly-brancher-ticket-face
  '((t (:foreground "magenta" :weight bold)))
  "Face for ticket IDs.")

(defface jolly-brancher-status-face
  '((t (:foreground "yellow" :weight bold)))
  "Face for status values.")

(defface jolly-brancher-type-face
  '((t (:foreground "cyan" :weight bold)))
  "Face for type values.")

(defface jolly-brancher-available-face
  '((t (:foreground "green" :weight bold)))
  "Face for available status.")

(defface jolly-brancher-preparing-face
  '((t (:foreground "green" :weight bold)))
  "Face for preparing status.")

(defun jolly-brancher-setup-font-lock ()
  "Set up font-lock keywords for jolly-brancher-mode."
  (font-lock-add-keywords
   nil
   '(
     ;; Ticket IDs (matches any JIRA format: PROJECT-NUMBER)
     ("^\\([A-Z]+-[0-9]+\\)" 1 'jolly-brancher-ticket-face)
     
     ;; Type values - match exact patterns with word boundaries
     ;; Put these before status patterns to ensure they take precedence
     ("\\<\\(Bug\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Task\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Epic\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Story\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Subtask\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Spike\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Incident\\)\\>" 1 'jolly-brancher-type-face)
     ("\\<\\(Tech Debt\\)\\>" 1 'jolly-brancher-type-face)
     
     ;; Status patterns - match exact patterns with word boundaries
     ("\\<\\(Backlog\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(Blocked\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(Done\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(In Progress\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(In Review\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(New\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(QA\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(Staged\\)\\>" 1 'jolly-brancher-status-face)
     ("\\<\\(To Do\\)\\>" 1 'jolly-brancher-status-face)

     ;; Special status terms
     ("\"\\(available\\)\"" 1 'jolly-brancher-available-face)
     ("\"\\(preparing\\)\"" 1 'jolly-brancher-preparing-face))
   t))

;; Define the variable for backward compatibility
(defvar jolly-brancher-tickets-mode-font-lock-keywords
  '(
    ;; Ticket IDs (matches any JIRA format: PROJECT-NUMBER)
    ("^\\([A-Z]+-[0-9]+\\)" 1 'jolly-brancher-ticket-face)
    
    ;; Type values - match exact patterns with word boundaries
    ("\\<\\(Bug\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Task\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Epic\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Story\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Subtask\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Spike\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Incident\\)\\>" 1 'jolly-brancher-type-face)
    ("\\<\\(Tech Debt\\)\\>" 1 'jolly-brancher-type-face)

    ;; Status patterns - match exact patterns with word boundaries
    ("\\<\\(Backlog\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(Blocked\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(Done\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(In Progress\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(In Review\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(New\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(QA\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(Staged\\)\\>" 1 'jolly-brancher-status-face)
    ("\\<\\(To Do\\)\\>" 1 'jolly-brancher-status-face)
    )
  "Font lock keywords for `jolly-brancher-tickets-mode'.")

;; Tickets mode definition and keymap

(defvar jolly-brancher-tickets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jolly-brancher-start-ticket-at-point)
    (define-key map (kbd "v") 'jolly-brancher-open-ticket-in-browser)
    (define-key map (kbd "g") 'jolly-brancher-refresh-tickets)
    (define-key map (kbd "s") 'jolly-brancher-change-ticket-status)
    (define-key map (kbd "y") 'jolly-brancher-change-ticket-type)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "m") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "n") 'jolly-brancher-list-next-up-tickets)
    (define-key map (kbd "u") 'jolly-brancher-list-unassigned-tickets)
    (define-key map (kbd "a") 'jolly-brancher-list-all-tickets)
    (define-key map (kbd "/") 'jolly-brancher-search-tickets)
    (define-key map (kbd "?") 'jolly-brancher-tickets-menu)
    (define-key map (kbd "e") 'jolly-brancher-end-ticket)
    (define-key map (kbd "f") 'jolly-brancher-filter-menu)
    (define-key map (kbd "o") 'make-jql-older)
    (define-key map (kbd "w") 'make-jql-newer)
    map)
  "Keymap for `jolly-brancher-tickets-mode'.")

(define-derived-mode jolly-brancher-tickets-mode special-mode "Jolly Brancher"
  "Major mode for viewing Jira tickets."
  (setq buffer-read-only t)
  (setq mode-name "Jolly Brancher Tickets")
  
  ;; Explicitly define the local map and override RET
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'jolly-brancher-start-ticket-at-point)
    (define-key map (kbd "v") 'jolly-brancher-open-ticket-in-browser)
    (define-key map (kbd "g") 'jolly-brancher-refresh-tickets)
    (define-key map (kbd "s") 'jolly-brancher-change-ticket-status)
    (define-key map (kbd "y") 'jolly-brancher-change-ticket-type)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "m") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "n") 'jolly-brancher-list-next-up-tickets)
    (define-key map (kbd "u") 'jolly-brancher-list-unassigned-tickets)
    (define-key map (kbd "a") 'jolly-brancher-list-all-tickets)
    (define-key map (kbd "/") 'jolly-brancher-search-tickets)
    (define-key map (kbd "?") 'jolly-brancher-tickets-menu)
    (define-key map (kbd "e") 'jolly-brancher-end-ticket)
    (define-key map (kbd "f") 'jolly-brancher-filter-menu)
    (define-key map (kbd "o") 'make-jql-older)
    (define-key map (kbd "w") 'make-jql-newer)
    (use-local-map map))
  
  ;; Set up font-lock with our keywords
  (setq-local font-lock-defaults '(nil t))
  (jolly-brancher-setup-font-lock)
  
  ;; Enable font-lock and other modes
  (font-lock-mode 1)
  (hl-line-mode 1))

(defun jolly-brancher--highlight-ticket ()
  "Highlight the current ticket line."
  (when (eq major-mode 'jolly-brancher-tickets-mode)
    (let ((ticket (jolly-brancher--get-ticket-at-point)))
      (when ticket
        (message "Current ticket: %s" ticket)))))

(provide 'jolly-brancher-ui)

;;; jolly-brancher-ui.el ends here

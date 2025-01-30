;;; jolly-brancher.el --- Git branch management with Jira integration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ashton Von Honnecke

;; Author: Ashton Von Honnecke
;; Keywords: vc, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (project "0.9.8"))
;; URL: https://github.com/ahonnecke/jolly-brancher

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides an Emacs interface to the jolly-brancher tool for Git branch
;; management with Jira integration.  Uses transient.el for a modern,
;; discoverable interface similar to Magit.
;;
;; To use, add to your init.el:
;;   (require 'jolly-brancher)
;;
;; Then you can use:
;;   C-c j j - Open jolly-brancher menu
;;   C-c j l - List tickets
;;   C-c j s - Start a branch
;;   C-c j e - End branch and create PR

;;; Code:

(require 'transient)
(require 'project)

(defgroup jolly-brancher nil
  "Git branch management with Jira integration."
  :group 'tools
  :prefix "jolly-brancher-")

(defcustom jolly-brancher-command "/home/ahonnecke/.pyenv/shims/jolly-brancher"
  "Command to run jolly-brancher."
  :type 'string
  :group 'jolly-brancher)

(defcustom jolly-brancher-issue-types
  '("Bug" "Story" "Task" "Spike" "Epic" "Subtask" )
  "List of available issue types."
  :type '(repeat string)
  :group 'jolly-brancher)

(defcustom jolly-brancher-jira-url "https://cirrusv2x.atlassian.net"
  "Base URL for Jira instance."
  :type 'string
  :group 'jolly-brancher)

(defvar-local jolly-brancher--current-repo nil
  "The current repository path for jolly-brancher commands.")

(defvar-local jolly-brancher--list-command nil
  "Store the command used to generate the ticket list.")

(defvar-local jolly-brancher--list-repo-path nil
  "Store the repository path for the ticket list.")

(defvar jolly-brancher-status-options
  '("To Do" "In Progress" "Backlog" "New" "In Review" "Blocked" "QA" "Staged" "Done")
  "List of available status options for tickets.")

;; Face definitions for syntax highlighting
(defface jolly-brancher-ticket-face
  '((((class color) (background light))
     (:foreground "#4078f2" :weight bold))
    (((class color) (background dark))
     (:foreground "#88c0d0" :weight bold)))
  "Face for ticket numbers."
  :group 'jolly-brancher)

(defface jolly-brancher-status-face
  '((((class color) (background light))
     (:foreground "#50a14f" :weight bold))
    (((class color) (background dark))
     (:foreground "#a3be8c" :weight bold)))
  "Face for ticket status."
  :group 'jolly-brancher)

(defface jolly-brancher-query-face
  '((((class color) (background light))
     (:foreground "#6c71c4" :weight bold))
    (((class color) (background dark))
     (:foreground "#8f94fb" :weight bold)))
  "Face for the query heading."
  :group 'jolly-brancher)

(defface jolly-brancher-repo-face
  '((((class color) (background light))
     (:foreground "#986801" :weight bold))
    (((class color) (background dark))
     (:foreground "#ebcb8b" :weight bold)))
  "Face for repository path."
  :group 'jolly-brancher)

(defface jolly-brancher-current-ticket-face
  '((((class color) (background light))
     (:background "#e8f0ff"))
    (((class color) (background dark))
     (:background "#2e3440")))
  "Face for the currently checked out ticket."
  :group 'jolly-brancher)

(defconst jolly-brancher-tickets-mode-font-lock-keywords
  `(
    ;; Repository path
    ("^Repository: \\(.*\\)$" (1 'jolly-brancher-repo-face))
    ;; Active Query heading
    ("^Active Query:$" . 'jolly-brancher-query-face)
    ;; Current ticket line (marked with *)
    ("^\\([A-Z]+-[0-9]+\\).*\\*$" (0 'jolly-brancher-current-ticket-face append))
    ;; Ticket numbers (e.g. PD-1234)
    ("[A-Z]+-[0-9]+" . 'jolly-brancher-ticket-face)
    ;; Status text in brackets
    ("\\[\\([^]]+\\)\\]" 1 'jolly-brancher-status-face)))

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for Git branch management with Jira integration."
  :lighter " JB"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c j j") 'jolly-brancher-menu)
            (define-key map (kbd "C-c j l") 'jolly-brancher-list-my-tickets)
            (define-key map (kbd "C-c j s") 'jolly-brancher-start-ticket)
            (define-key map (kbd "C-c j e") 'jolly-brancher-end-branch)
            (define-key map (kbd "C-c j t") 'jolly-brancher-mode-change-status)
            (define-key map (kbd "C-c j c") 'jolly-brancher--maybe-create-from-region)
            map))

(defun jolly-brancher--display-tickets (command repo-path)
  "Run COMMAND and display results in a tickets buffer with REPO-PATH."
  (let ((buf (get-buffer-create "*jolly-brancher-tickets*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jolly-brancher-tickets-mode)
        (setq-local jolly-brancher--list-command command)
        (setq-local jolly-brancher--list-repo-path repo-path)
        (setq-local jolly-brancher--current-repo repo-path)
        (insert (propertize (format "%s Tickets\n\n" (jolly-brancher--get-repo-name repo-path)) 'face 'bold))
        (insert "Press ? to show available commands\n\n")
        (shell-command command t)
        (goto-char (point-min))))
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-same-window)))))

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
    (define-key map (kbd "e") 'jolly-brancher-end-ticket)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "m") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "n") 'jolly-brancher-list-next-up-tickets)
    (define-key map (kbd "u") 'jolly-brancher-list-unassigned-tickets)
    (define-key map (kbd "a") 'jolly-brancher-list-all-tickets)
    (define-key map (kbd "/") 'jolly-brancher-search-tickets)
    (define-key map (kbd "?") 'jolly-brancher-tickets-menu)
    (use-local-map map))
  
  (setq-local font-lock-defaults '(jolly-brancher-tickets-mode-font-lock-keywords))
  (font-lock-mode 1)
  (hl-line-mode 1))

(defvar jolly-brancher-tickets-mode-font-lock-keywords
  '(("^\\([A-Z]+-[0-9]+\\)" 1 'jolly-brancher-ticket-face)
    ("│ \\(In Progress\\|Done\\|To Do\\|New\\) │" 1 'jolly-brancher-status-face)
    ("│ \\(Story\\|Bug\\|Task\\) │" 1 'jolly-brancher-type-face))
  "Font lock keywords for `jolly-brancher-tickets-mode'.")

(defface jolly-brancher-ticket-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for ticket IDs in jolly-brancher mode.")

(defface jolly-brancher-status-face
  '((t :inherit font-lock-string-face))
  "Face for ticket status in jolly-brancher mode.")

(defface jolly-brancher-type-face
  '((t :inherit font-lock-type-face))
  "Face for ticket type in jolly-brancher mode.")

(defvar jolly-brancher-tickets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jolly-brancher-start-ticket-at-point)
    (define-key map (kbd "v") 'jolly-brancher-open-ticket-in-browser)
    (define-key map (kbd "g") 'jolly-brancher-refresh-tickets)
    (define-key map (kbd "s") 'jolly-brancher-change-ticket-status)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "m") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "n") 'jolly-brancher-list-next-up-tickets)
    (define-key map (kbd "u") 'jolly-brancher-list-unassigned-tickets)
    (define-key map (kbd "a") 'jolly-brancher-list-all-tickets)
    (define-key map (kbd "/") 'jolly-brancher-search-tickets)
    (define-key map (kbd "?") 'jolly-brancher-tickets-menu)
    (define-key map (kbd "e") 'jolly-brancher-end-ticket)
    map)
  "Keymap for `jolly-brancher-tickets-mode'.")

(transient-define-prefix jolly-brancher-tickets-menu ()
  "Show menu for actions in the tickets buffer."
  ["Actions"
   ("RET" "Start branch for ticket" jolly-brancher-start-ticket-at-point)
   ("v" "View ticket in browser" jolly-brancher-open-ticket-in-browser)
   ("g" "Refresh list" jolly-brancher-refresh-tickets)
   ("s" "Change ticket status" jolly-brancher-change-ticket-status)
   ("/" "Search tickets" jolly-brancher-search-tickets)
   ("q" "Quit window" quit-window)]
  ["Filter tickets"
   ("m" "Show my tickets" jolly-brancher-list-my-tickets)
   ("n" "Show next-up tickets" jolly-brancher-list-next-up-tickets)
   ("u" "Show unassigned tickets" jolly-brancher-list-unassigned-tickets)
   ("a" "Show all tickets" jolly-brancher-list-all-tickets)])

(defun jolly-brancher--highlight-ticket ()
  "Highlight the current ticket line."
  (when (eq major-mode 'jolly-brancher-tickets-mode)
    (let ((ticket (jolly-brancher--get-ticket-at-point)))
      (when ticket
        (message "Current ticket: %s" ticket)))))

(defun jolly-brancher--get-repo-root ()
  "Get the root directory of the current Git repository.
Returns nil if not in a Git repository."
  (when-let* ((project (project-current))
              (root (project-root project)))
    (expand-file-name root)))

(defun jolly-brancher--get-repo-name (repo-path)
  "Get the repository name from REPO-PATH."
  (file-name-nondirectory (directory-file-name repo-path)))

(defun jolly-brancher-refresh-tickets ()
  "Manually refresh the tickets list."
  (interactive)
  (when (and jolly-brancher--list-command jolly-brancher--list-repo-path)
    (jolly-brancher--display-tickets jolly-brancher--list-command jolly-brancher--list-repo-path)))

(defun jolly-brancher--get-ticket-at-point ()
  "Get the ticket ID at point."
  (save-excursion
    (beginning-of-line)
    (message "Debugging: Current line: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (when (looking-at "^\\([A-Z]+-[0-9]+\\)")
      (message "Debugging: Ticket regex match found")
      (match-string-no-properties 1))))

(defun jolly-brancher--list-tickets (current-user no-assignee created-within)
  "List tickets with optional filters.

CURRENT-USER: If non-nil, show only tickets assigned to current user.
NO-ASSIGNEE: If non-nil, show only unassigned tickets.
CREATED-WITHIN: Time period for created filter (e.g. \"5w\" for 5 weeks)."
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let ((args '()))
        (when current-user
          (push "--current-user" args))
        (when no-assignee
          (push "--no-assignee" args))
        (when created-within
          (push (concat "--created_within=" created-within) args))
        (let ((cmd (jolly-brancher--format-command repo-path "list" args)))
          (jolly-brancher--display-tickets cmd repo-path)))
    (message "Not in a git repository")))

;;;###autoload
(defun jolly-brancher ()
  "Start jolly-brancher."
  (interactive)
  (jolly-brancher-list-next-up-tickets))

(defun jolly-brancher-list-my-tickets ()
  "List tickets assigned to the current user."
  (interactive)
  (jolly-brancher--list-tickets t nil nil))

(defun jolly-brancher-list-unassigned-tickets ()
  "List unassigned tickets."
  (interactive)
  (jolly-brancher--list-tickets nil t "5w"))

(defun jolly-brancher-list-all-tickets ()
  "List all tickets."
  (interactive)
  (jolly-brancher--list-tickets nil nil "5w"))

(defun jolly-brancher-list-next-up-tickets ()
  "List tickets that are In Progress or New, assigned to current user, in PD project."
  (interactive)
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let ((cmd (jolly-brancher--format-command repo-path "list" '("--next-up"))))
        (jolly-brancher--display-tickets cmd repo-path))
    (message "Not in a git repository")))

(defun jolly-brancher-open-ticket-in-browser ()
  "Open the Jira ticket at point in a web browser."
  (interactive)
  (message "Debugging: Attempting to open ticket in browser")
  (when-let ((ticket (jolly-brancher--get-ticket-at-point)))
    (message "Debugging: Found ticket: %s" ticket)
    (let ((jira-url jolly-brancher-jira-url))
      (message "Debugging: Jira URL: %s" jira-url)
      (browse-url (format "%s/browse/%s" jira-url ticket)))))

(defun jolly-brancher-start-ticket-at-point ()
  "Start a branch for the ticket at point."
  (interactive)
  (message "Debugging: Starting ticket start process")
  (when-let* ((ticket-id (jolly-brancher--get-ticket-at-point))
              (repo-path (buffer-local-value 'jolly-brancher--current-repo (current-buffer))))
    (message "Debugging: Ticket ID found: %s, Repo Path: %s" ticket-id repo-path)
    (let ((cmd (jolly-brancher--format-command repo-path "start" (list "--ticket" ticket-id))))
      (message "Starting branch for ticket %s in %s" ticket-id repo-path)
      (shell-command cmd))))

(defun jolly-brancher-start-ticket (ticket-key)
  "Start work on TICKET-KEY."
  (interactive "sTicket key: ")
  (let ((cmd (jolly-brancher--format-command nil "start" (list "--ticket" ticket-key))))
    (message "Running command: %s" cmd)
    (shell-command cmd)))

(defun jolly-brancher-end-branch ()
  "End current branch and create PR."
  (interactive)
  (when (yes-or-no-p "Create PR for current branch? ")
    (let* ((repo-path (jolly-brancher--get-repo-root))
           (cmd (jolly-brancher--format-command repo-path "end" nil)))
      (message "Running command: %s" cmd)
      (shell-command cmd))))

(defun jolly-brancher-end-ticket ()
  "End work on the current ticket branch and create a PR."
  (interactive)
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let ((cmd (jolly-brancher--format-command repo-path "end-ticket" nil)))
        (message "Ending ticket and creating PR...")
        (shell-command cmd))
    (message "Not in a git repository")))

(defun jolly-brancher-mode-end-ticket ()
  "Default end ticket command - ends current ticket and creates PR."
  (interactive)
  (jolly-brancher-end-ticket))

(defun jolly-brancher--get-suggested-reviewers ()
  "Get list of suggested reviewers based on file history."
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (output (shell-command-to-string
                  (format "%s suggest-reviewers --repo %s"
                          jolly-brancher-command
                          default-directory))))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

(defun jolly-brancher--get-reviewers ()
  "Get list of potential reviewers for PR."
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (manual-reviewers (shell-command-to-string
                            (format "%s list-reviewers --repo %s"
                                    jolly-brancher-command
                                    default-directory)))
         (suggested-reviewers (jolly-brancher--get-suggested-reviewers))
         (all-reviewers (append 
                         (when (and manual-reviewers
                                    (not (string-empty-p manual-reviewers)))
                           (split-string manual-reviewers "\n" t))
                         suggested-reviewers)))
    ;; Remove duplicates and sort
    (delete-dups all-reviewers)))

(defun jolly-brancher--select-reviewers ()
  "Interactively select reviewers from the available list."
  (let ((reviewers (jolly-brancher--get-reviewers)))
    (when reviewers  ; Only prompt if we got reviewers back
      (completing-read-multiple
       "Select reviewers (comma-separated): "
       reviewers))))

(defun jolly-brancher--format-description (text)
  "Format TEXT for use as a Jira description.
Wraps code blocks in triple backticks and preserves newlines."
  (let ((lines (split-string text "\n")))
    (format "{noformat}\n%s\n{noformat}" (string-join lines "\n"))))

(defun jolly-brancher--maybe-create-from-region ()
  "If region is active, create a ticket with the selected text as description."
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (deactivate-mark)
      (jolly-brancher-create-ticket text))))

(defun jolly-brancher-create-ticket (title description)
  "Create a new ticket with TITLE and DESCRIPTION."
  (interactive
   (list
    (read-string "Ticket title: ")
    (read-string "Ticket description: ")))
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (cmd (jolly-brancher--format-command nil "create-ticket"
                                              (list "--title" title
                                                    "--description" description))))
    (message "Creating ticket...")
    (shell-command cmd)))

(defun jolly-brancher-set-ticket-status (ticket-key status)
  "Set the status of TICKET-KEY to STATUS."
  (let ((cmd (jolly-brancher--format-command nil "set-status" (list "--ticket" ticket-key "--status" status))))
    (message "Setting status of %s to %s..." ticket-key status)
    (shell-command cmd)))

(defun jolly-brancher-change-ticket-status ()
  "Change the status of a ticket."
  (interactive)
  (let* ((ticket (jolly-brancher--get-ticket-at-point))
         (status (completing-read "New status: " jolly-brancher-status-options nil t)))
    (if ticket
        (jolly-brancher-set-ticket-status ticket status)
      (let ((ticket-key (read-string "Ticket key: ")))
        (jolly-brancher-set-ticket-status ticket-key status)))))

(defun jolly-brancher-mode-change-status ()
  "Default change status command - changes status of current ticket."
  (interactive)
  (jolly-brancher-change-ticket-status))

(defun jolly-brancher--switch-to-tickets ()
  "Switch to the jolly-brancher tickets buffer if it exists, otherwise create it."
  (let ((buffer (get-buffer "*jolly-brancher-tickets*")))
    (if buffer
        (switch-to-buffer buffer)
      (jolly-brancher-list-next-up-tickets))))

(defun jolly-brancher--format-command (repo-path action &rest args)
  "Format a jolly-brancher command with REPO-PATH, ACTION and ARGS."
  (message "DEBUG: Command args before processing: %S" args)
  (let ((cmd-args (list jolly-brancher-command "-vv")))
    (when repo-path
      (setq cmd-args (append cmd-args (list "--repo" repo-path))))
    (setq cmd-args (append cmd-args (list action)))
    (when args
      (setq cmd-args (append cmd-args (car args))))
    (message "DEBUG: Final cmd-args before quoting: %S" cmd-args)
    (let ((final-cmd (string-join (mapcar #'shell-quote-argument cmd-args) " ")))
      (message "DEBUG: Final command: %S" final-cmd)
      final-cmd)))

;;;###autoload
(transient-define-prefix jolly-brancher-menu ()
  "Show jolly-brancher menu."
  ["Actions"
   ("l" "List tickets" jolly-brancher-mode-list-tickets)
   ("s" "Start branch" jolly-brancher-start-ticket)
   ("e" "End branch" jolly-brancher-end-branch)
   ("c" "Create ticket" jolly-brancher-create-ticket)
   ("t" "Change ticket status" jolly-brancher-mode-change-status)]
  (interactive)
  (transient-setup 'jolly-brancher-menu))

;;;###autoload
(defun jolly-brancher ()
  "Show the jolly-brancher menu."
  (interactive)
  (transient-setup 'jolly-brancher-menu))

;;;###autoload
(global-set-key (kbd "M-j") 'jolly-brancher)

;;;###autoload
(defun jolly-brancher-search-tickets (query)
  "Search tickets with QUERY string."
  (interactive "sSearch tickets: ")
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let* ((jql (format "summary ~ \\\"%s\\\" OR description ~ \\\"%s\\\"" query query))  ; Using double quotes for JQL
             (args (list (concat "--jql=" jql)  ; No need for shell-quote-argument since we're escaping quotes
                         "--created_within=5w"))  ; Add time window to keep results manageable
             (cmd (jolly-brancher--format-command repo-path "list" args)))
        (message "Running search command: %s" cmd)
        (jolly-brancher--display-tickets cmd repo-path))
    (message "Not in a git repository")))

(define-key jolly-brancher-mode-map (kbd "C-c j t") 'jolly-brancher-list-next-up-tickets)
(define-key jolly-brancher-mode-map (kbd "C-c j n") 'jolly-brancher-list-next-up-tickets)
(define-key jolly-brancher-mode-map (kbd "C-c j m") 'jolly-brancher-list-my-tickets)
(define-key jolly-brancher-mode-map (kbd "C-c j u") 'jolly-brancher-list-unassigned-tickets)
(define-key jolly-brancher-mode-map (kbd "C-c j a") 'jolly-brancher-list-all-tickets)

;;;###autoload
(defun jolly-brancher-magit-integration ()
  "Show jolly-brancher tickets buffer from magit."
  (interactive)
  (jolly-brancher-list-next-up-tickets))

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "M-m") 'jolly-brancher-or-magit))

(with-eval-after-load 'jolly-brancher
  (define-key jolly-brancher-tickets-mode-map (kbd "M-m") 'jolly-brancher-or-magit))

;; Global binding for M-m to always work
(global-set-key (kbd "M-m") 'jolly-brancher-or-magit)

(defun jolly-brancher-or-magit ()
  "Start jolly-brancher or show magit status buffer."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (jolly-brancher-list-next-up-tickets)
    (magit-status)))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

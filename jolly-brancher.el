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
  '("Bug" "Story" "Task" "Enhancement" "Feature")
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
  '("To Do" "In Progress" "Backlog" "New" "In Review")
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

(defconst jolly-brancher-tickets-mode-font-lock-keywords
  `(
    ;; Repository path
    ("^Repository: \\(.*\\)$" (1 'jolly-brancher-repo-face))
    ;; Active Query heading
    ("^Active Query:$" . 'jolly-brancher-query-face)
    ;; Ticket numbers (e.g. PD-1234)
    ("[A-Z]+-[0-9]+" . 'jolly-brancher-ticket-face)
    ;; Status text in brackets
    ("\\[\\([^]]+\\)\\]" 1 'jolly-brancher-status-face)))

(defvar jolly-brancher-tickets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jolly-brancher-start-ticket-at-point)
    (define-key map "g" 'jolly-brancher-refresh-tickets)
    (define-key map "q" 'quit-window)
    (define-key map "m" 'jolly-brancher-list-my-tickets)
    (define-key map "u" 'jolly-brancher-list-unassigned-tickets)
    (define-key map "v" 'jolly-brancher-open-ticket-in-browser)
    (define-key map "a" 'jolly-brancher-list-all-tickets)
    (define-key map "?" 'jolly-brancher-tickets-menu)
    map)
  "Keymap for `jolly-brancher-tickets-mode'.")

(define-derived-mode jolly-brancher-tickets-mode special-mode "Jolly-Tickets"
  "Major mode for Jolly Brancher ticket listing."
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local font-lock-defaults '(jolly-brancher-tickets-mode-font-lock-keywords))
  (font-lock-mode 1))

(transient-define-prefix jolly-brancher-tickets-menu ()
  "Show menu for actions in the tickets buffer."
  ["Actions"
   ("RET" "Start branch for ticket" jolly-brancher-start-ticket-at-point)
   ("v" "View ticket in browser" jolly-brancher-open-ticket-in-browser)
   ("g" "Refresh list" jolly-brancher-refresh-tickets)
   ("s" "Change ticket status" jolly-brancher-change-ticket-status)
   ("q" "Quit window" quit-window)]
  ["Filter tickets"
   ("m" "Show my tickets" jolly-brancher-list-my-tickets)
   ("u" "Show unassigned tickets" jolly-brancher-list-unassigned-tickets)
   ("a" "Show all tickets" jolly-brancher-list-all-tickets)])

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

(defun jolly-brancher--get-ticket-at-point ()
  "Get the ticket ID at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([A-Z]+-[0-9]+\\)")
      (match-string-no-properties 1))))

(defun jolly-brancher--list-tickets (current-user no-assignee &optional created-within)
  "List tickets with CURRENT-USER and NO-ASSIGNEE filters.
Optional CREATED-WITHIN specifies time window for created date."
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let* ((args nil)
             (cmd (progn
                    (when created-within
                      (push (concat "--created_within=" created-within) args))
                    (cond
                     (current-user (push "--current-user" args))
                     (no-assignee (push "--no-assignee" args)))
                    (jolly-brancher--format-command repo-path "list" (nreverse args)))))
        (message "Running command: %s" cmd)
        (jolly-brancher--display-tickets cmd repo-path))
    (message "Not in a git repository")))

;;;###autoload
(defun jolly-brancher-list-my-tickets ()
  "List tickets assigned to the current user."
  (interactive)
  (jolly-brancher--list-tickets t nil nil))

;;;###autoload
(defun jolly-brancher-list-unassigned-tickets ()
  "List unassigned tickets."
  (interactive)
  (jolly-brancher--list-tickets nil t "5w"))

;;;###autoload
(defun jolly-brancher-list-all-tickets ()
  "List all tickets."
  (interactive)
  (jolly-brancher--list-tickets nil nil "5w"))

;;;###autoload
(defun jolly-brancher-mode-list-tickets ()
  "Default list tickets command - shows my tickets."
  (interactive)
  (jolly-brancher-list-my-tickets))

(defun jolly-brancher-open-ticket-in-browser ()
  "Open the Jira ticket at point in a web browser."
  (interactive)
  (if-let ((ticket (jolly-brancher--get-ticket-at-point)))
      (browse-url (concat jolly-brancher-jira-url "/browse/" ticket))
    (message "No ticket found at point")))

(defun jolly-brancher-start-ticket-at-point ()
  "Start a branch for the ticket at point."
  (interactive)
  (when-let* ((ticket-id (jolly-brancher--get-ticket-at-point))
              (repo-path (buffer-local-value 'jolly-brancher--current-repo (current-buffer))))
    (let ((cmd (jolly-brancher--format-command repo-path "start" (list "--ticket" ticket-id))))
      (message "Starting branch for ticket %s in %s" ticket-id repo-path)
      (shell-command cmd))))

(defun jolly-brancher-start-ticket (ticket-key)
  "Start work on TICKET-KEY."
  (interactive "sTicket key: ")
  (let ((cmd (jolly-brancher--format-command nil "start" (list "--ticket" ticket-key))))
    (message "Running command: %s" cmd)
    (shell-command cmd)))

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

(defun jolly-brancher-end-branch ()
  "End current branch and create PR."
  (interactive)
  (when (yes-or-no-p "Create PR for current branch? ")
    (let ((cmd (jolly-brancher--format-command nil "end")))
      (message "Running command: %s" cmd)
      (shell-command cmd))))

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

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for Git branch management with Jira integration."
  :lighter " JB"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c j j") 'jolly-brancher-menu)
            (define-key map (kbd "C-c j l") 'jolly-brancher-list-my-tickets)
            (define-key map (kbd "C-c j s") 'jolly-brancher-start-ticket)
            (define-key map (kbd "C-c j e") 'jolly-brancher-end-ticket)
            (define-key map (kbd "C-c j t") 'jolly-brancher-mode-change-status)
            (define-key map (kbd "C-c j c") 'jolly-brancher--maybe-create-from-region)
            map))

;;;###autoload
(transient-define-prefix jolly-brancher-menu ()
  "Show jolly-brancher menu."
  ["Actions"
   ("l" "List tickets" jolly-brancher-mode-list-tickets)
   ("o" "List open tickets" jolly-brancher-list-open-tickets)
   ("s" "Start branch" jolly-brancher-start-ticket)
   ("e" "End branch" jolly-brancher-end-branch)
   ("c" "Create ticket" jolly-brancher-create-ticket)
   ("t" "Change ticket status" jolly-brancher-mode-change-status)]
  (interactive)
  (transient-setup 'jolly-brancher-menu))

;;;###autoload
(defun jolly-brancher-setup ()
  "Setup jolly-brancher."
  (interactive)
  (jolly-brancher-mode 1))

(defun jolly-brancher-create-ticket (&optional default-description)
  "Create a new ticket with optional DEFAULT-DESCRIPTION."
  (interactive)
  (message "DEBUG: Starting create-ticket with initial-description: %s" default-description)
  (let* ((title (read-string "Ticket title: "))
         (description (read-string "Ticket description: "
                                   (when default-description
                                     (jolly-brancher--format-description default-description))))
         (type (completing-read "Ticket type: "
                                jolly-brancher-issue-types
                                nil t nil nil "Bug")))
    (if-let ((repo-path (jolly-brancher--get-repo-root)))
        (let ((cmd (jolly-brancher--format-command
                    repo-path
                    "create-ticket"
                    (list "--title" title
                          "--description" description
                          "--type" type))))
          (message "Running command: %s" cmd)
          (shell-command cmd))
      (message "Not in a git repository"))))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

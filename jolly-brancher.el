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

(defvar-local jolly-brancher--current-repo nil
  "The current repository path for jolly-brancher commands.")

(defun jolly-brancher--format-command (repo-path action &rest args)
  "Format a jolly-brancher command with REPO-PATH, ACTION and ARGS."
  (message "DEBUG: Command args before processing: %S" args)
  (let ((cmd-args (list jolly-brancher-command "-vv")))
    (when repo-path
      (setq cmd-args (append cmd-args (list "--repo" repo-path))))
    (setq cmd-args (append cmd-args (list action)))
    (dolist (arg-pair args)
      (message "DEBUG: Processing arg pair: %S" arg-pair)
      (when (and (consp arg-pair) (car arg-pair) (cdr arg-pair))
        (message "DEBUG: Adding to cmd-args: %S %S" (car arg-pair) (cdr arg-pair))
        (setq cmd-args (append cmd-args (list (car arg-pair) (cdr arg-pair))))))
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

(define-derived-mode jolly-brancher-tickets-mode special-mode "Jolly-Tickets"
  "Major mode for Jolly Brancher ticket listing."
  (setq buffer-read-only t)
  (setq-local line-move-visual t))

(let ((map jolly-brancher-tickets-mode-map))
  (define-key map (kbd "RET") #'jolly-brancher-start-ticket-at-point)
  (define-key map [mouse-1] #'jolly-brancher-start-ticket-at-point)
  (define-key map "q" #'quit-window))

(defun jolly-brancher--display-tickets (command repo-path)
  "Run COMMAND and display results in a tickets buffer with REPO-PATH."
  (let ((buf (get-buffer-create "*Jolly Brancher Tickets*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jolly-brancher-tickets-mode)  ; Set mode first
        (insert "Jolly Brancher Tickets\n\n")
        (insert "Press RET or click on a ticket to start a branch.\n\n")
        (setq-local jolly-brancher--current-repo repo-path)
        (shell-command command t)  ; Insert into current buffer
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun jolly-brancher--get-ticket-at-point ()
  "Get the ticket ID at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([A-Z]+-[0-9]+\\)")
      (match-string-no-properties 1))))

(defun jolly-brancher-list-tickets ()
  "List tickets for the current repository."
  (interactive)
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let ((cmd (jolly-brancher--format-command repo-path "list")))
        (message "Running command: %s" cmd)
        (jolly-brancher--display-tickets cmd repo-path))
    (message "Not in a git repository")))

(defun jolly-brancher-list-open-tickets ()
  "List open tickets for the current repository."
  (interactive)
  (let ((cmd (jolly-brancher--format-command nil "open-tickets")))
    (message "Running command: %s" cmd)
    (jolly-brancher--display-tickets cmd nil)))

(defun jolly-brancher-start-ticket-at-point ()
  "Start a branch for the ticket at point."
  (interactive)
  (when-let* ((ticket-id (jolly-brancher--get-ticket-at-point))
              (repo-path (buffer-local-value 'jolly-brancher--current-repo (current-buffer))))
    (let ((cmd (jolly-brancher--format-command repo-path "start" (cons "--ticket" ticket-id))))
      (message "Starting branch for ticket %s in %s" ticket-id repo-path)
      (shell-command cmd))
    t))

(defun jolly-brancher-start-ticket (ticket-key)
  "Start work on TICKET-KEY."
  (interactive "sTicket key: ")
  (let ((cmd (jolly-brancher--format-command nil "start" (cons "--ticket" ticket-key))))
    (message "Running command: %s" cmd)
    (shell-command cmd)))

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

;;;###autoload
(transient-define-prefix jolly-brancher-menu ()
  "Show jolly-brancher menu."
  ["Actions"
   ("l" "List tickets" jolly-brancher-list-tickets)
   ("o" "List open tickets" jolly-brancher-list-open-tickets)
   ("s" "Start branch" jolly-brancher-start-ticket)
   ("e" "End branch" jolly-brancher-end-branch)
   ("c" "Create ticket" jolly-brancher-create-ticket)]
  (interactive)
  (transient-setup 'jolly-brancher-menu))

(defun jolly-brancher-create-ticket (&optional initial-description)
  "Create a new bug ticket.
If INITIAL-DESCRIPTION is provided or region is active, use it as the default description."
  (interactive)
  (message "DEBUG: Starting create-ticket with initial-description: %S" initial-description)
  (let* ((default-description (or initial-description
                                 (when (use-region-p)
                                   (buffer-substring-no-properties
                                    (region-beginning)
                                    (region-end)))))
         (title (read-string "Ticket title: "))
         (description (read-string "Ticket description: "
                                   (when default-description
                                     (jolly-brancher--format-description default-description))))
         (type (completing-read "Issue type: " jolly-brancher-issue-types
                              nil t nil nil "Bug")))
    (message "DEBUG: Creating ticket with title: %S description: %S type: %S" title description type)
    (let ((cmd (jolly-brancher--format-command
                nil
                "create-ticket"
                (cons "--title" title)
                (cons "--description" description)
                (cons "--type" type))))
      (message "DEBUG: Final command string: %S" cmd)
      (shell-command cmd))))

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for jolly-brancher integration."
  :lighter " JB"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c j j") 'jolly-brancher-menu)
            (define-key map (kbd "C-c j l") 'jolly-brancher-list-tickets)
            (define-key map (kbd "C-c j o") 'jolly-brancher-list-open-tickets)
            (define-key map (kbd "C-c j s") 'jolly-brancher-start-ticket)
            (define-key map (kbd "C-c j e") 'jolly-brancher-end-branch)
            (define-key map (kbd "C-c j c") 'jolly-brancher--maybe-create-from-region)
            map))

;;;###autoload
(defun jolly-brancher-setup ()
  "Setup jolly-brancher."
  (interactive)
  (jolly-brancher-mode 1))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

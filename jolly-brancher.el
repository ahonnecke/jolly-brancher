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

(defvar-local jolly-brancher--current-repo nil
  "The current repository path for jolly-brancher commands.")

(defvar jolly-brancher--ticket-history nil
  "History of ticket IDs used.")

(defun jolly-brancher--get-repo-root ()
  "Get the root directory of the current project."
  (or (when-let ((project (project-current)))
        (project-root project))
      (error "Not in a project")))

(defun jolly-brancher--parse-ticket-list (output)
  "Parse OUTPUT from jolly-brancher list command into a list of tickets."
  (let ((lines (split-string output "\n" t)))
    (cl-remove-if #'null
                  (mapcar (lambda (line)
                            (when (string-match "^\\([A-Z]+-[0-9]+\\)\\s-+\\(.*\\)$" line)
                              (list :id (match-string 1 line)
                                    :summary (match-string 2 line))))
                          lines))))

(defun jolly-brancher--format-ticket (ticket)
  "Format a TICKET for display."
  (format "%-12s  %-50s"
          (propertize (plist-get ticket :id)
                      'face 'font-lock-keyword-face)
          (plist-get ticket :summary)))

(defun jolly-brancher--get-ticket-at-point ()
  "Get the ticket ID at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([A-Z]+-[0-9]+\\)")
      (match-string-no-properties 1))))

(defun jolly-brancher--run-command (cmd)
  "Run CMD and return its output, handling errors appropriately."
  (with-temp-buffer
    (message "Running command: %s" cmd)
    (let ((exit-code (call-process-shell-command cmd nil t)))
      (let ((output (buffer-string)))
        (if (= exit-code 0)
            output
          (message "Command failed with output: %s" output)
          nil)))))

(defun jolly-brancher-list-tickets ()
  "Display tickets in a new buffer with actions."
  (interactive)
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (command (format "%s --repo %s list"
                         jolly-brancher-command default-directory))
         (output (jolly-brancher--run-command command)))
    (when output
      (setq-local jolly-brancher--current-repo default-directory)
      (let* ((tickets (jolly-brancher--parse-ticket-list output))
             (buf (get-buffer-create "*Jolly Brancher Tickets*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (jolly-brancher-tickets-mode)
            (setq-local jolly-brancher--current-repo default-directory)
            (insert "Jolly Brancher Tickets\n\n")
            (insert "Press RET or click on a ticket to show actions.\n\n")
            (if tickets
                (dolist (ticket tickets)
                  (when ticket
                    (insert (jolly-brancher--format-ticket ticket) "\n")))
              (insert "No tickets found.\n")))
          (goto-char (point-min)))
        (switch-to-buffer buf)))))

(defun jolly-brancher-start-ticket-branch ()
  "Start a branch for the ticket at point."
  (interactive)
  (let ((ticket-id (jolly-brancher--get-ticket-at-point))
        (repo-path (buffer-local-value 'jolly-brancher--current-repo (current-buffer))))
    (if (and ticket-id repo-path)
        (let* ((command (format "%s --repo %s start --ticket %s"
                              jolly-brancher-command
                              repo-path
                              ticket-id))
               (output (jolly-brancher--run-command command)))
          (if output
              (message "Started branch for ticket %s" ticket-id)
            (message "Failed to start branch. Check the Messages buffer for details.")))
      (message "No ticket at point or repository not set"))))

(defun jolly-brancher-view-ticket ()
  "Open the current ticket in browser."
  (interactive)
  (when-let ((ticket-id (jolly-brancher--get-ticket-at-point)))
    (browse-url (format "https://cirrusv2x.atlassian.net/browse/%s" ticket-id))))

(defun jolly-brancher-copy-ticket-id ()
  "Copy the current ticket ID to kill ring."
  (interactive)
  (when-let ((ticket-id (jolly-brancher--get-ticket-at-point)))
    (kill-new ticket-id)
    (message "Copied %s to clipboard" ticket-id)))

;;;###autoload
(defun jolly-brancher-start-branch (ticket)
  "Start a new branch for TICKET."
  (interactive
   (list (read-string "Ticket ID: " nil 'jolly-brancher--ticket-history)))
  (when (string-empty-p ticket)
    (user-error "Ticket ID is required"))
  (let ((output (jolly-brancher--run "start" "--ticket" ticket)))
    (message "Started branch: %s" (string-trim output))))

;;;###autoload
(defun jolly-brancher-end-branch ()
  "End current branch and create PR."
  (interactive)
  (when (yes-or-no-p "Create PR for current branch? ")
    (let ((output (jolly-brancher--run "end")))
      (message "Created PR: %s" (string-trim output)))))

;;;###autoload
(defun jolly-brancher ()
  "Git branch management with Jira integration."
  (interactive)
  (transient-setup 'jolly-brancher))

(transient-define-prefix jolly-brancher ()
  "Git branch management with Jira integration."
  :value '("--parent" "dev")
  ["Arguments"
   ("-p" "Parent branch" "--parent=")]
  ["Actions"
   ("l" "List tickets" jolly-brancher-list-tickets)
   ("o" "List open tickets" jolly-brancher-list-open-tickets)
   ("s" "Start branch" jolly-brancher-start-branch)
   ("e" "End branch (create PR)" jolly-brancher-end-branch)])

;;;###autoload
(defun jolly-brancher-list-open-tickets ()
  "Display currently open tickets in a new buffer with actions."
  (interactive)
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (command (format "%s --repo %s open-tickets"
                         jolly-brancher-command default-directory))
         (output (jolly-brancher--run-command command)))
    (if output
        (let* ((tickets (jolly-brancher--parse-ticket-list output))
               (buf (get-buffer-create "*Jolly Brancher Open Tickets*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (jolly-brancher-tickets-mode)
              (insert "Open Jolly Brancher Tickets\n\n")
              (insert "Press RET or click on a ticket to show actions.\n\n")
              (if tickets
                  (dolist (ticket tickets)
                    (when ticket
                      (insert (jolly-brancher--format-ticket ticket) "\n")))
                (insert "No open tickets found.\n")))
            (goto-char (point-min)))
          (switch-to-buffer buf))
      (message "Failed to get open tickets. Check the Messages buffer for details."))))

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for jolly-brancher key bindings."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c j j") 'jolly-brancher)
            (define-key map (kbd "C-c j l") 'jolly-brancher-list-tickets)
            (define-key map (kbd "C-c j o") 'jolly-brancher-list-open-tickets)
            (define-key map (kbd "C-c j s") 'jolly-brancher-start-branch)
            (define-key map (kbd "C-c j e") 'jolly-brancher-end-branch)
            map))

;;;###autoload
(defun jolly-brancher-setup ()
  "Setup jolly-brancher."
  (interactive)
  (jolly-brancher-mode 1))

(define-derived-mode jolly-brancher-tickets-mode special-mode "Jolly-Tickets"
  "Major mode for Jolly Brancher ticket listing."
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  ;; Custom keymap for the tickets buffer
  (let ((map jolly-brancher-tickets-mode-map))
    (define-key map (kbd "RET") #'jolly-brancher-ticket-action-menu)
    (define-key map (kbd "s") #'jolly-brancher-start-ticket-branch)
    (define-key map (kbd "q") #'quit-window)
    (define-key map [mouse-1] #'jolly-brancher-ticket-action-menu)))

(transient-define-prefix jolly-brancher-ticket-action-menu ()
  "Actions for the current ticket."
  [:description
   (lambda ()
     (let ((ticket-id (jolly-brancher--get-ticket-at-point)))
       (format "Actions for ticket %s" (or ticket-id "unknown"))))

   ["Actions"
    ("s" "Start branch for this ticket" jolly-brancher-start-ticket-branch)
    ("v" "View in browser" jolly-brancher-view-ticket)
    ("c" "Copy ticket ID" jolly-brancher-copy-ticket-id)]]
  (interactive)
  (if (jolly-brancher--get-ticket-at-point)
      (transient-setup 'jolly-brancher-ticket-action-menu)
    (message "No ticket at point")))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

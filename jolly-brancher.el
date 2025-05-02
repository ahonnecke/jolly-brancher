;;; jolly-brancher.el --- Git branch management with Jira integration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ashton Von Honnecke

;; Author: Ashton Von Honnecke
;; Keywords: vc, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (project "0.9.8") (magit "3.4.0"))
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
;;   M-m - Toggle between jolly-brancher and magit
;;   M-j or C-c j j - Open jolly-brancher menu
;;   C-c j l - List tickets
;;   C-c j s - Start a branch
;;   C-c j e - End branch and create PR

;;; Code:

(require 'magit)
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

(defcustom jolly-brancher-status-options
  '("To Do" "In Progress" "Backlog" "New" "In Review" "Blocked" "QA" "Staged" "Done")
  "List of available status options for tickets."
  :type '(repeat string)
  :group 'jolly-brancher)

(defcustom jolly-brancher-jql-templates
  '((my-tickets . "project = PD AND assignee = currentUser() AND status in (%s)")
    (unassigned . "project = PD AND assignee is EMPTY AND status in (%s)")
    (next-up . "project = PD AND assignee = currentUser() AND status in ('In Progress', 'New')")
    (all-tickets . "project = PD AND status in (%s)")
    (search . "project = PD AND (summary ~ \"%s\" OR description ~ \"%s\")"))
  "JQL templates for different ticket views.
Each template can contain %s which will be replaced with appropriate values."
  :type '(alist :key-type symbol :value-type string)
  :group 'jolly-brancher)

(defvar-local jolly-brancher--current-repo nil
  "The current repository path for jolly-brancher commands.")

(defvar-local jolly-brancher--list-command nil
  "Store the command used to generate the ticket list.")

(defvar-local jolly-brancher--list-repo-path nil
  "Store the repository path for the ticket list.")

(defvar-local jolly-brancher--current-jql nil
  "Store the current JQL query for the ticket list.")

(defvar-local jolly-brancher--current-created-within nil
  "Store the current created-within filter value.")

(defun jolly-brancher--format-status-list ()
  "Format status list for JQL."
  (mapconcat (lambda (s) (format "'%s'" s))
             jolly-brancher-status-options
             ", "))

(defun jolly-brancher--construct-jql (type &optional created-within query)
  "Construct JQL query for TYPE of ticket view.
Optional CREATED-WITHIN adds time filter (e.g. \"5w\" for 5 weeks).
Optional QUERY is used for search type queries."
  (let* ((template (alist-get type jolly-brancher-jql-templates))
         (jql (cond
               ;; If it's a search and the query looks like a ticket ID (e.g., "PD-1316")
               ((and (eq type 'search) query (string-match-p "^[A-Z]+-[0-9]+$" query))
                (format "key = %s" query))
               
               ;; If it's a search and the query looks like just a number (e.g., "1316")
               ((and (eq type 'search) query (string-match-p "^[0-9]+$" query))
                (format "key = PD-%s" query))
               
               ;; For regular search queries
               ((eq type 'search)
                (format template query query))  ; For search, use query in both summary and description
               
               ;; For other types
               (t
                (format template (jolly-brancher--format-status-list))))))
    (if created-within
        (concat jql " AND created >= -" created-within)
      jql)))

(defun jolly-brancher--modify-jql-status (status jql)
  "Modify JQL query to change status filter to STATUS in JQL."
  (if (string-match "status\\s-+in\\s-+(\\([^)]+\\))" jql)
      (replace-match (format "'%s'" status) t t jql 1)
    (concat jql " AND status = '" status "'")))

(defun jolly-brancher--modify-jql-assignee (assignee jql)
  "Modify JQL query to change assignee in JQL."
  (let ((new-assignee (cond
                       ((string= assignee "currentUser") "currentUser()")
                       ((string= assignee "unassigned") "EMPTY")
                       (t (format "'%s'" assignee)))))
    (if (string-match "assignee\\s-*=\\s-*\\([^\\s-]+\\)" jql)
        (replace-match new-assignee t t jql 1)
      (if (string-match "assignee\\s-+is\\s-+\\([^\\s-]+\\)" jql)
          (replace-match (format "= %s" new-assignee) t t jql 0)
        (concat jql " AND assignee = " new-assignee)))))

(defun jolly-brancher--modify-jql-created (weeks-offset jql)
  "Modify JQL query to adjust created date by WEEKS-OFFSET weeks in JQL."
  (if (string-match "created\\s-+>=\\s-+-\\([0-9]+\\)w" jql)
      (let ((current-weeks (string-to-number (match-string 1 jql))))
        (replace-match (number-to-string (+ current-weeks weeks-offset)) t t jql 1))
    (concat jql " AND created >= -" (number-to-string (abs weeks-offset)) "w")))

(defun jolly-brancher--refresh-with-jql (jql)
  "Refresh ticket list with new JQL query."
  (when-let ((repo-path jolly-brancher--list-repo-path))
    (let* ((args (list (format "--jql=%s" jql)))
           (cmd (jolly-brancher--format-command repo-path "list" args)))
      (with-current-buffer (get-buffer "*jolly-brancher-tickets*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq jolly-brancher--current-jql jql)
          (insert (shell-command-to-string cmd))
          (goto-char (point-min)))))))

(defun jolly-brancher--list-tickets (type &optional created-within query)
  "List tickets based on TYPE with optional CREATED-WITHIN filter and search QUERY."
  (if-let ((repo-path (jolly-brancher--get-repo-root)))
      (let* ((jql (jolly-brancher--construct-jql type created-within query))
             (args (list (format "--jql=%s" jql)))
             (cmd (jolly-brancher--format-command repo-path "list" args)))
        (with-current-buffer (get-buffer-create "*jolly-brancher-tickets*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (jolly-brancher-tickets-mode)
            (setq-local jolly-brancher--list-command cmd
                       jolly-brancher--list-repo-path repo-path
                       jolly-brancher--current-repo repo-path
                       jolly-brancher--current-jql jql
                       jolly-brancher--current-created-within created-within)
            (insert (shell-command-to-string cmd))
            (goto-char (point-min)))
          (pop-to-buffer (current-buffer))))
    (message "Not in a git repository")))

(transient-define-prefix jolly-brancher-dispatch ()
  "Show popup menu for jolly-brancher commands."
  ["Jolly Brancher Commands"
   ["Tickets"
    ("l" "List my tickets" jolly-brancher-list-my-tickets)
    ("u" "List unassigned tickets" jolly-brancher-list-unassigned-tickets)
    ("a" "List all tickets" jolly-brancher-list-all-tickets)
    ("n" "List next-up tickets" jolly-brancher-list-next-up-tickets)
    ("/" "Search tickets" jolly-brancher-search-tickets)
    ("f" "Filter current view" jolly-brancher-filter-menu)]
   ["Actions"
    ("s" "Start work on ticket" jolly-brancher-start)
    ("e" "End work and create PR" jolly-brancher-end)
    ("c" "Create new ticket" jolly-brancher-create-ticket)
    ("t" "Set ticket status" jolly-brancher-set-status)]
   ["Navigation"
    ("m" "Toggle Magit/Jolly" jolly-brancher-toggle-magit)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix jolly-brancher-tickets-menu ()
  "Show menu for actions in the tickets buffer."
  ["Jolly Brancher Tickets"
   ["Actions"
    ("RET" "Start branch for ticket" jolly-brancher-start-ticket-at-point)
    ("v" "View ticket in browser" jolly-brancher-open-ticket-in-browser)
    ("g" "Refresh list" jolly-brancher-refresh-tickets)
    ("s" "Change ticket status" jolly-brancher-change-ticket-status)
    ("e" "End work and create PR" jolly-brancher-end-ticket)
    ("q" "Quit window" quit-window)]
   ["Filter Tickets"
    ("m" "Show my tickets" jolly-brancher-list-my-tickets)
    ("n" "Show next-up tickets" jolly-brancher-list-next-up-tickets)
    ("u" "Show unassigned tickets" jolly-brancher-list-unassigned-tickets)
    ("a" "Show all tickets" jolly-brancher-list-all-tickets)
    ("/" "Search tickets" jolly-brancher-search-tickets)
    ("f" "Filter current view" jolly-brancher-filter-menu)]])

(defun jolly-brancher-list-my-tickets ()
  "List tickets assigned to the current user."
  (interactive)
  (jolly-brancher--list-tickets 'my-tickets "5w"))

(defun jolly-brancher-list-unassigned-tickets ()
  "List unassigned tickets."
  (interactive)
  (jolly-brancher--list-tickets 'unassigned "5w"))

(defun jolly-brancher-list-all-tickets ()
  "List all tickets."
  (interactive)
  (jolly-brancher--list-tickets 'all-tickets "5w"))

(defun jolly-brancher-list-next-up-tickets ()
  "List tickets that are In Progress or New, assigned to current user, in PD project."
  (interactive)
  (jolly-brancher--list-tickets 'next-up))

(defun jolly-brancher-search-tickets (query)
  "Search tickets with QUERY string."
  (interactive "sSearch tickets: ")
  (jolly-brancher--list-tickets 'search "5w" query))

;;;###autoload
(defun jolly-brancher ()
  "Start jolly-brancher."
  (interactive)
  (jolly-brancher-list-next-up-tickets))

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

(defun jolly-brancher--display-tickets (cmd repo-path)
  "Display tickets using CMD in a buffer for REPO-PATH."
  (let ((buffer (get-buffer-create "*jolly-brancher-tickets*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jolly-brancher-tickets-mode)
        (setq-local jolly-brancher--list-command cmd
                    jolly-brancher--list-repo-path repo-path
                    jolly-brancher--current-repo repo-path
                    jolly-brancher--current-jql nil
                    jolly-brancher--current-created-within nil)
        (insert (shell-command-to-string cmd))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

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
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "m") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "n") 'jolly-brancher-list-next-up-tickets)
    (define-key map (kbd "u") 'jolly-brancher-list-unassigned-tickets)
    (define-key map (kbd "a") 'jolly-brancher-list-all-tickets)
    (define-key map (kbd "/") 'jolly-brancher-search-tickets)
    (define-key map (kbd "?") 'jolly-brancher-tickets-menu)
    (define-key map (kbd "e") 'jolly-brancher-end-ticket)
    (define-key map (kbd "f") 'jolly-brancher-filter-menu)
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
    (define-key map (kbd "f") 'jolly-brancher-filter-menu)
    map)
  "Keymap for `jolly-brancher-tickets-mode'.")

(defvar jolly-brancher-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Global prefix key bindings
    (define-key map (kbd "C-c j j") 'jolly-brancher-dispatch)
    (define-key map (kbd "C-c j l") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "C-c j s") 'jolly-brancher-start)
    (define-key map (kbd "C-c j e") 'jolly-brancher-end)
    (define-key map (kbd "C-c j t") 'jolly-brancher-set-status)
    (define-key map (kbd "C-c j c") 'jolly-brancher-create-ticket)
    ;; Quick access keys
    (define-key map (kbd "M-j") 'jolly-brancher-dispatch)
    (define-key map (kbd "M-m") 'jolly-brancher-toggle-magit)
    ;; Single key bindings in jolly-brancher buffers
    (define-key map (kbd "j") 'jolly-brancher-dispatch)
    (define-key map (kbd "l") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "s") 'jolly-brancher-start)
    (define-key map (kbd "e") 'jolly-brancher-end)
    (define-key map (kbd "t") 'jolly-brancher-set-status)
    (define-key map (kbd "c") 'jolly-brancher-create-ticket)
    map)
  "Keymap for `jolly-brancher-mode'.")

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for Git branch management with Jira integration.
\\{jolly-brancher-mode-map}

Global Commands:
\\[jolly-brancher-toggle-magit] - Toggle between jolly-brancher/magit
\\[jolly-brancher-dispatch] - Open jolly-brancher menu
\\[jolly-brancher-list-my-tickets] - List tickets
\\[jolly-brancher-start] - Start branch
\\[jolly-brancher-end] - End branch and create PR"
  :lighter " Jolly"
  :keymap jolly-brancher-mode-map
  :global t
  (if jolly-brancher-mode
      (message "Jolly Brancher mode enabled. Press M-j or C-c j j for commands")
    (message "Jolly Brancher mode disabled")))

(defun jolly-brancher-filter-status ()
  "Change status filter in current JQL query."
  (interactive)
  (if (not jolly-brancher--current-jql)
      (message "No active ticket list to filter")
    (let ((status (completing-read "Status: " jolly-brancher-status-options nil t)))
      (jolly-brancher--refresh-with-jql
       (jolly-brancher--modify-jql-status status jolly-brancher--current-jql)))))

(defun jolly-brancher-filter-assignee ()
  "Change assignee filter in current JQL query."
  (interactive)
  (if (not jolly-brancher--current-jql)
      (message "No active ticket list to filter")
    (let ((assignee (completing-read "Assignee: "
                                   '("currentUser" "unassigned" "someone else")
                                   nil t)))
      (when (string= assignee "someone else")
        (setq assignee (read-string "Enter assignee name: ")))
      (jolly-brancher--refresh-with-jql
       (jolly-brancher--modify-jql-assignee assignee jolly-brancher--current-jql)))))

(defun jolly-brancher-filter-older ()
  "Make created date filter one week older."
  (interactive)
  (if (not jolly-brancher--current-jql)
      (message "No active ticket list to filter")
    (jolly-brancher--refresh-with-jql
     (jolly-brancher--modify-jql-created 1 jolly-brancher--current-jql))))

(defun jolly-brancher-filter-newer ()
  "Make created date filter one week newer."
  (interactive)
  (if (not jolly-brancher--current-jql)
      (message "No active ticket list to filter")
    (jolly-brancher--refresh-with-jql
     (jolly-brancher--modify-jql-created -1 jolly-brancher--current-jql))))

(transient-define-prefix jolly-brancher-filter-menu ()
  "Show menu for filtering the current ticket list."
  :value '()
  ["Filter Current View"
   ["Change Filters"
    ("s" "Status" jolly-brancher-filter-status)
    ("a" "Assignee" jolly-brancher-filter-assignee)]
   ["Date Range"
    ("o" "One week older" jolly-brancher-filter-older)
    ("n" "One week newer" jolly-brancher-filter-newer)]
   ["Actions"
    ("g" "Refresh" jolly-brancher-refresh-tickets)
    ("q" "Quit" transient-quit-one)]])

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

(defun jolly-brancher--highlight-ticket ()
  "Highlight the current ticket line."
  (when (eq major-mode 'jolly-brancher-tickets-mode)
    (let ((ticket (jolly-brancher--get-ticket-at-point)))
      (when ticket
        (message "Current ticket: %s" ticket)))))

;;;###autoload
(defun jolly-brancher ()
  "Show the jolly-brancher menu."
  (interactive)
  (transient-setup 'jolly-brancher-menu))

;;;###autoload
(global-set-key (kbd "M-j") 'jolly-brancher)
(global-set-key (kbd "C-c j j") 'jolly-brancher)

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
      (jolly-brancher--switch-to-tickets)
    (magit-status)))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

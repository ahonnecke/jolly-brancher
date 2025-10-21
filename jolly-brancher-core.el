;;; jolly-brancher-core.el --- Core functionality for jolly-brancher -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains the core functionality for jolly-brancher,
;; including customization variables and main commands.

;;; Code:

(require 'transient)
(require 'jolly-brancher-utils)

;; Customization variables

(defgroup jolly-brancher nil
  "Git branch management with Jira integration."
  :group 'tools
  :prefix "jolly-brancher-")

(defcustom jolly-brancher-issue-types
  '("Bug" "Story" "Task" "Spike" "Epic" "Subtask" )
  "List of available issue types."
  :type '(repeat string)
  :group 'jolly-brancher)

(defcustom jolly-brancher-jira-url "https://errasoft.atlassian.net"
  "Base URL for Jira instance."
  :type 'string
  :group 'jolly-brancher)

(defcustom jolly-brancher-status-options
  '("To Do" "In Progress" "Backlog" "New" "In Review" "Blocked" "QA" "Staged" "Done")
  "List of available status options for tickets."
  :type '(repeat string)
  :group 'jolly-brancher)

;; Local variables
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

;; Core transient menus

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
    ("t" "Set ticket status" jolly-brancher-set-status)
    ("y" "Set ticket type" jolly-brancher-change-ticket-type)]
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
    ("y" "Change ticket type" jolly-brancher-change-ticket-type)
    ("e" "End work and create PR" jolly-brancher-end-ticket)
    ("q" "Quit window" quit-window)]
   ["Filter Tickets"
    ("m" "Show my tickets" jolly-brancher-list-my-tickets)
    ("n" "Show next-up tickets" jolly-brancher-list-next-up-tickets)
    ("u" "Show unassigned tickets" jolly-brancher-list-unassigned-tickets)
    ("a" "Show all tickets" jolly-brancher-list-all-tickets)
    ("/" "Search tickets" jolly-brancher-search-tickets)
    ("f" "Filter current view" jolly-brancher-filter-menu)]])

;; Core ticket listing functions

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
  "List tickets that are In Progress or New, assigned to current user."
  (interactive)
  (jolly-brancher--list-tickets 'next-up))

(defun jolly-brancher-search-tickets (query)
  "Search tickets with QUERY string."
  (interactive "sSearch tickets: ")
  (jolly-brancher--list-tickets 'search "5w" query))

;; Core ticket action functions

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

(defun jolly-brancher-set-type ()
  "Set the type of the current branch's ticket."
  (interactive)
  (let* ((branch-name (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
         (ticket-match (and branch-name (string-match "\\([A-Z]+-[0-9]+\\)" branch-name)))
         (ticket-key (if ticket-match (match-string 1 branch-name)
                       (read-string "Ticket key: ")))
         (issue-types '("Epic" "Story" "Task" "Bug" "Spike" "Subtask" "Incident" "Tech Debt"))
         (type (completing-read "New type: " issue-types nil t)))
    (jolly-brancher-set-ticket-type ticket-key type)))

(defun jolly-brancher-set-ticket-type (ticket-key type)
  "Set the type of TICKET-KEY to TYPE."
  (let ((cmd (jolly-brancher--format-command nil "set-type" (list "--ticket" ticket-key "--issue-type" type))))
    (message "Setting type of %s to %s..." ticket-key type)
    (shell-command cmd)))

(defun jolly-brancher-change-ticket-type ()
  "Change the type of a ticket."
  (interactive)
  (let* ((ticket (jolly-brancher--get-ticket-at-point))
         (issue-types '("Epic" "Story" "Task" "Bug" "Spike" "Subtask" "Incident" "Tech Debt"))
         (type (completing-read "New type: " issue-types nil t)))
    (if ticket
        (jolly-brancher-set-ticket-type ticket type)
      (let ((ticket-key (read-string "Ticket key: ")))
        (jolly-brancher-set-ticket-type ticket-key type)))))

(defun jolly-brancher-mode-change-type ()
  "Default change type command - changes type of current ticket."
  (interactive)
  (jolly-brancher-change-ticket-type))

(defun jolly-brancher--switch-to-tickets ()
  "Switch to the jolly-brancher tickets buffer if it exists, otherwise create it."
  (let ((buffer (get-buffer "*jolly-brancher-tickets*")))
    (if buffer
        (switch-to-buffer buffer)
      (jolly-brancher-list-next-up-tickets))))

;; Define keymap for the mode
(defvar jolly-brancher-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Global prefix key bindings
    (define-key map (kbd "C-c j j") 'jolly-brancher-dispatch)
    (define-key map (kbd "C-c j l") 'jolly-brancher-list-my-tickets)
    (define-key map (kbd "C-c j s") 'jolly-brancher-start)
    (define-key map (kbd "C-c j e") 'jolly-brancher-end)
    (define-key map (kbd "C-c j t") 'jolly-brancher-set-status)
    (define-key map (kbd "C-c j y") 'jolly-brancher-change-ticket-type)
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
    (define-key map (kbd "y") 'jolly-brancher-change-ticket-type)
    (define-key map (kbd "c") 'jolly-brancher-create-ticket)
    map)
  "Keymap for `jolly-brancher-mode'.")

(provide 'jolly-brancher-core)

;;; jolly-brancher-core.el ends here

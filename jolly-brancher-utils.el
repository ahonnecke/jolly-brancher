;;; jolly-brancher-utils.el --- Utility functions for jolly-brancher -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains utility functions for the jolly-brancher package.

;;; Code:

(require 'project)

;; Customization variables
(defcustom jolly-brancher-command "/home/ahonnecke/.pyenv/shims/jolly-brancher"
  "Command to run jolly-brancher."
  :type 'string
  :group 'jolly-brancher)

;; Utility functions

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

(provide 'jolly-brancher-utils)

;;; jolly-brancher-utils.el ends here
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
;;   C-c o j - Open jolly-brancher menu
;;   C-c o l - List tickets
;;   C-c o s - Start a branch
;;   C-c o e - End branch and create PR

;;; Code:

(require 'transient)
(require 'project)

(defgroup jolly-brancher nil
  "Git branch management with Jira integration."
  :group 'tools
  :prefix "jolly-brancher-")

(defcustom jolly-brancher-command "jolly-brancher"
  "Command to run jolly-brancher."
  :type 'string
  :group 'jolly-brancher)

(defvar jolly-brancher--ticket-history nil
  "History of ticket IDs used.")

(defun jolly-brancher--get-repo-root ()
  "Get the root directory of the current project."
  (or (when-let ((project (project-current)))
        (project-root project))
      (error "Not in a project")))

(defun jolly-brancher--parse-ticket-list (output)
  "Parse OUTPUT from jolly-brancher list command into alist."
  (mapcar (lambda (line)
            (when (string-match "\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)|\\([^|]+\\)" line)
              (list (match-string 1 line)
                    :summary (match-string 2 line)
                    :type (match-string 3 line)
                    :status (match-string 4 line))))
          (split-string output "\n" t)))

(defun jolly-brancher--run (action &rest args)
  "Run jolly-brancher with ACTION and ARGS."
  (let* ((default-directory (jolly-brancher--get-repo-root))
         (buffer (get-buffer-create "*jolly-brancher*"))
         (command (append (list jolly-brancher-command action "--repo" default-directory)
                        args)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message "Running: %s" (string-join command " "))
        (let ((exit-code (apply #'call-process
                               (car command)
                               nil (current-buffer) t
                               (cdr command))))
          (if (zerop exit-code)
              (buffer-string)
            (display-buffer buffer)
            (error "Jolly-brancher failed with exit code %d" exit-code)))))))

;;;###autoload
(defun jolly-brancher-list-tickets ()
  "List all in-progress tickets."
  (interactive)
  (let* ((output (jolly-brancher--run "list"))
         (tickets (jolly-brancher--parse-ticket-list output))
         (buf (get-buffer-create "*Jolly Brancher Tickets*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Jolly Brancher Tickets\n\n")
        (insert (format "%-12s  %-50s  %-10s  %s\n"
                       "Ticket" "Summary" "Type" "Status"))
        (insert (make-string 80 ?-) "\n")
        (dolist (ticket tickets)
          (when ticket  ; Skip nil entries
            (insert (format "%-12s  %-50s  %-10s  %s\n"
                           (car ticket)
                           (truncate-string-to-width
                            (or (plist-get (cdr ticket) :summary) "")
                            50 nil nil "...")
                           (plist-get (cdr ticket) :type)
                           (plist-get (cdr ticket) :status)))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

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
   ("s" "Start branch" jolly-brancher-start-branch)
   ("e" "End branch (create PR)" jolly-brancher-end-branch)])

;;;###autoload
(define-minor-mode jolly-brancher-mode
  "Minor mode for jolly-brancher key bindings."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
           (define-key map (kbd "C-c o j") 'jolly-brancher)
           (define-key map (kbd "C-c o l") 'jolly-brancher-list-tickets)
           (define-key map (kbd "C-c o s") 'jolly-brancher-start-branch)
           (define-key map (kbd "C-c o e") 'jolly-brancher-end-branch)
           map))

;;;###autoload
(defun jolly-brancher-setup ()
  "Setup jolly-brancher."
  (interactive)
  (jolly-brancher-mode 1))

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

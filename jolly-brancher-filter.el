;;; jolly-brancher-filter.el --- Filtering functionality for jolly-brancher -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains functions for filtering ticket lists in the jolly-brancher package.

;;; Code:

(require 'transient)
(require 'jolly-brancher-core)
(require 'jolly-brancher-jql)
(require 'jolly-brancher-utils)

;; Filter functions

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
    ("w" "One week newer" jolly-brancher-filter-newer)]
   ["Actions"
    ("g" "Refresh" jolly-brancher-refresh-tickets)
    ("q" "Quit" transient-quit-one)]])

(provide 'jolly-brancher-filter)

;;; jolly-brancher-filter.el ends here
;;; jolly-brancher-jql.el --- JQL query handling for jolly-brancher -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains functions for constructing and manipulating JQL queries
;; for the jolly-brancher package.

;;; Code:

(require 'jolly-brancher-core)
(require 'jolly-brancher-utils)

(defcustom jolly-brancher-jql-templates
  '((my-tickets . "assignee = currentUser() AND status in (%s)")
    (unassigned . "assignee is EMPTY AND status in (%s)")
    (next-up . "assignee = currentUser() AND status in ('In Progress', 'New')")
    (all-tickets . "status in (%s)")
    (search . "(summary ~ \"%s\" OR description ~ \"%s\")"))
  "JQL templates for different ticket views.
Each template can contain %s which will be replaced with appropriate values.
The project filter is automatically added by the backend based on .jolly.ini configuration."
  :type '(alist :key-type symbol :value-type string)
  :group 'jolly-brancher)

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
               ;; If it's a search and the query looks like a ticket ID (e.g., "SYSMIC-1316")
               ((and (eq type 'search) query (string-match-p "^[A-Z]+-[0-9]+$" query))
                (format "key = %s" query))
               
               ;; If it's a search and the query looks like just a number (e.g., "1316")
               ;; Let the backend handle constructing the full ticket ID with project prefix
               ((and (eq type 'search) query (string-match-p "^[0-9]+$" query))
                query)
               
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

(defun make-jql-older ()
  "Add a week to the created JQL filter."
  (interactive)
  (if (not jolly-brancher--current-jql)
      (message "No active ticket list to filter")
    (let ((current-created-within (or jolly-brancher--current-created-within "5w")))
      ;; Extract the number from the string (e.g., "5w" -> 5)
      (when (string-match "\\([0-9]+\\)w" current-created-within)
        (let ((weeks (string-to-number (match-string 1 current-created-within))))
          ;; Add one week to the current value
          (setq-local jolly-brancher--current-created-within (format "%dw" (+ weeks 1)))
          (jolly-brancher--refresh-with-jql
           (jolly-brancher--modify-jql-created 1 jolly-brancher--current-jql)))))))

(defun make-jql-newer ()
 "Subtract a week from the created JQL filter."
 (interactive)
 (if (not jolly-brancher--current-jql)
     (message "No active ticket list to filter")
   (let ((current-created-within (or jolly-brancher--current-created-within "5w")))
     ;; Extract the number from the string (e.g., "5w" -> 5)
     (when (string-match "\\([0-9]+\\)w" current-created-within)
       (let ((weeks (string-to-number (match-string 1 current-created-within))))
         ;; Subtract one week from the current value, but ensure it's at least 1
         (setq-local jolly-brancher--current-created-within (format "%dw" (max 1 (- weeks 1))))
         (jolly-brancher--refresh-with-jql
          (jolly-brancher--modify-jql-created -1 jolly-brancher--current-jql)))))))

(provide 'jolly-brancher-jql)

;;; jolly-brancher-jql.el ends here
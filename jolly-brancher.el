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

;; Load all jolly-brancher components in dependency order
(require 'jolly-brancher-utils)    ;; Basic utilities and variables
(require 'jolly-brancher-core)     ;; Core functionality
(require 'jolly-brancher-jql)      ;; JQL query handling
(require 'jolly-brancher-ui)       ;; UI components
(require 'jolly-brancher-filter)   ;; Filtering functionality
(require 'jolly-brancher-integration) ;; Integration with other packages

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

;;;###autoload
(defun jolly-brancher-reload ()
  "Reload all jolly-brancher Emacs Lisp files.
This is useful after updating the code or fixing bugs."
  (interactive)
  (message "Reloading jolly-brancher...")
  (unload-feature 'jolly-brancher-integration t)
  (unload-feature 'jolly-brancher-filter t)
  (unload-feature 'jolly-brancher-ui t)
  (unload-feature 'jolly-brancher-jql t)
  (unload-feature 'jolly-brancher-core t)
  (unload-feature 'jolly-brancher-utils t)
  (unload-feature 'jolly-brancher t)
  (require 'jolly-brancher)
  (message "Jolly-brancher reloaded successfully!"))

;;;###autoload
(defun jolly-brancher ()
  "Show the jolly-brancher menu."
  (interactive)
  (transient-setup 'jolly-brancher-menu))

;;;###autoload
(global-set-key (kbd "M-j") 'jolly-brancher)
(global-set-key (kbd "C-c j j") 'jolly-brancher)

;; Global binding for M-m to always work
(global-set-key (kbd "M-m") 'jolly-brancher-or-magit)

(provide 'jolly-brancher)

;;; jolly-brancher.el ends here

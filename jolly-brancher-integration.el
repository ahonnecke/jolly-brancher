;;; jolly-brancher-integration.el --- Integration with other packages -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains functions for integrating jolly-brancher with other packages
;; like Magit.

;;; Code:

(require 'jolly-brancher-core)
(require 'jolly-brancher-utils)
(require 'jolly-brancher-ui)

;; Integration functions

;;;###autoload
(defun jolly-brancher-magit-integration ()
  "Show jolly-brancher tickets buffer from magit."
  (interactive)
  (jolly-brancher-list-next-up-tickets))

(defun jolly-brancher-toggle-magit ()
  "Toggle between jolly-brancher and magit."
  (interactive)
  (jolly-brancher-or-magit))

(defun jolly-brancher-or-magit ()
  "Start jolly-brancher or show magit status buffer."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (jolly-brancher--switch-to-tickets)
    (magit-status)))

;; Set up integration with Magit
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "M-m") 'jolly-brancher-or-magit))

(with-eval-after-load 'jolly-brancher
  (define-key jolly-brancher-tickets-mode-map (kbd "M-m") 'jolly-brancher-or-magit))

(provide 'jolly-brancher-integration)

;;; jolly-brancher-integration.el ends here
;;; init.el -- Example markdown-xwidget installation  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Straight and use-package
;;  ----------------------------------------------------------------------------

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; markdown-xwidget
;;  ----------------------------------------------------------------------------

(use-package markdown-xwidget
  :after markdown-mode
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-github-theme "light-high-contrast")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))

;;; init.el ends here

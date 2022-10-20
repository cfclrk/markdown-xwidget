;;; init.el -- Example markdown-xwidget usage  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Quelpa and use-package
;;  ----------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package use-package-ensure
  :config
  (setq use-package-ensure-function 'quelpa)
  (setq use-package-always-ensure t))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; use-package
(quelpa '(quelpa-use-package
          :fetcher git
          :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)

;;; Editor General
;;  ----------------------------------------------------------------------------

(set-language-environment "UTF-8")

(setq inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; markdown-xwidget

(use-package markdown-xwidget
  :after markdown-mode
  :quelpa (markdown-xwidget
             :fetcher github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-github-theme "light")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "github-dark-dimmed")
  (markdown-xwidget-command "pandoc"))

;;; init.el ends here

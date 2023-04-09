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

;;; markdown-xwidget
;;  ----------------------------------------------------------------------------

(use-package markdown-xwidget
  :after markdown-mode
  :quelpa (markdown-xwidget
             :fetcher github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-github-theme "light-high-contrast")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))

;;; init.el ends here

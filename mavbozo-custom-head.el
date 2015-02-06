(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

;; Install package when not installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; My default package
(defvar my-packages '(ergoemacs-mode clojure-mode clojure-snippets tabbar company magit dash rainbow-delimiters cider markdown-mode php-mode web-mode s)
  "A list of packages to ensure are installed at launch.")

;; Install those default packages if not yet installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/zenburn-emacs")

;; ergoemacs mode
(require 'ergoemacs-mode)
;; (setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;; (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout

;; visual line mode
(global-visual-line-mode 1)

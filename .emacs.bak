;;(server-start)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; set ergoemacs mode
(setq ergoemacs-theme nil)
(setq ergoemacs-keyboard-layout "us")
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; set ido-mode
(ido-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   (quote
    ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" default)))
 '(ergoemacs-mode-used "5.13.12-1")
 '(ergoemacs-theme nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/SpiderOak/Archive/T/todo.txt")
 '(php-mode-coding-style (quote symfony2))
 '(tabbar-background-color "SystemWindowFrame")
 '(tabbar-separator (quote (0.3)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(setq default-directory (getenv "HOME"))


;; delete text under mouse highlight
(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1)

;; copy-cut-paste windows behaviour
(cua-mode 1)

(tabbar-mode 1)

;; auto insert close bracket pairs
(electric-pair-mode 1)


(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-style 'expression) ; highlight entire bracket expression

(global-linum-mode 1) ; display line numbers in margin.

(column-number-mode 1) ;; show column cursor position

;; xahlee's css color mode
;; http://ergoemacs.org/emacs/emacs_CSS_colors.html
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'php-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)

;; php-mode & ergoemacs-mode's keybinding hack
(add-hook 'php-mode-hook
	  (lambda () (global-set-key (kbd "M-f") 'delete-char)))

;; clojure's compojure library
(add-hook 'clojure-mode-hook
	  (lambda () 
	    (define-clojure-indent
	      (defroutes 'defun)
	      (GET 2)
	      (POST 2)
	      (PUT 2)
	      (DELETE 2)
	      (HEAD 2)
	      (ANY 2)
	      (context 2))))

;; enable narrowing to region
(put 'narrow-to-region 'disabled nil)

;;(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20131128.233/dict")
(require 'auto-complete-config)
(ac-config-default)

;;undo-tree-mode keybinding
(require 'undo-tree)
(global-undo-tree-mode)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-S-z") 'redo) 


;;uniquify tab name for file with same name
(require 'uniquify)

;;noctilux theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/noctilux-theme-20140406.2")

(load-theme 'noctilux t)


;;(require 'org)
;; org-mode setup
(setq org-agenda-files (list "~/SpiderOak/Archive/T/todo.txt"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; turn on save place so that when opening a file, the cursor will be at the last position. http://ergoemacs.org/emacs/emacs_save_cursor_position.html
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el") ) ; use standard emacs dir
(setq-default save-place t)


;; special mouse tabbar
(global-set-key [mouse-4] 'tabbar-backward)
(global-set-key [mouse-5] 'tabbar-forward) ;; tabbar.el, put all the buffers on the tabs.

;; Mac style
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(global-set-key (kbd "C-M-j") 'tabbar-backward-tab)
(global-set-key (kbd "C-M-l") 'tabbar-forward-tab)


(defun cider-repl-reset ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(user/reset)")
    (cider-repl-return)))

(global-set-key (kbd "C-c r") 'cider-repl-reset)


(defun cider-repl-refresh ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(clojure.tools.namespace.repl/refresh)")
    (cider-repl-return)))

(global-set-key (kbd "C-c C-r") 'cider-repl-refresh)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
 
;;Remove ^M from clojure repl in windows
;;From http://stackoverflow.com/a/11787550/1393248
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'cider-repl-mode-hook 'remove-dos-eol) ;Remove ^M from clojure repl in windows

;;;TABBAR SETTING

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "SystemActiveTitle"))))
 '(tabbar-selected ((t (:background "SystemButtonHilight" :foreground "gray11"))))
 '(tabbar-unselected ((t (:background "gray" :foreground "SystemWindowFrame")))))

;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
	(if (and (buffer-modified-p (tabbar-tab-value tab))
		 (buffer-file-name (tabbar-tab-value tab)))
	    (concat " + " (concat ad-return-value " "))
	  (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(setq default-directory (getenv "HOME"))  ;; set default directory
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files

(setq initial-buffer-choice "~/SpiderOak/Archive/T/todo.txt")

(setq org-agenda-files (list "~/SpiderOak/Archive/T/todo.txt"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; TABBAR SETTINGS
(global-set-key (kbd "C-M-j") 'tabbar-backward-tab)
(global-set-key (kbd "C-M-l") 'tabbar-forward-tab)

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


;; emacs completion mode
(add-hook 'after-init-hook 'global-company-mode)

;; emacs smart parenthesis
(add-to-list 'load-path "~/.emacs.d/vendor/smartparens")
(load "~/.emacs.d/init-smartparens")

;; clojure mode customizations
(add-hook 'clojure-mode-hook 'subword-mode)

;; rainbow delimiters for all programming mode
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)



;; CIDER setup
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(setq cider-auto-select-error-buffer nil)

(add-to-list 'load-path "~/.emacs.d/vendor/clojure-cheatsheet")
(require 'clojure-cheatsheet)


;; YASNIPPET

(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))


;; CLOJURE SNIPPETS
(require 'clojure-snippets)


;; DATOMIC SNIPPETS
(add-to-list 'load-path "~/.emacs.d/vendor/datomic-snippets")
(require 'datomic-snippets)


;; WEB-MODE
(require 'web-mode) (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; CIDER CUSTOMIZATION
;;; Remove ^M from clojure repl in windows
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'cider-repl-mode-hook 'remove-dos-eol) ;Remove ^M from clojure repl in windows


;;; cider reloaded
(defun cider-repl-reset ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(user/reset)")
    (cider-repl-return)))

;;unset f12 key which is globally bind by ergoemacs-mode
(global-unset-key (kbd "<f12>")) 

(defun set-cider-repl-reset-key ()
  (global-set-key (kbd "<f12>") 'cider-repl-reset))

(add-hook 'cider-repl-mode-hook 'set-cider-repl-reset-key)


(defun cider-repl-refresh ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(refresh)")
    (cider-repl-return)))


(defun set-cider-repl-refresh-key ()
  (global-set-key (kbd "<f10>") 'cider-repl-refresh))

(add-hook 'cider-repl-mode-hook 'set-cider-repl-refresh-key)


(defun cider-repl-refresh-all ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(refresh-all)")
    (cider-repl-return)))


(defun set-cider-repl-refresh-all-key ()
  (global-set-key (kbd "<f11>") 'cider-repl-refresh-all))

(add-hook 'cider-repl-mode-hook 'set-cider-repl-refresh-all-key)



(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-sexp-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-find-or-create-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))


(defun set-cider-eval-expression-at-point-in-repl ()
  (global-set-key (kbd "<f9>") 'cider-eval-expression-at-point-in-repl))

(add-hook 'cider-repl-mode-hook 'set-cider-eval-expression-at-point-in-repl)


;; UNIQUIFY
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")


;; ERLANG

(setq load-path (cons  (concat (getenv "ERLANG_TOOLS_PATH") "\\emacs")
			load-path))
(setq erlang-root-dir (getenv "ERLANG_PATH"))
(setq exec-path (cons (concat (getenv "ERLANG_PATH") "\\bin") exec-path))
(require 'erlang-start)

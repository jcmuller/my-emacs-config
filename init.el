;;; init.el --- Configuration
;;; Commentary:
;;; Code:

;; setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; package definition and  configuration
(use-package ace-window :ensure t)
(use-package auto-complete :ensure t :delight)
(use-package browse-at-remote :ensure t :bind (("C-c g g" . #'browse-at-remote)))
(use-package bundler :ensure t :hook (ruby-mode enh-ruby-mode))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :bind ([f5] . #'toggle-theme)
  :config
  (defun toggle-theme ()
    "Cycle font sizes."
    (interactive)
    (if (get 'toggle-theme 'state)
	(progn
	  (color-theme-sanityinc-tomorrow-day)
	  (put 'toggle-theme 'state nil))
      (progn
	(color-theme-sanityinc-tomorrow-night)
	(put 'toggle-theme 'state t))))
  (load-theme 'sanityinc-tomorrow-day))

(use-package dockerfile-mode :ensure t)
(use-package editorconfig-custom-majormode :ensure t)
(use-package editorconfig :ensure t)

(use-package enh-ruby-mode
  :ensure t
  :config (add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook)
  :custom
  (enh-ruby-bounce-deep-indent nil)
  (enh-ruby-deep-indent-construct nil)
  (enh-ruby-deep-indent-paren nil)
  (enh-ruby-hanging-brace-deep-indent-level 2)
  (enh-ruby-hanging-brace-indent-level 2)
  (enh-ruby-hanging-indent-level 2)
  (enh-ruby-hanging-paren-deep-indent-level 2)
  (enh-ruby-hanging-paren-indent-level 2)
  (enh-ruby-indent-level 2))

(use-package evil
  :ensure t
  :custom (evil-shift-width 2)
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
    (define-key evil-normal-state-map (kbd "SPC f") #'neotree-find)
    (define-key evil-normal-state-map (kbd "SPC SPC") #'neotree-toggle)
    (define-key evil-window-map (kbd "]") #'xref-find-definitions-other-window)

    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter))

  (evil-ex-define-cmd "Gblame" #'magit-blame-popup)
  (evil-ex-define-cmd "Gbrowse" #'browse-at-remote)
  (evil-ex-define-cmd "grep" #'projectile-ripgrep)
  (evil-ex-define-cmd "ls" #'helm-mini)
  (evil-ex-define-cmd "ts" 'xref-find-definitions-with-prompt))

(use-package evil-magit :ensure t)
(use-package evil-matchit :ensure t)
(use-package evil-numbers :ensure t)
(use-package evil-rails :ensure t)
(use-package evil-surround :ensure t)
(use-package evil-tabs :ensure t)
(use-package evil-textobj-anyblock :ensure t)
(use-package evil-textobj-column :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package find-file-in-project :ensure t)

(use-package flycheck
  :ensure t
  :delight
  :custom
  (flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id)))

(use-package gh-md :ensure t)

(use-package git-gutter+
  :ensure t
  :delight
  :custom
  (git-gutter+-git-executable "/usr/bin/git"))

(use-package go-autocomplete :ensure t :hook go-mode)

(use-package go-mode
  :ensure t
  :bind (("M-." . #'godef-jump)
	 ("M-*" . #'pop-tag-mark))
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save))

  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (setenv "gopath" "/home/jcmuller/go")
  (add-to-list 'exec-path "/home/jcmuller/go/bin")
  (with-eval-after-load 'go-mode (require 'go-autocomplete)))

(use-package goto-last-change :ensure t)

(use-package helm
  :ensure t
  :bind (("C-x C-f" . #'helm-find-files)
	 ("C-x r b" . #'helm-filtered-bookmarks)
	 ("C-x b" . #'helm-mini)
	 ("M-x" . #'helm-M-x))
  :delight
  :custom
  (helm-M-x-fuzzy-match t)
  (helm-candidate-number-limit 50)
  (helm-completion-in-region-fuzzy-match t)
  (helm-exit-idle-delay 0)
  (helm-ff-link-stype-map nil)
  (helm-idle-delay 0.1)
  (helm-input-idle-delay 0.1)
  (helm-ls-git-fuzzy-match t)
  (helm-mode-fuzzy-match t))

(use-package helm-bundle-show :ensure t)
(use-package helm-cmd-t :ensure t)
(use-package helm-flx :ensure t)
(use-package helm-flycheck :ensure t)
(use-package helm-fuzzier :ensure t)
(use-package helm-fuzzy-find :ensure t)
(use-package helm-ls-git :ensure t)
(use-package helm-projectile :ensure t :config (helm-projectile-on))

(use-package magit
  :ensure t
  :bind ([f9] . #'magit-status)
  :custom
  (magit-commit-arguments (quote ("--verbose")))
  (magit-git-executable "/usr/bin/git"))

(use-package magit-popup :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-mode+ :ensure t)

(use-package neotree
  :ensure t
  :bind ([f8] . #'neotree-project-dir)
  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
	  (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
	  (if (neo-global--window-exists-p)
	      (progn
		(neotree-dir project-dir)
		(neotree-find file-name)))
	(message "Could not find git project root."))))

  (add-hook 'neo-change-root-hook
	    (lambda () (neo-buffer--with-resizable-window
			(let ((fit-window-to-buffer-horizontally t))
			  (fit-window-to-buffer))))))

(use-package org-alert :ensure t)
(use-package org-evil :ensure t)
(use-package origami :ensure t)

(use-package delight :ensure t)

(use-package autorevert :delight auto-revert-mode)
(use-package subword :delight)

(use-package projectile
  :ensure t
  :custom (projectile-switch-project-action (quote projectile-vc))
  :delight '(:eval (concat " Proj:" (projectile-project-name))))

(use-package projectile-rails :ensure t :delight :hook (ruby-mode enh-ruby-mode))
(use-package projectile-ripgrep :ensure t)
(use-package ripgrep :ensure t)
(use-package rubocop :ensure t :delight :custom (rubocop-check-command "rubocop --format emacs") :hook (ruby-mode enh-ruby-mode))
(use-package ruby-end :ensure t :delight :custom (ruby-end-insert-newline nil))
(use-package ruby-extra-highlight :ensure t :hook (ruby-mode enh-ruby-mode))
(use-package ruby-refactor :ensure t :custom (ruby-refactor-add-parens t) :hook (ruby-mode enh-ruby-mode))

(use-package ruby-mode
  :config (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
  :custom (ruby-deep-arglist nil))

(use-package ruby-test-mode :ensure t :delight :hook (ruby-mode enh-ruby-mode))
(use-package ruby-tools :commands ruby-tools-mode :ensure t)
(use-package sentence-navigation :ensure t)
(use-package undo-tree :ensure t :delight)
(use-package vdiff :ensure t)
(use-package yaml-mode :ensure t)
(use-package yasnippet :ensure t :delight yas-minor-mode)
(use-package yasnippet-snippets :ensure t)

(defun big-font-face ()
  "Set big font face."
  (interactive)
  (message "Changing font to Ubuntu Mono 12")
  (custom-set-faces '(default ((t (:family "Ubuntu Mono" :foundry "xos4" :slant normal :weight normal :height 120 :width normal))))))

(defun small-font-face ()
  "Set small font face."
  (interactive)
  (message "Changing font to Terminus 9")
  (custom-set-faces '(default ((t (:family "Terminus" :foundry "xos4" :slant normal :weight normal :height 90 :width normal))))))

(small-font-face)

(defun set-exec-path-from-shell-path ()
  "Override exec path."
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$shell --login -i -c 'echo $path'"))))
    (setenv "path" path-from-shell)
					;(setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-path))

(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/bin")
(add-to-list 'exec-path "/home/jcmuller/bin")
(add-to-list 'exec-path "/home/jcmuller/.rbenv/shims/bundle")

;; mode hooks
(defun my-ruby-mode-hook ()
  "My Ruby mode hook."
  (electric-pair-mode)
  (projectile-rails-mode)
  (rubocop-mode t)
  (ruby-end-mode t)
  (ruby-block-mode t)
  (ruby-block-highlight-mode t)
  (ruby-extra-highlight-mode t)
  (ruby-refactor-mode-launch)
  (ruby-test-mode t)
  (ruby-tools-mode t))

(defun my-modes-hook ()
  "My global hook."

  (column-number-mode t)
  (dynamic-completion-mode t)
  (evil-mode t)
  (global-auto-complete-mode t)
  (global-evil-matchit-mode t)
  (global-evil-surround-mode t)
  (global-evil-tabs-mode t)
  (global-flycheck-mode t)
  (global-font-lock-mode t)
  (global-git-gutter+-mode t)
  (global-linum-mode t)
  (global-origami-mode t)
  (global-subword-mode t)
  (helm-fuzzier-mode t)
  (helm-mode t)
  (menu-bar-mode 0)
  (projectile-mode t)
  (savehist-mode t)
  (scroll-bar-mode 0)
  (show-paren-mode t)
  (tool-bar-mode 0)
  (yas-global-mode 1))

;; default to utf-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(universal-coding-system-argument 'utf-8)

(add-hook 'after-init-hook #'my-modes-hook)
(add-hook 'before-save-hook #'whitespace-cleanup)

(add-to-list 'auto-mode-alist
	     '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))
					;'("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(defun toggle-font ()
  "Cycle font sizes."
  (interactive)
  (if (get 'toggle-font 'state)
      (progn
	(small-font-face)
	(put 'toggle-font 'state nil))
    (progn
      (big-font-face)
      (put 'toggle-font 'state t))))

(defun xref-find-definitions-with-prompt (identifier)
  "Search for the definition of IDENTIFIER."
  (interactive (list (xref--read-identifier "Find definitions of: ")))

  (xref--find-definitions identifier nil))

(global-set-key [f4] #'xref-find-definitions-with-prompt)
(global-set-key [f6] #'toggle-font)
(global-set-key (kbd "C-}")     #'xref-find-definitions-with-prompt)

;; Shorten git branch name
(defun my-shorten-vc-mode-line (string)
  "Shorten VC mode STRING line."
  (cond
   ((string-prefix-p "Git" string)
    (concat "" (substring string 4)))
   (t string)))

(advice-add 'vc-git-mode-line-string :filter-return 'my-shorten-vc-mode-line)

 ;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
  ;;;  find-tag that automagically reruns etags when it cant find a
  ;;;  requested item and then makes a new try to locate it.
  ;;;  Fri Mar 15 09:52:14 2002
;; (defadvice find-tag (around refresh-etags activate)
;;  "Rerun etags and reload tags if tag not found and redo find-tag.
;;  If buffer is modified, ask about save before running etags."
;; (let ((extension (file-name-extension (buffer-file-name))))
;;   (condition-case err
;;   ad-do-it
;;     (error (and (buffer-modified-p)
;;         (not (ding))
;;         (y-or-n-p "Buffer is modified, save it? ")
;;         (save-buffer))
;;        (er-refresh-etags extension)
;;        ad-do-it))))
;; (defun er-refresh-etags (&optional extension)
;; "Run etags on all peer files in current dir and reload them silently."
;; (interactive)
;; (shell-command (format "etags *.%s" (or extension "el")))
;; (let ((tags-revert-without-query t))  ; don't query, revert silently
;;   (visit-tags-table default-directory nil)))

(provide 'init)
;;; init.el ends here

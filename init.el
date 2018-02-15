;;; init.el --- Configuration
;;; Commentary:
;;; Code:

;; Gotta have the right fonts
(if window-system
    (progn
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

      (small-font-face)))

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
(use-package ace-window)

(use-package auto-complete
  :delight
  :defer
  :custom
  (ac-dwim t)
  (ac-delay 0.2)
  :config
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-abbrev
                     ac-source-words-in-same-mode-buffers))
  (ac-config-default))

(use-package ac-etags
  :ensure t
  :config
  (ac-etags-setup)
  (ac-etags-ac-setup))

(use-package autorevert :delight auto-revert-mode)
(use-package browse-at-remote :bind (("C-c g g" . #'browse-at-remote)))

(use-package color-theme-sanityinc-tomorrow
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

(use-package delight)
(use-package dockerfile-mode)
(use-package editorconfig-custom-majormode)
(use-package editorconfig)

(use-package enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-hook)
  (auto-complete-mode)
  (ruby-end-mode)
  (flycheck-mode)
  (electric-pair-mode)

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
  :custom
  (evil-shift-width 2)
  (evil-select-search-module 'evil-search-module 'evil-search)
  :config
  (require 'evil-magit)
  (require 'evil-matchit)
  (require 'evil-numbers)
  (require 'evil-rails)
  (require 'evil-surround)
  (require 'evil-tabs)
  (require 'evil-textobj-anyblock)
  (require 'evil-textobj-column)

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

(use-package evil-magit)
(use-package evil-matchit)
(use-package evil-numbers)
(use-package evil-rails)
(use-package evil-surround)
(use-package evil-tabs)
(use-package evil-textobj-anyblock)
(use-package evil-textobj-column)
(use-package evil-visualstar :config (global-evil-visualstar-mode))
(use-package find-file-in-project)

(use-package flycheck
  :delight
  :custom
  (flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
  (flycheck-disabled-checkers (quote (json-jsonlist javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package gh-md)
(use-package git-gutter+ :delight :custom (git-gutter+-git-executable "/usr/bin/git"))
(use-package go-autocomplete :hook go-mode)

(use-package go-mode
  :bind (("M-." . #'godef-jump)
         ("M-*" . #'pop-tag-mark))
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save))

  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (setenv "gopath" "/home/jcmuller/go")
  (with-eval-after-load 'go-mode (require 'go-autocomplete)))

(use-package goto-last-change)

(use-package helm
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

(use-package helm-bundle-show)
(use-package helm-cmd-t)
(use-package helm-flx)
(use-package helm-flycheck)
(use-package helm-fuzzier)
(use-package helm-fuzzy-find)
(use-package helm-ls-git)
(use-package helm-projectile :config (helm-projectile-on))

(use-package json-mode
  :init (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-jsx-mode))
  :custom
  (js2-allow-rhino-new-expr-initializer nil)
  (js2-basic-offset 2)
  (js2-highlight-level 3)
  (js2-include-node-externs t)
  (js2-missing-semi-one-line-override t)
  (js2-mode-assume-strict t)
  (js2-strict-trailing-comma-warning nil)
  (js2-warn-about-unused-function-arguments t)
  (js-switch-indent-offset 2)
  (js2-global-externs '(
                        "afterAll"
                        "afterEach"
                        "beforeAll"
                        "beforeEach"
                        "describe"
                        "expect"
                        "it"
                        "jest"
                        "require"
                        "test"))
  (add-hook 'js2-mode-hook (lambda ()
                             ;;(subword-mode 1)
                             ;;(diminish 'subword-mode)
                             (js2-imenu-extras-mode 1)))
  (rename-modeline "js2-mode" js2-mode "JS2")
  (rename-modeline "js2-mode" js2-jsx-mode "JSX2")
  :config
  (electric-pair-mode)
  (use-package tern
    :diminish tern-mode
    :init
    (add-hook 'js2-mode-hook 'tern-mode))
  (use-package js-doc)
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config
    (js2r-add-keybindings-with-prefix "C-c r")))

(use-package jsx-mode
  :init (setq jsx-indent-level 2))

(use-package magit
  :bind ([f9] . #'magit-status)
  :custom
  (magit-commit-arguments (quote ("--verbose")))
  (magit-git-executable "/usr/bin/git"))

(use-package magit-popup)
(use-package markdown-mode)
(use-package markdown-mode+)

(use-package neotree
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

(use-package org-alert)
(use-package org-evil)
(use-package org :custom (org-log-done t))
(use-package origami)

(use-package projectile
  :custom
  (projectile-switch-project-action (quote my-projectile-switch-project-action))
  (projectile-enable-caching t)
  :delight '(:eval (concat " Proj:" (projectile-project-name))))

(use-package pdf-tools :magic ("%PDF" . pdf-view-mode) :config (pdf-tools-install))
(use-package projectile-rails :delight :config (projectile-rails-global-mode t))
(use-package projectile-ripgrep)
(use-package ripgrep)

(use-package rspec-mode :config (rspec-install-snippets) :after ruby-mode)

(use-package ruby-mode
  :load-path "/usr/share/emacs/25.3.50/lisp/progmodes"
  :ensure nil
  :config
  (progn
    (use-package ruby-tools)
    (use-package rubocop
      ;:delight
      :custom (rubocop-check-command "rubocop --format emacs"))
    (use-package bundler :load-path "~/.emacs.d/lisp")
    (use-package ruby-block :load-path "~/.emacs.d/lisp" :custom (ruby-block-highlight-toggle t))
    (use-package ruby-end :custom (ruby-end-insert-newline nil))
    (use-package ruby-extra-highlight :config (ruby-extra-highlight-mode t))
    (use-package ruby-refactor :custom (ruby-refactor-add-parens t) :config (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

    (require 'bundler)
    (require 'rubocop)
    (require 'ruby-block)
    (require 'ruby-end)
    (require 'ruby-extra-highlight)
    (require 'ruby-test-mode)
    (require 'ruby-tools)

    (electric-pair-mode t))

  :custom
  (ruby-deep-arglist nil)
  (ruby-deep-indent-paren nil)
                                        ;(ruby-use-smie nil)
  )

(use-package ruby-test-mode
  ;:delight
  :hook ruby-mode)
(use-package sentence-navigation)
                                        ;(use-package subword :delight)
(use-package undo-tree :delight)
(use-package vdiff)

(use-package web-mode
  :mode "\\.html\\'" "\\.erb\\'" "\\.ejs\\'"
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-extra-snippets '(("erb" . (
                                       ("q=" . "<%= | %>")
                                       ("qq" . "<% | %>")
                                       )
                              )))
  :config
  (ac-etags-setup)
  (ac-etags-ac-setup)
  (flycheck-add-mode 'html-tidy 'web-mode)

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      )))

(use-package yaml-mode)
(use-package yasnippet :delight yas-minor-mode)
(use-package yasnippet-snippets)

;; mode hooks
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
                                        ;(global-subword-mode nil)
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

(defun my-projectile-switch-project-action ()
  "Load this thing."
  (projectile-vc)
  (ac-etags-clear-cache)
  (message "Ran project action."))

(defun remove-dup-empty-lines ()
  "Remove duplicate empty lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t) (replace-match "\n\n"))))

(defun my-whitespace-cleanup ()
  "Remove whitespace."
  (interactive)
  (whitespace-cleanup)
  (remove-dup-empty-lines))

(add-hook 'after-init-hook #'my-modes-hook)
(add-hook 'before-save-hook #'my-whitespace-cleanup)

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

;;; init.el --- Configuration
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'backup-directory-alist '("." . "~/.emacs.d/backups"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

(load-theme 'sanityinc-tomorrow-day)

(setenv "gopath" "/home/jcmuller/go")

(add-to-list 'exec-path "/home/jcmuller/go/bin")

(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/bin")
(add-to-list 'exec-path "/home/jcmuller/bin")
(add-to-list 'exec-path "/home/jcmuller/.rbenv/shims/bundle")

;; mode hooks
(defun my-go-mode-hook ()
  "My Go mode hook."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "m-.") 'godef-jump)
  (local-set-key (kbd "m-*") 'pop-tag-mark))

(defun my-ruby-mode-hook ()
  "My Ruby mode hook."
  (require 'bundler)
  (require 'rubocop)
  (require 'ruby-block)
  (require 'ruby-end)
  (require 'ruby-extra-highlight)
  (require 'ruby-refactor)
  (require 'ruby-test-mode)
  (require 'ruby-tools)

  (auto-complete-mode)
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
  (require 'origami)
  (require 'yasnippet)

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

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'after-init-hook #'my-modes-hook)
(add-hook 'before-save-hook #'whitespace-cleanup)

(add-to-list 'auto-mode-alist
	     '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))
	     ;'("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(require 'browse-at-remote)
(require 'evil)
(require 'evil-magit)
(require 'evil-tabs)
(require 'find-file-in-project)
(require 'helm-cmd-t)
(require 'helm-fuzzy-find)
(require 'helm-fuzzier)
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-projectile)
(require 'projectile-ripgrep)

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

(evil-ex-define-cmd "ls" 'helm-mini)
(evil-ex-define-cmd "Gblame" 'magit-blame-popup)
(evil-ex-define-cmd "grep" 'projectile-ripgrep)
(evil-ex-define-cmd "ts" 'xref-find-definitions-with-prompt)

(helm-projectile-on)

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
			(fit-window-to-buffer)))))

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
(global-set-key [f5] #'toggle-theme)
(global-set-key [f6] #'toggle-font)
(global-set-key [f8] #'neotree-project-dir)
(global-set-key [f9] #'magit-status)

(global-set-key (kbd "C-c g g") #'browse-at-remote)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "M-x")     #'helm-M-x)
(global-set-key (kbd "C-}")     #'xref-find-definitions-with-prompt)

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

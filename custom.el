;;; custom.el --- Customization
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#d6d6d6" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#4d4d4c"))
 '(beacon-color "#c82829")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(default-frame-alist (quote ((font . "Ubuntu-Mono-12"))))
 '(electric-pair-mode t)
 '(enh-ruby-bounce-deep-indent nil)
 '(enh-ruby-deep-indent-construct nil)
 '(enh-ruby-deep-indent-paren nil)
 '(enh-ruby-hanging-brace-deep-indent-level 2)
 '(enh-ruby-hanging-brace-indent-level 2)
 '(enh-ruby-hanging-indent-level 2)
 '(enh-ruby-hanging-paren-deep-indent-level 2)
 '(enh-ruby-hanging-paren-indent-level 2)
 '(enh-ruby-indent-level 2)
 '(evil-shift-width 2)
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(font-lock-maximum-decoration t)
 '(git-gutter+-git-executable "/usr/bin/git")
 '(grep-command "rg -i -M 120 --no-heading --line-number --color never")
 '(helm-M-x-fuzzy-match t)
 '(helm-candidate-number-limit 50)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-exit-idle-delay 0)
 '(helm-ff-link-stype-map nil)
 '(helm-idle-delay 0.1)
 '(helm-input-idle-delay 0.1)
 '(helm-ls-git-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(magit-commit-arguments (quote ("--verbose")))
 '(magit-git-executable "/usr/bin/git")
 '(package-selected-packages
   (quote
    (rubocop org-alert yasnippet-snippets yasnippet origami vdiff dockerfile-mode markdown-mode+ gh-md markdown-mode helm-flx helm-flycheck helm-fuzzier helm-fuzzy-find enh-ruby-mode color-theme-sanityinc-tomorrow flycheck ruby-end ruby-extra-highlight ruby-refactor ruby-test-mode ruby-tools helm-ls-git helm-cmd-t helm-bundle-show bundler ace-window browse-at-remote neotree evil-magit evil yaml-mode evil-tabs projectile-ripgrep ripgrep editorconfig editorconfig-custom-majormode helm-projectile ## magit thrift sentence-navigation org-evil goto-last-change go-mode go-autocomplete git-gutter+ exec-path-from-shell evil-textobj-column evil-textobj-anyblock evil-surround evil-rails evil-numbers evil-matchit)))
 '(projectile-switch-project-action (quote projectile-vc))
 '(rubocop-check-command "rubocop --format emacs")
 '(ruby-deep-arglist nil)
 '(ruby-end-insert-newline nil)
 '(ruby-refactor-add-parens t)
 '(save-place t)
 '(truncate-lines t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator ":")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Terminus" :foundry "xos4" :slant normal :weight normal :height 90 :width normal))))
 '(enh-ruby-regexp-delimiter-face ((t (:foreground "#215255")))))

(provide 'custom)
;;; custom.el ends here

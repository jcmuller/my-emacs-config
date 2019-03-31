;;; custom.el --- Customization
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.2)
 '(ac-dwim t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#d6d6d6" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#4d4d4c"))
 '(beacon-color "#c82829")
 '(browse-url-browser-function (quote browse-url-generic) nil nil "Customized with use-package browse-at-remote")
 '(browse-url-generic-program "/usr/local/bin/chromium")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id) t)
 '(font-lock-maximum-decoration t)
 '(grep-command "rg -i -M 120 --no-heading --line-number --color never")
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold (* 20 1024 1024))
 '(line-number-mode nil)
 '(org-log-done t t)
 '(package-selected-packages
   (quote
    (markdown-toc yasnippet-snippets yaml-mode web-mode vdiff use-package tern sentence-navigation ruby-tools ruby-test-mode ruby-refactor ruby-extra-highlight ruby-end rubocop rspec-mode rjsx-mode projectile-ripgrep pdf-tools origami org-evil org-alert nvm neotree markdown-mode+ jsx-mode json-mode js2-refactor js-doc helm-projectile helm-ls-git helm-fuzzy-find helm-fuzzier helm-flycheck helm-flx helm-cmd-t helm-bundle-show goto-last-change go-rename go-guru go-autocomplete git-gutter+ gh-md flycheck-flow find-file-in-project exec-path-from-shell evil-visualstar evil-textobj-column evil-textobj-anyblock evil-tabs evil-surround evil-rails evil-numbers evil-matchit evil-magit evil-collection enh-ruby-mode editorconfig-custom-majormode dockerfile-mode delight ctags-update color-theme-sanityinc-tomorrow bundler browse-at-remote ace-window ac-etags)))
 '(save-place t)
 '(tab-width 2)
 '(truncate-lines t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator ":"))

(provide 'custom)
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
 '(browse-url-browser-function (quote browse-url-generic) nil nil "Customized with use-package browse-at-remote")
 '(browse-url-generic-program "/usr/local/bin/chromium")
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(default-frame-alist (quote ((font . "Ubuntu-Mono-12"))))
 '(electric-pair-mode t)
 '(fci-rule-color "#d6d6d6")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id) t)
 '(font-lock-maximum-decoration t)
 '(grep-command "rg -i -M 120 --no-heading --line-number --color never")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (yasnippet-snippets yaml-mode vdiff use-package thrift sentence-navigation ruby-tools ruby-test-mode ruby-refactor ruby-extra-highlight ruby-end rubocop projectile-ripgrep origami org-evil org-alert neotree markdown-mode+ helm-projectile helm-ls-git helm-fuzzy-find helm-fuzzier helm-flycheck helm-flx helm-cmd-t helm-bundle-show goto-last-change go-mode go-autocomplete git-gutter+ gh-md find-file-in-project exec-path-from-shell evil-textobj-column evil-textobj-anyblock evil-tabs evil-surround evil-rails evil-numbers evil-matchit evil-magit enh-ruby-mode editorconfig-custom-majormode dockerfile-mode color-theme-sanityinc-tomorrow bundler browse-at-remote ace-window)))
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

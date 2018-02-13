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
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#d6d6d6")
 '(font-lock-maximum-decoration t)
 '(grep-command "rg -i -M 120 --no-heading --line-number --color never")
 '(large-file-warning-threshold (* 20 1024 1024))
 '(save-place t)
 '(truncate-lines t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator ":"))

(provide 'custom)
;;; custom.el ends here

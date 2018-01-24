;;; bundler.el
;; Interact with Bundler from Emacs.
;;
;; 1) bundle-open
;;
;;    Wraps 'bundle open' which, if the given gem is installed and has been
;;    required correctly, will open the gem's source directory with dired.
;;
;;; Code:

(require 'cl-lib)
(require 'inf-ruby)

;;;###autoload
(defun bundle-open (gem-name)
  "Queries for a gem name and opens the location of the gem in dired."
  (interactive (list (completing-read "Bundled gem: " (bundle-list-gems-cached))))
    (if (= (length gem-name) 0)
        (message "No gem name given.")
      (let ((gem-location (bundle-gem-location gem-name)))
        (cond
         ((eq gem-location 'no-gemfile)
          (message "Could not find Gemfile"))
         (gem-location
          (neotree-dir gem-location))
         (t
          (message "Gem '%s' not found" gem-name))))))

(defun bundle-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-gemfile if the
Gemfile could not be found, or nil if the Gem could not be
found."
  (let ((bundler-stdout
         (shell-command-to-string
          (format "bundle show %s" (shell-quote-argument gem-name))))
        (remote (file-remote-p default-directory)))
    (cond
     ((string-match "Could not locate Gemfile" bundler-stdout)
      'no-gemfile)
     ((string-match "Could not find " bundler-stdout)
      nil)
     (t
      (concat remote
              (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               bundler-stdout)
              "/")))))

(defvar bundle-gem-list-cache
  (make-hash-table)
  "Holds a hash table of gem lists per directory.")

(cl-defun bundle-locate-gemfile (&optional (dir default-directory))
         (let ((has-gemfile (directory-files dir nil "^Gemfile$"))
               (is-root (equal dir "/")))
           (cond
            (has-gemfile dir)
            (is-root
             (print (format
                     "No Gemfile found in either %s or any parent directory!"
                     default-directory))
             nil)
            ((bundle-locate-gemfile (expand-file-name ".." dir))))))

(defun bundle-list-gems-cached ()
  (let* ((gemfile-dir (bundle-locate-gemfile))
         (gem-list (gethash gemfile-dir bundle-gem-list-cache)))
    (if (not gemfile-dir)
        nil
      (unless gem-list
        (print (format "Don't have directory %s in cache yet, updating." gemfile-dir))
        (setq gem-list (bundle-list-gems))
        (puthash gemfile-dir gem-list bundle-gem-list-cache))
      gem-list)))

(defun bundle-list-gems ()
  (save-excursion
    (let* ((cmd "bundle list")
           (bundle-out (shell-command-to-string cmd))
           (bundle-lines (split-string bundle-out "\n")))

      (defun parse-bundle-list-line (line)
        (cond
         ((string-match "^  \\* \\([^\s]+\\).*$" line)
          (match-string 1 line))
         ((string-match "Could not \\(find\\|locate\\)" line)
          (message line) nil)
         ((string-match "Gems included by the bundle:\\|^ *$" line)
          nil)
         (t
          (message "Warning: couldn't parse line from \"%s\":\n%s"
                   cmd line)
          nil)))

      (remq nil (mapcar 'parse-bundle-list-line bundle-lines)))))

(provide 'bundler)
;;; bundler.el ends here.

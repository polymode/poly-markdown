
(load-file "targets/utils.el")
(require 'package)

(setq package-deps (polymode-library-deps "poly-markdown.el")
      package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(defun package-desc-new (name)
  (cadr (assq name package-archive-contents)))

(defun package-outdated-p (name)
  (let ((old-version (package-desc-version (cadr (assq name package-alist))))
        (new-version (package-desc-version (cadr (assq name package-archive-contents)))))
    (and (listp old-version) (listp new-version)
         (version-list-< old-version new-version))))

(package-initialize)
(package-refresh-contents)

(dolist (package (append dev-deps package-deps))
  (if (package-installed-p package)
      (when (package-outdated-p package)
        (package-install-from-archive (cadr (assq package package-archive-contents))))
    (package-install package)))

(message "INSTALLED DEPS: %s"
         (mapconcat
          (lambda (pkg)
            (format "%s:%S" pkg
                    (package-desc-version (cadr (assq pkg package-archive-contents)))))
          package-deps
          " "))

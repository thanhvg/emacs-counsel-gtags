;; get needed packages for testing using straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-temp-buffer
      ;; http://ergoemacs.org/emacs/elisp_read_file_content.html
      (insert-file-contents "./straight.el/develop/install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; get extra needed packages
(straight-use-package 'f)
(straight-use-package 'counsel)

;; load ../counsel-gtags.el
(load
 (concat
  (locate-dominating-file "./" "counsel-gtags.el")
  "counsel-gtags.el"))

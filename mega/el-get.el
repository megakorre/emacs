(defun mega/sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (setq el-get-sources mega/packages)
  (setq my-packages (mapcar 'el-get-source-name el-get-sources))
  (el-get 'sync my-packages)
  (package-initialize))

(defun mega/boot-el-get ()
  (package-initialize)
  (if (require 'el-get nil t)
      (mega/sync-packages)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)
       (setq el-get-verbose t)
       (mega/sync-packages)))))

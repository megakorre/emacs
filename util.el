(defalias 'first 'car)
(defalias 'rest  'cdr)

(defconst ruby-block-beg-re
  "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
  )

(defconst ruby-non-block-do-re
  "\\(while\\|until\\|for\\|rescue\\)\\>"
  )

(defconst ruby-indent-beg-re
  "\\(\\s *\\(class\\|module\\|def\\)\\)\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin"
    )

(defconst ruby-modifier-beg-re
  "if\\|unless\\|while\\|until"
  )

(defconst ruby-modifier-re
  (concat ruby-modifier-beg-re "\\|rescue")
  )

(defconst ruby-block-mid-re
  "then\\|else\\|elsif\\|when\\|rescue\\|ensure"
  )

(defconst ruby-block-op-re
  "and\\|or\\|not"
  )

(defconst ruby-block-hanging-re
  (concat ruby-modifier-beg-re "\\|" ruby-block-op-re)
  )

(defconst ruby-block-end-re "end")

(defun github-package (name package)
  `(:name ,name
          :type github
          :pkgname ,package
          :after (progn
                   (require (quote ,name)))))

(defun melpa-package (name)
  `(:name ,name :type elpa
	  :after (progn (require (quote ,name)))))

(defun force-save-buffer ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defmacro deftag
  (name)
  `(progn (defclass ,name nil ())))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (setq my-packages (mapcar 'el-get-source-name el-get-sources))
  (el-get 'sync my-packages))

(defun el-get-init ()
  (if (require 'el-get nil t)
      (sync-packages)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)
       (setq el-get-verbose t)
       (sync-packages)))))

(defun el-get-packages (&rest packages)
  (package-initialize)
  (setq el-get-sources packages)
  (el-get-init)
  (package-initialize))

(defun in-modes? (modes)
  (-contains? modes major-mode))

(defun global-set-keys (&rest values)
  (-> (-partition 2 values)
    (--each (global-set-key (kbd (car it)) (car (cdr it))))))

(provide 'util)

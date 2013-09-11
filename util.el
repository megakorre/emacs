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
  (delete-trailing-whitespace)
  (save-buffer))

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

;; -------------------------------------------------------------------------------------

(defun m/keys (plist)
  (->> plist
    (-partition 2)
    (-map 'first)))

(defun m/vals (plist)
  (->> plist
    (-partition 2)
    (-map 'second)))

(defalias 'm/get 'plist-get)

(defun m/merge (plist-a &rest plist-b)
  (-reduce-from
   (lambda (plist-a plist-b)
     (->> (-partition 2 plist-b)
       (--reduce-from
	(let ((key (first it))
	      (val (second it)))
	  (plist-put acc key val))
	plist-a)))
   plist-a
   plist-b))

(defun m/p (&rest args)
  (assert (evenp (length args))
	  nil
	  "Plist has to have a even number of arguments")
  args)

(defun m/dissoc (plist key)
  (-flatten
   (--reject (eq (first it) key) (-partition 2 plist))))

(defun m/assoc (plist &rest pairs)
  (assert (evenp (length pairs)) nil "plist requiers even numbers of args")
  (m/merge plist pairs))

(defun m/update-in (plist index f &rest args)
  (assert (< 0 (length index)) nil "index has to be present")
  (let ((key (first index)))
    (if (eq 1 (length index))
	(m/assoc plist key (apply f (m/get plist key) args))
      (m/assoc plist key (apply 'm/update-in (m/get plist key) (rest index) f args)))))

(defun m/merge-with (f plist-a plist-b)
  (-reduce-from
   (lambda (accu input-pair)
     (let* ((key (first input-pair))
	    (old-val (plist-get accu key))
	    (new-val
	     (if old-val
		 (funcall f old-val (second input-pair))
	       (second input-pair))))
       (plist-put accu key new-val)))
   plist-a
   (-partition 2 plist-b)))

(defun m/assoc-in (plist index &rest keyvals)
  (m/update-in plist index 'm/merge keyvals))

(defun m/map-vals (f plist)
  (->> (-partition 2 plist)
    (-map (lambda (pair)
	    (list (first pair) (funcall f (second pair)))))
    (-flatten)))

(defun m/keyword->symbol (keyword)
  (intern (substring (symbol-name keyword) 1)))

(defun m/symbol->keyword (symbol)
  (intern (concat ":" (symbol-name symbol))))

(defun m/key-lookup-pair (keys map-name)
  (-map
   (lambda (key) `(,key (m/get ,map-name ,(m/symbol->keyword key))))
   keys))

(defmacro m/letm (form &rest code)
  (assert (evenp (length form)))
  (let* ((map (gensym))
	 (keys (first form))
	 (val-exp (second form))
	 (remaining (rest (rest form)))
	 (next-code
	  (if (null remaining)
	      `(progn .,code)
	    `(m/letm ,remaining .,code))))
    `(let* ((,map ,val-exp)
	    .,(m/key-lookup-pair keys map))
       ,next-code)))

(assert
 (eq (m/letm ((a b c) (list :a 1 :b 2 :c 3)
	      (d e)   (list :d 5 :e 2))
	     (+ a b c d e))
     13))

(provide 'util)

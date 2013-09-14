(require 'json)

(defun pivotal-project ()
  (string-to-int (slurp "~/.pivotal-project")))
(defun pivotal-api-key ()
  (slurp "~/.pivotolapi"))

(defun pivotal-fetch (request)
  (m/letm ((method path filter) request)
    (let ((url-request-method method)
	  (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
				       ("X-TrackerToken" . ,(pivotal-api-key))))
	  (url-request-data (if filter (format "filter=%s" filter))))
      (with-current-buffer (url-retrieve-synchronously
			    (concat "https://www.pivotaltracker.com/services/v5/" path))
	(buffer-string)))))

(defun pivotal-parse-r (content)
  (-map 'alist->plist
	(json-read-from-string
	 (s-join "\n"
		 (-> content
		   (split-string "\n\n")
		   (rest))))))

(defun pivotol-comments-r (story-id &rest args)
  (list
   :method "GET"
   :path (format "projects/%i/stories/%i/comments" (pivotal-project) story-id)))

(defun pivotal-stories-r (&rest args)
  (list
   :method "GET"
   :path (format "projects/%i/stories" (pivotal-project))
   :filter "state:started"))

(defun pivotal-active-stories ()
  (-> (pivotal-stories-r) (pivotal-fetch) (pivotal-parse-r)))

(defun pivotal-active-stories-names ()
  (--map
   (m/letm ((id name) it)
     (format "%i|%s" id (decode-coding-string name 'utf-8)))
   (pivotal-active-stories)))

(defun pivotal-make-ref (story-name)
  (interactive
   (list (completing-read "Story: " (pivotal-active-stories-names))))
  (insert
   (format "[%s]" (first (split-string story-name "|")))))

(defun pivotal-show-comments (story-name)
  (interactive
   (list (completing-read "Story: " (pivotal-active-stories-names))))
  (switch-to-buffer "*pivotol-comments*")
  (erase-buffer)
  (let* ((id (string-to-int (first (split-string story-name "|"))))
	 (comments (-> (pivotol-comments-r id)
		     pivotal-fetch
		     pivotal-parse-r)))
    (--each comments
      (m/letm ((text created_at) it)
	(insert (format "CREATED AT: %s\n\n" created_at))
	(insert (decode-coding-string text 'utf-8))
	(insert "\n\n")
	(insert (s-join "" (-repeat 30 "-")))
	(insert "\n")))))

(provide 'pivotol)

;; simple tool that downloads buglist from GitHub bugtracker

;; eval in scratch to test: 

;; fast (small repo):

;; (buglist-to-element   "https://api.github.com/repos/octocat/Hello-World/issues")
;; (buglist-write-buffer "https://api.github.com/repos/octocat/Hello-World/issues")

;; slow because of synchroneous download (~6sec for 18 requests):

;; (buglist-to-element   "https://api.github.com/repos/joyent/node/issues")
;; (buglist-write-buffer "https://api.github.com/repos/joyent/node/issues")

;; now, using Nicolas' exporter:
;; (let ((url "https://api.github.com/repos/octocat/Hello-World/issues"))
;;  (org-element-interpret-data `(org-data nil ,(buglist-to-element url))))


(require 'json)
(require 'url)

(defun getv (key alist)
  (cdr (assoc key alist)))

(defun buglist-write-buffer (url)
  (let ((bl (dl-json url)))
    (mapc 'bug-write-buffer bl))
  t ;; hide huge dump in *scratch*
)

(defun buglist-to-element (url)
  (let ((bl (dl-json url)))
    (mapcar 'bug-to-element bl))
)

(defun bug-write-buffer (b)
  "Insert bug B as a TODO in Org-mode syntax in current buffer."
  (insert (format "* %s %s\n%s\n\n"
                  (upcase (getv 'state b))
                  (getv 'title b)
                  (getv 'body b)))
)

(defun bug-to-element (b)
  "Returns bug B as a TODO element."
  (let ((title (getv 'title b))
        (state (upcase (getv 'state b))))
    `(headline 
      (:raw-value ,title :title ,title :level 1 
                  :todo-type todo :todo-keyword ,state)))
)


(defun dl-json-page (url)
  "Returns a cons of the parsed JSON object from URL and the next page URL."
  (let ((download-buffer (url-retrieve-synchronously url))
        page-next
        header-end
        ret)

    (save-excursion
      (set-buffer download-buffer)

      ;; get HTTP header end position
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (setq header-end (point))

      ;; get next page url
      (goto-char (point-min))
      (when (re-search-forward 
             "<\\(https://api.github.com.+?page=[0-9]+.*?\\)>; rel=\"next\""
             header-end t)
        (setq page-next (match-string 1)))

      (goto-char header-end)
      (setq ret (cons (json-read) page-next))
      (kill-buffer)
      ret
)))

(defun dl-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let* ((ret (dl-json-page url))
         (data (car ret))
         (url (cdr ret))
         (json data))

         (while url
           (setq ret (dl-json-page url))
           (setq data (car ret))
           (setq url (cdr ret))
           (setq json (vconcat json data)))

         json
))


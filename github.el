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
  "Returns the value of KEY in ALIST."
  (cdr (assoc key alist)))

(defun buglist-write-buffer (url)
  "Inserts buglist at URL in current buffer."
  (let ((bl (dl-json url)))
    (mapc 'bug-write-buffer bl))
  t) ;; hide huge dump in *scratch*


(defun buglist-to-element (url)
  "Returns buglist at URL as an element."
    (let ((buglist (mapcar 'bug-to-element (dl-json url))))
      `(headline (:level 1 :title ("Buglist"))
                 (section nil
                          (property-drawer (:properties (("url" . ,url)))))
                 ,@buglist)))

(defun bug-write-buffer (b)
  "Insert bug B as a TODO in Org-mode syntax in current buffer."
  (insert (format "* %s %s\n%s\n\n"
                  (upcase (getv 'state b))
                  (getv 'title b)
                  (getv 'body b))))


(defun bug-to-element (b)
  "Returns bug B as a TODO element."
  `(headline
    (:raw-value    ,(getv 'title b)
     :title        ,(getv 'title b)
     :level        2
     :todo-type    todo
     :todo-keyword ,(upcase (getv 'state b)))
    (section
     nil
     (property-drawer
      (:properties
       (("id" . ,(getv 'number b)))))
     (paragraph nil ,(getv 'body b)))))

(defun append-max-page (url)
  "Returns same URL with the maximum element-per-page param allowed by github."
  (if (string-match-p "per_page" url)
      url
    (concat url (if (string-match-p "\\?" url) nil "?") "&per_page=100")))

(defun dl-json-page (url)
  "Returns a cons of the parsed JSON object from URL and the next page URL."
  (let ((download-buffer (url-retrieve-synchronously (append-max-page url)))
        page-next
        header-end
        ret)

    (with-current-buffer download-buffer
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
      ret)))

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

         json))


(defun org-replace-buglist (elem buglist)
  "Replace first occurence of the buglist with the same url as
BUGLIST in ELEM by BUGLIST."
  (let* ((url (org-headline-url buglist))
         (old (org-find-buglist elem url)))
    (when (and url old buglist)
      (setcar old (car buglist))
      (setcdr old (cdr buglist))
      elem)))


(defun org-headline-url (e)
  "Returns the url of the buglist in headline E."
  (cdr (assoc "url"
              (org-element-property
               :properties
               (car (org-element-contents
                     (car (org-element-contents e))))))))

(defun org-find-buglist (elem url)
  "Returns first occurence of a headline with a url propertie of URL
in element ELEM.

If URL is nil, returns first headline with a url properties."
  (let ((type (org-element-type elem))
        (contents (org-element-contents elem)))
    (cond
     ;; if it's a buglist with the right url, return it
     ((and (eq type 'headline)
           (if url (string= url (org-headline-url elem)) (org-headline-url elem)))
      elem)
     ;; else if it contains elements, look recursively in it
     ((or (eq type 'org-data) (memq type org-element-greater-elements))
      (let (found)
        (catch 'exit
          (mapc (lambda (e)
                  (when (setq found (org-find-buglist e url))
                    (throw 'exit found)))
                contents)
          found)))
     ;; terminal case
     (t
      nil))))

(defun org-sync-import (url)
  "Fetch and insert bugs from URL."
  (interactive "sURL:")
  (save-excursion
    (insert (org-element-interpret-data
             `(org-data nil ,(buglist-to-element url))))))

(defun org-sync-pull ()
  "Update first buglist in current buffer."
  (interactive)
  ;; since we replace the whole buffer, save-excusion doesn't work so
  ;; we manually (re)store the point
  (let* ((oldpoint (point))
         (doc (org-element-parse-buffer))
         (oldbl (org-find-buglist doc nil))
         (url (org-headline-url oldbl))
         (newbl (buglist-to-element url)))

    (org-replace-buglist doc newbl)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (org-element-interpret-data doc))
    (goto-char oldpoint)))

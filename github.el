;; simple tool that downloads buglist from GitHub bugtracker

;; fast (small repo):
;; M-x os-sync-import RET github.com/octocat/Hello-World RET

;; slow because of synchroneous download (~4sec):
;; M-x os-import RET github.com/joyent/node RET

;; buglist data structure

;; '(:title "My buglist"
;;   :url "http://github.com/repos/octocat/Hello-World"
;;   :bugs (BUGS...))


;; bug data structure
;; '(:id 3
;;   :status 'open ;; or 'closed
;;   :title "foo" :desc "blah"
;;   :priority 0 ;; up to 4 for max priority
;;   :tags ("a" "b" "c")
;;   :author "Aurélien"
;;   :assignee "Foo"

;;   ;; dates are in ISO-8601
;;   :date-deadline "2011-04-12T23:09:31Z"
;;   :date-creation "2011-04-10T20:09:31Z"
;;   :date-modification "2011-04-10T21:09:31Z"

;;   ;; backend-specific properties
;;   ;; ...
;;   )

;; os-pull is broken atm

(require 'json)
(require 'url)

(defconst os-buglist-properties
  '(title url)
  "List of shared buglist properties.")

(defconst os-bug-properties
  '(id status author title priority tags date-deadline
       date-creation date-modification author assignee desc)
  "List of shared bug properties.")

(defconst os-github-bug-properties
  '()
  "List of bug properties specific to GitHub.")

(defconst os-github-buglist-properties
  '()
  "List of buglist properties specific to GitHub.")

(defun os-propertize (sym)
  "Return sym as a property i.e. prefixed with :."
  (intern (concat ":" (symbol-name sym))))

(defun os-get-prop (key b)
  "Return value of the property KEY in buglist or bug B."
  (plist-get b key))

;; XXX: per_page defconst
(defun os-github-buglist-url (repo)
  "Return the first url to fetch bugs from REPO.

Return the first page url from REPO url or nil if REPO is
invalid."
  (when (string-match "github.com/\\(?:repos/\\)?\\([^/]+/[^/]+\\)" repo)
    (concat "https://api.github.com/repos/" (match-string 1 repo)
            "/issues?per_page=100")))

(defun os-github-fetch-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let* ((ret (os-github-fetch-json-page url))
         (data (car ret))
         (url (cdr ret))
         (json data))

         (while url
           (setq ret (os-github-fetch-json-page url))
           (setq data (car ret))
           (setq url (cdr ret))
           (setq json (vconcat json data)))

         json))

(defun os-github-fetch-json-page (url)
  "Return a cons (JSON object from URL . next page url)."
  (let ((download-buffer (url-retrieve-synchronously url))
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

(defun os-github-repo-name (url)
  "Return the name of the repo at URL."
  (if (string-match "github.com/\\(?:repos/\\)[^/]+/\\([^/]+\\)" url)
      (match-string 1 url)
    "<project name>"))

(defun os-github-fetch-buglist (repo)
  "Return the buglist at REPO."
  (let* ((url (os-github-buglist-url repo))
         (json (os-github-fetch-json url))
         (title (concat "Bugs of " (os-github-repo-name url))))

    `(:title ,title
             :url ,url
             :bugs ,(mapcar 'os-github-json-to-bug json))))

(defun os-github-json-to-bug (data)
  "Return DATA (in json) converted to a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key data)))
    (let* ((id (v 'number))
           (stat (if (string= (v 'state) "open") 'open 'closed))
           (title (v 'title))
           (desc  (v 'body))
           (author (va 'login (v 'user)))
           (assignee (va 'login (v 'assignee)))
           (ctime (v 'created_at))
           (milestone (v 'milestone))
           (dtime (va 'description milestone))
           (mtime (v 'updated_at))
           (tags (mapcar (lambda (e)
                           (va 'name e)) (v 'labels)))
           (priority 2))

      `(:id ,id
            :author ,author
            :assignee ,assignee
            :status ,stat
            :title ,title
            :desc ,desc
            :priority ,priority ;; XXX: defvar for default priority
            :tags ,tags
            :date-deadline ,dtime
            :date-creation ,ctime
            :date-modification ,mtime))))


(defun os-buglist-to-element (bl)
  "Returns buglist BL as an element."
    (let* ((elist (mapcar 'os-bug-to-element (os-get-prop :bugs bl)))
           (title (os-get-prop :title bl))
           (url (os-get-prop :url bl))
           (backend-props (os-plist os-github-buglist-properties bl)))
      `(headline
        (:level 1 :title (,title))
        (section
         nil
         (property-drawer (:properties (("url" . ,url) ,@backend-props))))
        ,@elist)))

(defun os-filter-list (list minus)
  "Return a copy of LIST without elements in MINUS."
  (let ((final (copy-seq list)))
    (mapc (lambda (x)
            (delq x final)) minus)
    final))

(defun os-plist (properties bug &optional prefix)
  "Return plist of PROPERTIES in bug or buglist BUG.

Prefix property name with : when PREFIX is non-nil."
  (mapcar (lambda (x)
            (let ((p (os-propertize x)))
              (cons (if prefix p x) (os-get-prop p bug)))) properties))

(defun os-bug-to-element (b)
  "Returns bug B as a TODO element."
  (let* ((skip '(title status desc))
         (props (os-filter-list os-bug-properties skip))
         (plist (os-plist props b))
         (backend-plist (os-plist os-github-bug-properties b)))

    `(headline
      (:title ,(os-get-prop :title b)
              :level 2
              :todo-type todo
              :todo-keyword ,(upcase (symbol-name (os-get-prop :status b))))

      (section
       nil
       (property-drawer
        (:properties (,@plist ,@backend-plist)))
       (paragraph nil ,(os-get-prop :desc b))))))


(defun os-replace-buglist (elem buglist)
  "Replace first occurence of the buglist with the same url as
BUGLIST in ELEM by BUGLIST."
  (let* ((url (os-headline-url buglist))
         (old (os-find-buglist elem url)))
    (when (and url old buglist)
      (setcar old (car buglist))
      (setcdr old (cdr buglist))
      elem)))


(defun os-headline-url (e)
  "Returns the url of the buglist in headline E."
  (cdr (assoc "url"
              (os-element-property
               :properties
               (car (os-element-contents
                     (car (os-element-contents e))))))))

(defun os-find-buglist (elem url)
  "Returns first occurence of a headline with a url propertie of URL
in element ELEM.

If URL is nil, returns first headline with a url properties."
  (let ((type (org-element-type elem))
        (contents (org-element-contents elem)))
    (cond
     ;; if it's a buglist with the right url, return it
     ((and (eq type 'headline)
           (if url
               (string= url (os-headline-url elem))
             (os-headline-url elem)))
      elem)
     ;; else if it contains elements, look recursively in it
     ((or (eq type 'org-data) (memq type org-element-greater-elements))
      (let (found)
        (catch 'exit
          (mapc (lambda (e)
                  (when (setq found (os-find-buglist e url))
                    (throw 'exit found)))
                contents)
          found)))
     ;; terminal case
     (t
      nil))))

(defun os-import (url)
  "Fetch and insert bugs from URL."
  (interactive "sURL:")
  (let* ((buglist (os-github-fetch-buglist url))
         (elem (os-buglist-to-element buglist)))
    (save-excursion
      (insert (org-element-interpret-data
               `(org-data nil ,elem))))))

(defun os-pull ()
  "Update first buglist in current buffer."
  (interactive)
  ;; since we replace the whole buffer, save-excusion doesn't work so
  ;; we manually (re)store the point
  (let* ((oldpoint (point))
         (doc (org-element-parse-buffer))
         (oldbl (os-find-buglist doc nil))
         (url (os-headline-url oldbl))
         (newbl (buglist-to-element url)))

    (os-replace-buglist doc newbl)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (org-element-interpret-data doc))
    (goto-char oldpoint)))

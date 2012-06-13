;; bitbucket backend
(require 'org-sync)

(defcustom os-bb-auth
  nil
  "Bitbucket login (\"user\" . \"pwd\")")

(defun os-bb-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
AUTH is a cons (\"user\" . \"pwd\"). Return the server
decoded response in JSON."
  (let* ((url-request-method method)
         (url-request-data data)
         (auth os-bb-auth)
         (buf))

    (if (consp auth)
        ;; dynamically bind auth related vars
        (let* ((str (concat (car auth) ":" (cdr auth)))
               (encoded (base64-encode-string str))
               (login `(("api.bitbucket.org:443" ("Bitbucket API" . ,encoded))))
               (url-basic-auth-storage 'login))
          (setq buf (url-retrieve-synchronously url)))
      ;; nothing more to bind
      (setq buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))


(defun os-bb-buglist-url (repo)
  "Return the issue API URL for REPO."
  (when (string-match "bitbucket.org/\\([^/]+\\)/\\([^/]+\\)" repo)
    (format "https://api.bitbucket.org/1.0/repositories/%s/%s/issues"
            (match-string 1 repo)
            (match-string 2 repo))))

(defun os-bb-json-to-bug (json)
  "Return JSON as a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key json)))
    (let* ((id (v 'local_id))
           (author (va 'username (v 'reported_by)))
           (assignee (va 'username (v 'responsible)))
           (txtstatus (v 'status))
           (status (if (or (string= txtstatus "open")
                           (string= txtstatus "new"))
                       'open
                     'closed))
           (title (v 'title))
           (desc (v 'content))
           (priority 2)
           (ctime (v 'utc_created_on))
           (mtime (v 'utc_last_updated)))
      
      `(:id ,id
            :assignee ,assignee
            :status ,status
            :title ,title
            :desc ,desc
            :priority ,priority ;; XXX: defvar for default priority
            :date-deadline ,dtime
            :date-creation ,ctime
            :date-modification ,mtime))))    
 
(defun os-bb-repo-name (url)
  "Return repo name at URL."
  (when (string-match "bitbucket.org/\\([^/]+\\)/\\([^/]+\\)" url)
    (match-string 2 url)))
 

;; override
(defun os-bb-fetch-buglist (repo)
  "Return the buglist at url REPO."
  (let* ((url (os-bb-buglist-url repo))
         (json (os-bb-request "GET" url))
         (title (concat "Bugs of " (os-bb-repo-name url))))

    `(:title ,title
             :url ,url
             :bugs ,(mapcar 'os-bb-json-to-bug (cdr (assoc 'issues json))))))


(defun os-bb-json-to-bug (json)
  "Return JSON as a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key json)))
    (let* ((id (v 'local_id))
           (author (va 'username (v 'reported_by)))
           (assignee (va 'username (v 'responsible)))
           (txtstatus (v 'status))
           (status (if (or (string= txtstatus "open")
                           (string= txtstatus "new"))
                       'open
                     'closed))
           (title (v 'title))
           (desc (v 'content))
           (ctime (v 'utc_created_on))
           (mtime (v 'utc_last_updated)))
      
      `(:id ,id
            :assignee ,assignee
            :status ,status
            :title ,title
            :desc ,desc
            :date-creation ,ctime
            :date-modification ,mtime))))

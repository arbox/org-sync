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
         (buf)
         (url-request-extra-headers
          (unless data
            '(("Content-Type" . "application/x-www-form-urlencoded")))))

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

;; override
(defun os-bb-base-url (url)
  "Return base URL."
  (cond
   ;; web ui url
  ((string-match "^\\(?:http://\\)?\\(?:www\\.\\)?bitbucket.org/\\([^/]+\\)/\\([^/]+\\)/?$" url)
   (concat "https://api.bitbucket.org/1.0/repositories/"
           (match-string 1 url) "/" (match-string 2 url)))
  
  ;; api url
  ((string-match "api.bitbucket.org/1.0/repositories")
   url)))
  
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
            :date-deadline ,dtime
            :date-creation ,ctime
            :date-modification ,mtime))))

(defun os-bb-bug-to-form (bug)
  "Return BUG as an form alist."
  (let* ((title (os-get-prop :title bug))
         (desc (os-get-prop :desc bug))
         (assignee (os-get-prop :assignee bug))
         (status (if (eq (os-get-prop :status bug) 'open) "open" "resolved")))
    `(("title" . ,title)
      ("status" . ,status)
      ("content" . ,desc)
      ("responsible" . ,assignee))))

(defun os-bb-post-encode (args)
  "Return form alist ARGS as a url-encoded string."
  (mapconcat (lambda (arg)
               (concat (url-hexify-string (car arg))
                       "="
                       (url-hexify-string (cdr arg))))
             args "&"))

(defun os-bb-repo-name (url)
  "Return repo name at URL."
  (when (string-match "api\\.bitbucket.org/1\\.0/repositories/\\([^/]+\\)/\\([^/]+\\)" url)
    (match-string 2 url)))

(defun os-bb-repo-user (url)
  "Return repo username at URL."
  (when (string-match "api\\.bitbucket.org/1\\.0/repositories/\\([^/]+\\)/\\([^/]+\\)" url)
    (match-string 1 url)))

;; override
(defun os-bb-fetch-buglist ()
  "Return the buglist at os-base-url."
  (let* ((url (concat os-base-url "/issues"))
         (json (os-bb-request "GET" url))
         (title (concat "Bugs of " (os-bb-repo-name url))))

    `(:title ,title
             :url ,os-base-url
             :bugs ,(mapcar 'os-bb-json-to-bug (cdr (assoc 'issues json))))))


(defun os-bb-json-to-bug (json)
  "Return JSON as a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key json)))
    (let* ((id (v 'local_id))
           (metadata (v 'metadata))
           (kind (va 'kind metadata))
           (version (va 'version metadata))
           (component (va 'component metadata))
           (milestone (va 'milestone metadata))
           (author (va 'username (v 'reported_by)))
           (assignee (va 'username (v 'responsible)))
           (txtstatus (v 'status))
           (status (if (or (string= txtstatus "open")
                           (string= txtstatus "new"))
                       'open
                     'closed))
           (priority (v 'priority))
           (title (v 'title))
           (desc (v 'content))
           (ctime (os-parse-date (v 'utc_created_on)))
           (mtime (os-parse-date (v 'utc_last_updated))))

      `(:id ,id
            :priority ,priority
            :assignee ,assignee
            :status ,status
            :title ,title
            :desc ,desc
            :date-creation ,ctime
            :date-modification ,mtime
            :kind ,kind
            :version ,version
            :component ,component
            :milestone ,milestone))))

;; override
(defun os-bb-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return an updated buglist."
  (let* ((new-url (concat os-base-url "/issues"))
         (new-bugs
          (mapcar (lambda (b)
                    (let* ((sync (os-get-prop :sync b))
                           (id (os-get-prop :id b))
                           (data (os-bb-post-encode (os-bb-bug-to-form b)))
                           (modif-url (format "%s/%d/" new-url id))
                           (result
                            (cond
                             ;; new bug
                             ((eq sync 'new)
                              (os-bb-request "POST" new-url data))

                             ;; delete bug
                             ((eq sync 'delete)
                              (os-bb-request "DELETE" modif-url))

                             ;; update bug
                             ((eq sync 'change)
                              (os-bb-request "PUT" modif-url data)))))

                      (cond
                       ;; if bug was :sync same, return it
                       ((null result)
                        b)

                       ;; else, result is the updated bug
                       (t
                        (os-bb-json-to-bug result)))))
                  (os-get-prop :bugs buglist))))

    `(:title ,(os-get-prop :title buglist)
             :url ,url
             :bugs ,new-bugs)))

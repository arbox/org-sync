;; github backend

(require 'org-sync)

(defcustom os-github-auth
  nil
  "Github login (\"user\" . \"pwd\")")

;; override
(defun os-github-fetch-buglist (last-update)
  "Return the buglist at os-base-url."
  (let* ((url (concat os-base-url "/issues?per_page=100"))
         (json (vconcat (os-github-fetch-json url)
                        (os-github-fetch-json (concat url "&state=closed"))))
         (title (concat "Bugs of " (os-github-repo-name url))))

    `(:title ,title
             :url ,os-base-url
             :bugs ,(mapcar 'os-github-json-to-bug json))))

;; override
(defun os-github-base-url (url)
  "Return base url."
  (when (string-match "github.com/\\(?:repos/\\)?\\([^/]+\\)/\\([^/]+\\)" url)
    (let ((user (match-string 1 url))
          (repo (match-string 2 url)))
    (concat "https://api.github.com/repos/" user "/" repo ""))))

;; override
(defun os-github-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return an updated buglist."
  (let* ((new-url (concat os-base-url "/issues"))
         (new-bugs
          (mapcar (lambda (b)
                    (let* ((sync (os-get-prop :sync b))
                           (id (os-get-prop :id b))
                           (data (os-github-bug-to-json b))
                           (modif-url (format "%s/%d" new-url id))
                           (result
                            (cond
                             ;; new bug
                             ((eq sync 'new)
                              (os-github-request "POST" new-url data))

                             ;; delete bug
                             ((eq sync 'delete)
                              (os-github-request "DELETE" modif-url))

                             ;; update bug
                             ((eq sync 'change)
                              (os-github-request "PATCH" modif-url data))))
                           (err (cdr (assoc 'message result))))

                      (cond
                       ;; if bug was :sync same, return it
                       ((null result)
                        b)

                       ;; if json result has a message field, error
                       ((stringp err)
                        (error "Github: %s" err))

                       ;; else, result is the updated bug
                       (t
                        (os-github-json-to-bug result)))))
                  (os-get-prop :bugs buglist))))

    `(:title ,(os-get-prop :title buglist)
             :url ,(os-get-prop :url buglist)
             :bugs ,new-bugs)))


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

(defun os-github-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
AUTH is a cons (\"user\" . \"pwd\"). Return the server
decoded response in JSON."
  (let* ((url-request-method method)
         (url-request-data data)
         (auth os-github-auth)
         (buf))

    (if (consp auth)
        ;; dynamically bind auth related vars
        (let* ((str (concat (car auth) ":" (cdr auth)))
               (encoded (base64-encode-string str))
               (login `(("api.github.com:443" ("Github API" . ,encoded))))
               (url-basic-auth-storage 'login))
          (setq buf (url-retrieve-synchronously url)))
      ;; nothing more to bind
      (setq buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(defun os-github-repo-name (url)
  "Return the name of the repo at URL."
  (if (string-match "github.com/repos/[^/]+/\\([^/]+\\)" url)
      (match-string 1 url)
    "<project name>"))

(defun os-github-filter-desc (desc)
  "Return a filtered description of a GitHub description."
  (replace-regexp-in-string "\\([^ \t\n]\\)[ \t\n]*\\'"
                            "\\1\n"
                            (replace-regexp-in-string "\r\n" "\n" desc)))

(defun os-github-json-to-bug (data)
  "Return DATA (in json) converted to a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key data)))
    (let* ((id (v 'number))
           (stat (if (string= (v 'state) "open") 'open 'closed))
           (title (v 'title))
           (desc  (os-github-filter-desc (v 'body)))
           (author (va 'login (v 'user)))
           (assignee (va 'login (v 'assignee)))
           (milestone-alist (v 'milestone))
           (milestone (va 'title milestone-alist))
           (ctime (os-parse-date (v 'created_at)))
           (dtime (os-parse-date (va 'due_on milestone-alist)))
           (mtime (os-parse-date (v 'updated_at)))
           (tags (mapcar (lambda (e)
                           (va 'name e)) (v 'labels))))

      `(:id ,id
            :author ,author
            :assignee ,assignee
            :status ,stat
            :title ,title
            :desc ,desc
            :milestone ,milestone
            :tags ,tags
            :date-deadline ,dtime
            :date-creation ,ctime
            :date-modification ,mtime))))

(defun os-github-bug-to-json (bug)
  "Return BUG as JSON."
  (json-encode
   `((title . ,(os-get-prop :title bug))
     (body . ,(os-get-prop :desc bug))
     (assignee . ,(os-get-prop :assignee bug))
     (state . ,(symbol-name (os-get-prop :status bug)))
     (labels . [ ,@(os-get-prop :tags bug) ]))))

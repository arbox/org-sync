;; simple tool that downloads buglist from GitHub bugtracker

;; fast (small repo):
;; M-x os-import RET github.com/octocat/Hello-World RET

;; slow because of synchroneous download (~4sec):
;; M-x os-import RET github.com/joyent/node RET

;; update every buglist in current document
;; M-x os-sync

;; buglist data structure

;; '(:title "My buglist"
;;   :url "http://github.com/repos/octocat/Hello-World"
;;   :bugs (BUGS...))

;; bug data structure
;; '(:id 3
;;   :status 'open ;; or 'closed
;;   :sync 'delete ;; or 'change 'conflict-local 'conflict-remote 'same (= nil)
;;   :title "foo"
;;   :desc "blah"
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

(require 'org-element)
(require 'cl)
(require 'json)
(require 'url)

(defcustom os-github-auth
  nil
  "Github login (\"user\" . \"pwd\")")

(defconst os-buglist-properties
  '(title url)
  "List of shared buglist properties.")

(defconst os-bug-properties
  '(id status title priority tags date-deadline date-creation
  date-modification author assignee desc)
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

(defun os-set-prop (key val b)
  "Set KEY to VAL in buglist or bug B."
  (plist-put b key val))

(defun os-github-user-repo-from-url (url)
  "Return a cons (username . repo-name) extracted from URL."
  (when (string-match "github.com/\\(?:repos/\\)?\\([^/]+\\)/\\([^/]+\\)" url)
    (list (match-string 1 url) (match-string 2 url))))

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

(defun os-github-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return an updated buglist."
  (let* ((url (os-get-prop :url buglist))
         (ret (os-github-user-repo-from-url url))
         (user (first ret))
         (repo (second ret))
         (new-url
          (concat "https://api.github.com/repos/" user "/" repo "/issues"))
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


(defun os-github-bug-to-json (bug)
  "Return BUG as JSON."
  (json-encode
   `((title . ,(os-get-prop :title bug))
     (body . ,(os-get-prop :desc bug))
     (assignee . ,(os-get-prop :assignee bug))
     (state . ,(symbol-name (os-get-prop :status bug)))
     (labels . [ ,@(os-get-prop :tags bug) ]))))

(defun os-buglist-to-element (bl)
  "Returns buglist BL as an element."
  (let* ((elist (delq nil (mapcar 'os-bug-to-element (os-get-prop :bugs bl))))
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
  (let (plist)
    (dolist (x properties)
      (let* ((p (os-propertize x))
             (val (os-get-prop p bug)))
        (when val
          (setq plist (cons (cons (if prefix p x) val) plist)))))
    plist))

(defun os-bug-to-element (b)
  "Return bug B as a TODO element if it is visible, nil otherwise."
  (let* ((skip '(title status desc sync))
         (props (os-filter-list os-bug-properties skip))
         (plist (os-plist props b))
         (backend-plist (os-plist os-github-bug-properties b)))

    (unless (eq 'delete (os-get-prop :sync b))
      `(headline
        (:title ,(os-get-prop :title b)
                :level 2
                :todo-type todo
                :todo-keyword ,(upcase (symbol-name (os-get-prop :status b))))

        (section
         nil
         (property-drawer
          (:properties (,@plist ,@backend-plist)))
         (paragraph nil ,(os-github-filter-desc (os-get-prop :desc b))))))))

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
              (org-element-property
               :properties
               (car (org-element-contents
                     (car (org-element-contents e))))))))

(defun os-buglist-headline-p (elem)
  "Return t if ELEM is a buglist headline."
  (and
   (eq (org-element-type elem) 'headline)
   (stringp (os-headline-url elem))))

(defun os-headline-to-buglist (h)
  "Return headline H as a buglist."
  (let* ((alist (org-element-property
                 :properties
                 (car (org-element-contents
                       (car (org-element-contents h))))))
         (title (org-element-property :title h))
         (url (cdr (assoc "url" alist))))
    `(:title ,title
             :url ,url
             :bugs ,(mapcar
                     'os-headline-to-bug
                     (nthcdr 1 (org-element-contents h))))))

(defun os-headline-to-bug (h)
  "Return headline H as a bug."
  ;; value of "sym" in alist a
  (flet ((va (k a) (cdr (assoc (symbol-name k) a)))
         (str-to-n (x) (if (stringp x) (string-to-number x) -1)))
    (let* ((titlecons (org-element-property :title h))
           (title (car titlecons))
           (status (if (string= "OPEN"
                                (org-element-property :todo-keyword h))
                       'open
                     'closed))
           (sync (when (string=
                        "DELETE" (org-element-property :todo-keyword h))
                   'delete))
           (desc
            (org-element-interpret-data
             (nthcdr 1 (org-element-contents (car (org-element-contents h))))))
           (headline-alist (org-element-property
                            :properties
                            (car
                             (org-element-contents
                              (car (org-element-contents h))))))
           (id (str-to-n (va 'id  headline-alist)))
           (priority (str-to-n (va 'priority headline-alist)))
           (tags-str (va 'tags headline-alist))
           (tags (when (stringp tags-str)
                   (mapcar 'symbol-name (read tags-str))))
           (author (va 'author headline-alist))
           (assignee (va 'assignee headline-alist))
           (dtime (va 'date-deadline headline-alist))
           (ctime (va 'date-creation headline-alist))
           (mtime (va 'date-modification headline-alist))
           (backend-plist (mapcar (lambda (x)
                                    (cons
                                     (os-propertize x)
                                     (va x headline-alist)))
                                  os-github-bug-properties)))
      `(:id ,id
            :desc ,desc
            :status ,status
            :sync ,sync
            :title ,title
            :priority ,priority
            :tags ,tags
            :author ,author
            :assignee ,assignee
            :date-deadline ,dtime
            :date-creation ,ctime
            :date-modification ,mtime
            ,@backend-plist))))

(defun os-find-buglists (elem)
  "Return every buglist headlines in ELEM."
  (let ((type (org-element-type elem))
        (contents (org-element-contents elem)))
    (cond
     ;; if it's a buglist, return it
     ((os-buglist-headline-p elem)
      elem)
     ;; else if it contains elements, look recursively in it
     ((or (eq type 'org-data) (memq type org-element-greater-elements))
      (let (buglist)
        (mapc (lambda (e)
                (let ((h (os-find-buglists e)))
                  (when h
                    (setq buglist (cons h buglist)))))
              contents)
        buglist))
     ;; terminal case
     (t
      nil))))

(defun os-add-keyword (tree key val)
  "Add KEY:VAL as a header in TREE by side-effects and return TREE.
If KEY is already equal to VAL, no change is made."
  (catch :exit
    (let* ((section (first (org-element-contents tree))))
      (when (and (eq 'org-data (org-element-type tree))
                 (eq 'section  (org-element-type section)))

        (dolist (e (org-element-contents section))
          (let* ((type (org-element-type e))
                 (ekey (org-element-property :key e))
                 (eval (org-element-property :value e)))

            (when (and (eq 'keyword type)
                       (string= ekey key)
                       (string= eval val))
              (throw :exit nil))))

        (setf (nthcdr 2 section)
              (cons
               `(keyword (:key ,key :value ,val))
               (org-element-contents section))))))
  tree)


(defun os-import (url)
  "Fetch and insert bugs from URL."
  (interactive "sURL: ")
  (let* ((buglist (os-github-fetch-buglist url))
         (elem (os-buglist-to-element buglist))
         (bug-keyword '(sequence "OPEN" "|" "CLOSED")))
    (save-excursion
      (insert (org-element-interpret-data
               `(org-data nil ,elem)))

      (unless (member bug-keyword org-todo-keywords)
        (goto-char (point-min))
        (insert "#+TODO: OPEN | CLOSED\n")
        (add-to-list 'org-todo-keywords '(sequence "OPEN" "|" "CLOSED"))

        ;; the buffer has to be reparsed in order to have the new
        ;; keyword taken into account
        ;; from org-ctrl-c-ctrl-c, thanks to vsync in #org-mode
        (let ((org-inhibit-startup-visibility-stuff t)
              (org-startup-align-all-tables nil))
          (when (boundp 'org-table-coordinate-overlays)
            (mapc 'delete-overlay org-table-coordinate-overlays)
            (setq org-table-coordinate-overlays nil))
          (org-save-outline-visibility 'use-markers (org-mode-restart)))))))

(defun os-get-bug-id (buglist id)
  "Return bug ID from BUGLIST."
  (catch :exit
    (mapc (lambda (x)
            (when (= (os-get-prop :id x) id)
              (throw :exit x)))
          (os-get-prop :bugs buglist))
    nil))

(defun os-buglist-contains-dups (buglist)
  "Return t if BUGLIST contains bugs with the same id."
  (let* ((len 0)
         (ids (mapcar
              (lambda (x)
                (incf len)
                (os-get-prop :id x))
              (os-get-prop :bugs buglist))))
    (delete-dups ids)
    (/= len (length ids))))

(defun os-merge-buglist (local remote)
  "Return a buglist merged from buglist LOCAL & REMOTE."
  (let* ((local-bugs (os-get-prop :bugs local))
         (remote-bugs (os-get-prop :bugs remote))
         (merged-bugs))

    (when (os-buglist-contains-dups local)
      (error "Buglist \"%s\" contains unmerged bugs." (car (os-get-prop :title local))))

    ;; A. handle local bugs and fill the merged-bugs list
    (dolist (loc local-bugs)
      (let* ((id (os-get-prop :id loc))
             (rem (os-get-bug-id remote id)))

        (cond
         ;; if the bug doesn't exist in remote, it's a new one or it
         ;; was deleted
         ((null rem)
          ;; if the remote bug doesn't exist but the local one has a
          ;; valid id, it means the remote one was deleted, ignore it
          (unless (and (numberp id) (>= id 0))
            (os-set-prop :sync 'new loc)
            (setq merged-bugs (append merged-bugs (list loc))))

         ;; if the bug was marked to be deleted, insert it but don't
         ;; display it
         ((eq 'delete (os-get-prop :sync loc))
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; if local bug = remote bug, nothing to sync
         ((os-bug-equalp loc rem)
          (os-set-prop :sync 'same loc)
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; if local != remote but the modification date is equal, we
         ;; keep the local and push it. we keep a ref to the remote to
         ;; compute a diff when we push
         ((os-bug-prop-equalp :date-modification loc rem)
          (os-set-prop :sync 'change loc)
          (os-set-prop :old-bug rem loc)
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; local and remote differ, insert both and let the user merge
         (t
          (os-set-prop :sync 'conflict-local  loc)
          (os-set-prop :sync 'conflict-remote rem)
          (setq merged-bugs (append merged-bugs (list loc rem)))))))

    ;; B. handle remote bugs and complete the merged-bugs list
    (dolist (rem remote-bugs)
      (let* ((id (os-get-prop :id rem))
             (loc (os-get-bug-id local id)))

        (cond
         ;; if local bug = remote bug => already handled in step A.
         ;; local and remote differ   => already handled in step A.

         ;; if the bug doesn't exist in local, it's a new one
         ;; don't resend it, and insert it
         ((null loc)
          (os-set-prop :sync 'same rem)
          (setq merged-bugs (append merged-bugs (list rem)))))))

    ;; return new merged buglist
    `(:title ,(os-get-prop :title local) ;; always keep local title
             :url ,(os-get-prop :url local)
             :bugs ,merged-bugs)))

(defun os-set-equal (a b)
  "Return t if list A and B have the same elements, no matter the order."
  (catch :exit
    (mapc (lambda (e)
            (unless (member e b)
              (throw :exit nil)))
          a)
    (mapc (lambda (e)
            (unless (member e a)
              (throw :exit nil)))
          b)
    t))

(defun os-bug-diff (a b)
  "Return a list of properties that differs in A and B."
  (let ((diff)
        (props
        '(:id :status :title :desc :priority :author :assignee
              :date-deadline :date-modification :date-creation)))
    (dolist (p props)
      (unless (os-bug-prop-equalp p a b)
        (setq diff (append diff `(,p (,(os-get-prop p a) ,(os-get-prop p b)))))))

    (let ((tag-a (os-get-prop :tags a))
          (tag-b (os-get-prop :tags b)))
      (unless (os-set-equal tag-a tag-b)
        (setq diff (append diff `(:tags ,tag-a ,tab-b)))))
    diff))

(defun os-bug-equalp (a b)
  "Return t if A = B."
  (flet ((peq (p) (os-bug-prop-equalp p a b)))
    (and
     (peq :id)
     (peq :status)
     (peq :title)
     (peq :desc)
     (peq :priority)
     (peq :author)
     (peq :assignee)
     (peq :date-deadline)
     (peq :date-modification)
     (peq :date-creation)
     (os-set-equal (os-get-prop :tags a) (os-get-prop :tags b)))))

(defun os-buglist-equalp (a b)
  "Return t if A = B."
  (when (os-bug-prop-equalp :url a b)
      (catch :exit
        (mapcar*
         (lambda (a b)
           (unless (os-bug-equalp a b)
             (thow :exit nil)))
         (os-get-prop :bugs a)
         (os-get-prop :bugs b))
        t)))

(defun os-bug-prop-equalp (prop a b)
  "Return t if bug A PROP = bug B PROP, nil otherwise."
  (equal (os-get-prop prop a) (os-get-prop prop b)))

(defun os-push (buglist)
  "Send the new BUGLIST to the bugtracker."
  (os-github-send-buglist buglist))

(defun os-sync ()
  "Update buglists in current buffer."
  (interactive)
  (let* ((local-doc (org-element-parse-buffer))
         (local-headlines (os-find-buglists local-doc))
         (local-buglists (mapcar 'os-headline-to-buglist local-headlines))
         (local-urls (mapcar (lambda (x) (os-get-prop :url x)) local-buglists))
         (remote-buglists (mapcar 'os-github-fetch-buglist local-urls))
         (merged-buglists (mapcar*
                           'os-merge-buglist local-buglists remote-buglists))
         (updated-merged-buglists (mapcar 'os-push merged-buglists))
         (merged-headlines (mapcar 'os-buglist-to-element updated-merged-buglists)))

    ;; replace headlines in local-doc
    (mapcar* (lambda (a b) (setf (car a) (car b) (cdr a) (cdr b)))
             local-headlines merged-headlines)

    (os-add-keyword local-doc "TODO" "OPEN | CLOSED")

    ;; since we replace the whole buffer, save-excusion doesn't work so
    ;; we manually (re)store the point
    (let ((oldpoint (point)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (org-element-interpret-data local-doc))
      (goto-char oldpoint))))

(defun os-sync-debug (&optional sym)
  (let* ((local-doc (org-element-parse-buffer))
         (local-headlines (os-find-buglists local-doc))
         (local-buglists (mapcar 'os-headline-to-buglist local-headlines))
         (local-urls (mapcar (lambda (x) (os-get-prop :url x)) local-buglists))
         (remote-buglists (mapcar 'os-github-fetch-buglist local-urls))
         (merged-buglists (mapcar*
                           'os-merge-buglist local-buglists remote-buglists))
         );(updated-merged-buglists (mapcar 'os-push merged-buglists))
         ;(merged-headlines (mapcar 'os-buglist-to-element updated-merged-buglists))))

    (cond
     ((eq sym 'headline)
      merged-headlines)
     ((eq sym 'local)
      local-buglists)
     ((eq sym 'remote)
      remote-buglists)
     ((eq sym 'merged)
      merged-buglists)
     (t
      (list
       local-buglists remote-buglists merged-buglists merged-headlines)))))

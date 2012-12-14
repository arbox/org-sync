;;; os-github.el --- Redmine backend for org-sync.

;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, github, synchronization
;; Homepage: http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/org-sync
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is not part of GNU Emacs.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a backend for org-sync to synchnonize
;; issues from a github tracker with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.

;; This backend supports basic bug synching along with tag creation.
;; If you add or change the tags of an issue to something that doesn't
;; exists, it will be created.

;;; Code:

(eval-when-compile (require 'cl))
(require 'url)
(require 'os)
(require 'json)

(defvar os-github-backend
  '((base-url      . os-github-base-url)
    (fetch-buglist . os-github-fetch-buglist)
    (send-buglist  . os-github-send-buglist))
  "Github backend.")

(defvar url-http-end-of-headers)

(defvar os-github-auth nil
  "Github login (\"user\" . \"pwd\")")

(defun os-github-fetch-labels ()
  "Return list of labels at os-base-url."
  (let* ((url (concat os-base-url "/labels"))
         (json (os-github-fetch-json url)))
    (mapcar (lambda (x)
              (cdr (assoc 'name x)))
            json)))

(defun os-github-random-color ()
  "Return a random hex color code 6 characters string without #."
  (random t)
  (format "%02X%02X%02X" (random 256) (random 256) (random 256)))

(defun os-github-color-p (color)
  "Return non-nil if COLOR is a valid color code."
  (and (stringp color) (string-match "^[0-9a-fA-F]\\{6\\}$" color)))

(defun os-github-create-label (label &optional color)
  "Create new COLOR LABEL at os-base-url and return it.

LABEL must be a string.  COLOR must be a 6 characters string
containing a hex color code without the #.  Take a random color
when not given."
  (let* ((url (concat os-base-url "/labels"))
         (json (json-encode `((name . ,label)
                              (color . ,(if (os-github-color-p color)
                                            color
                                          (os-github-random-color)))))))
    (os-github-request "POST" url json)))

(defun os-github-handle-tags (bug existing-tags)
  "Create any label in BUG that is not in EXISTING-TAGS.

Append new tags in EXISTING-TAGS by side effects."
  (let* ((tags (os-get-prop :tags bug)))
    (dolist (tag tags)
      (when (os-append! tag existing-tags)
        (os-github-create-label tag)))))

(defun os-github-time-to-string (time)
  "Return TIME as a full ISO 8601 date string, but without timezone adjustments (which github doesn't support"
  (format-time-string "%Y-%m-%dT%TZ" time t))

;; override
(defun os-github-fetch-buglist (last-update)
  "Return the buglist at os-base-url."
  (let* ((since (when last-update
                  (format "&since=%s" (os-github-time-to-string last-update))))
         (url (concat os-base-url "/issues?per_page=100" since))
         (json (vconcat (os-github-fetch-json url)
                        (os-github-fetch-json (concat url "&state=closed"))))
         (title (concat "Bugs of " (os-github-repo-name url))))

    `(:title ,title
             :url ,os-base-url
             :bugs ,(mapcar 'os-github-json-to-bug json)
             :since ,last-update)))

;; override
(defun os-github-base-url (url)
  "Return base url."
  (when (string-match "github.com/\\(?:repos/\\)?\\([^/]+\\)/\\([^/]+\\)" url)
    (let ((user (match-string 1 url))
          (repo (match-string 2 url)))
    (concat "https://api.github.com/repos/" user "/" repo ""))))

;; override
(defun os-github-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return new bugs."
  (let* ((new-url (concat os-base-url "/issues"))
         (existing-tags (os-github-fetch-labels))
         (newbugs))
    (dolist (b (os-get-prop :bugs buglist))
      (let* ((sync (os-get-prop :sync b))
             (id (os-get-prop :id b))
             (data (os-github-bug-to-json b))
             (modif-url (format "%s/%d" new-url (or id 0)))
             (result
              (cond
               ;; new bug
               ((null id)
                (os-github-handle-tags b existing-tags)
                (push (os-github-json-to-bug
                       (os-github-request "POST" new-url data)) newbugs))

               ;; update bug
               (t
                (os-github-handle-tags b existing-tags)
                (os-github-request "PATCH" modif-url data))))
             (err (cdr (assoc 'message result))))

        (when (stringp err)
          (error "Github: %s" err))))
    `(:bugs ,newbugs)))

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

(defun os-github-url-retrieve-synchronously (url)
  "Retrieve the specified url using authentication data from
os-github-auth. AUTH is a cons (\"user\" . \"pwd\")."
  (let ((auth os-github-auth))
    (if (consp auth)
        ;; dynamically bind auth related vars
        (let* ((str (concat (car auth) ":" (cdr auth)))
               (encoded (base64-encode-string str))
               (login `(("api.github.com:443" ("Github API" . ,encoded))))
               (url-basic-auth-storage 'login))
          (url-retrieve-synchronously url))
      ;; nothing more to bind
      (url-retrieve-synchronously url))))

(defun os-github-fetch-json-page (url)
  "Return a cons (JSON object from URL . next page url)."
  (let ((download-buffer (os-github-url-retrieve-synchronously url))
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
Return the server decoded JSON response."
  (message "%s %s %s" method url (prin1-to-string data))
  (let* ((url-request-method method)
         (url-request-data data)
         (buf (os-github-url-retrieve-synchronously url)))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(defun os-github-repo-name (url)
  "Return the name of the repo at URL."
  (if (string-match "github.com/repos/[^/]+/\\([^/]+\\)" url)
      (match-string 1 url)
    "<project name>"))

;; XXX: we need an actual markdown parser here...
(defun os-github-filter-desc (desc)
  "Return a filtered description of a GitHub description."
  (setq desc (replace-regexp-in-string "\r\n" "\n" desc))
  (setq desc (replace-regexp-in-string "\\([^ \t\n]\\)[ \t\n]*\\'"
                                       "\\1\n" desc)))          

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
  (let ((state (os-get-prop :status bug)))
    (unless (member state '(open closed))
      (error "Github: unsupported state \"%s\"" (symbol-name state)))

  (json-encode
   `((title . ,(os-get-prop :title bug))
     (body . ,(os-get-prop :desc bug))
     (assignee . ,(os-get-prop :assignee bug))
     (state . ,(symbol-name (os-get-prop :status bug)))
     (labels . [ ,@(os-get-prop :tags bug) ])))))

;;; os-github.el ends here

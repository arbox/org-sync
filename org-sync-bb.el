;;; org-sync-bb.el --- Bitbucket backend for org-sync.
;;
;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, bitbucket, synchronization
;; Homepage: https://github.com/arbox/org-sync
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package implements a backend for org-sync to synchnonize
;; issues from a bitbucket repo with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.
;;
;; This backend only supports basic synchronization for now.
;; Components, versions and milestones are ignored.
;;
;;; Code:

(eval-when-compile (require 'cl))
(require 'org-sync)
(require 'url)
(require 'json)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar org-sync-bb-backend
  '((base-url      . org-sync-bb-base-url)
    (fetch-buglist . org-sync-bb-fetch-buglist)
    (send-buglist  . org-sync-bb-send-buglist))
  "Bitbucket backend.")

(defvar org-sync-bb-auth nil
  "Bitbucket login (\"user\" . \"pwd\")")

(defun org-sync-bb-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
AUTH is a cons (\"user\" . \"pwd\").  Return the server
decoded response in JSON."
  (message "%s %s %s" method url (prin1-to-string data))
  (let* ((url-request-method method)
         (url-request-data data)
         (auth org-sync-bb-auth)
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
      (prog1
          (cons url-http-response-status (ignore-errors (json-read)))
        (kill-buffer)))))

;; override
(defun org-sync-bb-base-url (url)
  "Return base URL."
  (cond
   ;; web ui url
  ((string-match "^\\(?:https?://\\)?\\(?:www\\.\\)?bitbucket.org/\\([^/]+\\)/\\([^/]+\\)/?$" url)
   (concat "https://api.bitbucket.org/1.0/repositories/"
           (match-string 1 url) "/" (match-string 2 url)))

  ;; api url
  ((string-match "api.bitbucket.org/1.0/repositories" url)
   url)))


;; From https://confluence.atlassian.com/display/BITBUCKET/Issues

;;     title: The title of the new issue.
;;     content: The content of the new issue.
;;     component: The component associated with the issue.
;;     milestone: The milestone associated with the issue.
;;     version: The version associated with the issue.
;;     responsible: The username of the person responsible for the issue.

;;     priority: The priority of the issue.  Valid priorities are:
;;     - trivial
;;     - minor
;;     - major
;;     - critical
;;     - blocker

;;     status: The status of the issue.  Valid statuses are:
;;     - new
;;     - open
;;     - resolved
;;     - on hold
;;     - invalid
;;     - duplicate
;;     - wontfix

;;     kind: The kind of issue.  Valid kinds are:
;;     - bug
;;     - enhancement
;;     - proposal
;;     - task

(defconst org-sync-bb-priority-list
  '("trivial" "minor" "major" "critical" "blocker")
  "List of valid priority for a bitbucket issue.")

(defconst org-sync-bb-status-list
  '("new" "open" "resolved" "on hold" "invalid" "duplicate" "wontfix")
  "List of valid status for a bitbucket issue.")

(defconst org-sync-bb-kind-list
  '("bug" "enhancement" "proposal" "task")
  "List of valid kind for a bitbucket issue.")

(defun org-sync-bb-bug-to-form (bug)
  "Return BUG as an form alist."
  (let* ((priority (org-sync-get-prop :priority bug))
         (title (org-sync-get-prop :title bug))
         (desc (org-sync-get-prop :desc bug))
         (assignee (org-sync-get-prop :assignee bug))
         (status (if (eq (org-sync-get-prop :status bug) 'open) "open" "resolved"))
         (kind (org-sync-get-prop :kind bug)))

    (if (and priority (not (member priority org-sync-bb-priority-list)))
      (error "Invalid priority \"%s\" at bug \"%s\"." priority title))

    (if (and kind (not (member kind org-sync-bb-kind-list)))
      (error "Invalid kind \"%s\" at bug \"%s\"." kind title))

    (cl-remove-if (lambda (x)
                    (null (cdr x)))
                  `(("title"       . ,title)
                    ("status"      . ,status)
                    ("content"     . ,desc)
                    ("responsible" . ,assignee)
                    ("priority"    . ,priority)
                    ("kind"        . ,kind)))))

(defun org-sync-bb-post-encode (args)
  "Return form alist ARGS as a url-encoded string."
  (mapconcat (lambda (arg)
               (concat (url-hexify-string (car arg))
                       "="
                       (url-hexify-string (cdr arg))))
             args "&"))

(defun org-sync-bb-repo-name (url)
  "Return repo name at URL."
  (when (string-match "api\\.bitbucket.org/1\\.0/repositories/\\([^/]+\\)/\\([^/]+\\)" url)
    (match-string 2 url)))

(defun org-sync-bb-repo-user (url)
  "Return repo username at URL."
  (when (string-match "api\\.bitbucket.org/1\\.0/repositories/\\([^/]+\\)/\\([^/]+\\)" url)
    (match-string 1 url)))

;; override
(defun org-sync-bb-fetch-buglist (last-update)
  "Return the buglist at org-sync-base-url."
  (let* ((url (concat org-sync-base-url "/issues"))
         (res (org-sync-bb-request "GET" url))
         (code (car res))
         (json (cdr res))
         (title (concat "Issues of " (org-sync-bb-repo-name url))))

    `(:title ,title
             :url ,org-sync-base-url
             :bugs ,(mapcar 'org-sync-bb-json-to-bug (cdr (assoc 'issues json))))))


(defun org-sync-bb-json-to-bug (json)
  "Return JSON as a bug."
  (cl-flet* ((va (key alist) (cdr (assoc key alist)))
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
           (ctime (org-sync-parse-date (v 'utc_created_on)))
           (mtime (org-sync-parse-date (v 'utc_last_updated))))

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
(defun org-sync-bb-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return new bugs."
  (let* ((new-url (concat org-sync-base-url "/issues"))
         (new-bugs))
    (dolist (b (org-sync-get-prop :bugs buglist))
      (let* ((id (org-sync-get-prop :id b))
             (data (org-sync-bb-post-encode (org-sync-bb-bug-to-form b)))
             (modif-url (format "%s/%d/" new-url (or id 0)))
             res)
        (cond
         ;; new bug
         ((null id)
          (setq res (org-sync-bb-request "POST" new-url data))
          (when (/= (car res) 200)
            (error "Can't create new bug \"%s\"" (org-sync-get-prop :title b)))
          (push (org-sync-bb-json-to-bug (cdr res)) new-bugs))

         ;; delete bug
         ((org-sync-get-prop :delete b)
          (setq res (org-sync-bb-request "DELETE" modif-url))
          (when (not (member (car res) '(404 204)))
            (error "Can't delete bug #%d" id)))

         ;; update bug
         (t
          (setq res (org-sync-bb-request "PUT" modif-url data))
          (when (/= (car res) 200)
            (error "Can't update bug #%id" id))
          (push (org-sync-bb-json-to-bug (cdr res)) new-bugs)))))
    `(:bugs ,new-bugs)))

(provide 'org-sync-bb)
;;; org-sync-bb.el ends here

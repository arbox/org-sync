;;; org-sync-gitlab --- Gitlab backend for org-sync
;;
;; Copyright (C) 2017  Yisrael Dov Lebow
;;
;; Author:  Yisrael Dov Lebow <lebow at lebowtech dot com>
;; Keywords: org, gitlab, synchronization
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
;; This package implements a gitlab backend for org-sync to synchronize issues
;; from a Gitlab repo with an org-mode buffer. Read Org-sync documentation for
;; more information about it.
;;
;; This backend only supports basic synchronization for now. Components,
;; versions, tags and milestones are ignored.
;;
;; You must set the org-sync-gitlab-auth-token before you can sync.
;;
;; Specify the URL to your project, for example: https://gitlab.com/group/project
;; Do not include any ending like /issues or similar.
;;
;; The org-sync backend list (org-sync-backend-alist) is setup per default to
;; associate any URL containing 'gitlab' with the gitlab backend, matching both
;; the public gitlab.com but also a private gitlab instance as long as 'gitlab'
;; is in the URL. If you are using a private GitLab instance that does not
;; include 'gitlab' in the URL, you can add an entry to org-sync-backend-alist
;; to forcibly associate the FQDN of your instance with the gitlab backend:
;;
;; (add-to-list 'org-sync-backend-alist (cons "private-instance.example.com" 'org-sync-gitlab-backend))
;;
;;; Code:


(eval-when-compile (require 'cl))
(require 'org-sync)
(require 'url)
(require 'json)

(defvar org-sync-gitlab-backend
  '((base-url      . org-sync-gitlab-base-url)
    (fetch-buglist . org-sync-gitlab-fetch-buglist)
    (send-buglist  . org-sync-gitlab-send-buglist))
  "Gitlab backend.")

(defun org-sync-gitlab-base-url (url)
  "Return base URL."
  ;; if no url type, try https
  (when (not (string-match "^https?://" url))
    (setq url (concat "https://" url)))
  url)

(defun org-sync-fqdn-from-url (url)
  "Return FQDN part from a URL, effectively stripping leading https:// and the path of the URL"
  (string-match "/\\([^/]+\\)/?" url)
  (match-string 1 url))

(defun org-sync-gitlab-api-url ()
  "Gets the api url from the base-url"
  (let ((url org-sync-base-url))
    (let ((fqdn (org-sync-fqdn-from-url url)))
      (string-match (concat fqdn "/\\([^/]+\\)/\\([^/]+\\)/?$") url)
      (concat "https://" fqdn "/api/v4/projects/"
              (match-string 1 url) "%2F" (match-string 2 url) "/" ))))

;; override
(defun org-sync-gitlab-fetch-buglist (last-update)
  "Return the buglist at org-sync-base-url."
  ;;TODO implement SINCE
  ;;TODO get name for task list from url 
  `(:title "Tasks"
           :url ,org-sync-base-url
           :bugs ,(org-sync-gitlab-fetch-bugs last-update)
           ))

(defun org-sync-gitlab-fetch-bugs (last-update)
  "Return the json bugs."
  ;;TODO impliment LAST-UPDATE
  (let
      ((jsonBugs (org-sync-gitlab-request
                  "GET"
                  (concat (org-sync-gitlab-api-url) "issues?per_page=100"))))
    (mapcar 'org-sync-gitlab-json-to-bug jsonBugs)
    )
  )

;; override
(defun org-sync-gitlab-send-buglist (buglist)
  "Send a BUGLIST to gitlab and return updated BUGLIST."
  (dolist (b (org-sync-get-prop :bugs buglist))
    (let*
        (
         (id (org-sync-get-prop :id b))
         (issuePath (concat "issues/" (if id (number-to-string id ))))
         (state_event (if (string= (org-sync-get-prop :status b) 'open) "reopen" "close"))
         (issueDataJson (json-encode `((title . ,(org-sync-get-prop :title b))
                                       (description . ,(org-sync-get-prop :desc b))
                                       (state_event . ,state_event)
                                       )))
         )
      (cond
       ;; new bug (no id)
       ((null id)
        (org-sync-gitlab-request-json
         "POST"
         (concat (org-sync-gitlab-api-url)
                 issuePath)
         issueDataJson
         '(("Content-Type" . "application/json"))))

       ;; delete bug
       ((org-sync-get-prop :delete b)
        (org-sync-gitlab-request
         "DELETE"
         (concat (org-sync-gitlab-api-url)
                 issuePath
                 )))

       ;; else, modified bug
       (t
        (org-sync-gitlab-request
         "PUT"
         (concat (org-sync-gitlab-api-url)
                 issuePath)
         issueDataJson
         '(("Content-Type" . "application/json")))))
      )
    )
  ;;brute force update bugs
  ;;TODO be smarter and only show updated bugs
  `(:bugs ,(org-sync-gitlab-fetch-bugs (org-sync-get-prop :since
                                                          buglist)))
  )


(defun org-sync-gitlab-json-to-bug (data)
  "Convert the provided Json DATA into a bug."
  `(
    :id ,(assoc-default `iid  data) ;; iid is the internal issue id, used for updating
        :title ,(assoc-default `title data)
        :status, (if (string= (assoc-default `state data) "opened") 'open 'closed)
        :date-creation ,(org-sync-parse-date (assoc-default 'created_at data))
        :date-modification ,(org-sync-parse-date (assoc-default 'updated_at data))
        :web-url ,(assoc-default 'web_url data)
        :weight ,(assoc-default 'weight data)
        :desc, (assoc-default `description data)))


(defun org-sync-gitlab-request (method url &optional data extra-headers)
  "Sends HTTP request at URL using METHOD with DATA
Return a JSON response"
  ;; TODO implement error handling - we have to check we get 200 OK back from
  ;; server before trying to parse
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers (append `(("Private-Token" .  ,(org-sync-gitlab-get-auth-token)))
                                            extra-headers))
         )
    (message "%s %s %s" method url (prin1-to-string data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (prog1
          (if
              (not (string-equal method "DELETE" ))
              (json-read)
            )
        (kill-buffer)
        ))))

(defun org-sync-gitlab-get-auth-token ()
  "Gets the private-token."
  ;;TODO: prompt the user for auth token
  (unless  org-sync-gitlab-auth-token
    (error "Please set org-sync-gitlab-auth-token"))
  org-sync-gitlab-auth-token
  )

(provide 'org-sync-gitlab)
 ;;; org-sync-gitlab ends here



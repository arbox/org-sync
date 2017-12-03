;;; org-sync-gitlab --- Gitalab backend for org-sync
;;
;; Copyright (C) 2017  Yisrael Dov Lebow
;;
;; Author:  Yisrael Dov Lebow <lebow at lebowtech dot com>
;; Keywords: org, gitlab, synchronization
;; Homepage: https://github.com/yisraeldov/org-sync
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

(defvar org-sync-gitlab-backend
  '((base-url      . org-sync-gitlab-base-url)
    (fetch-buglist . org-sync-gitlab-fetch-buglist)
    (send-buglist  . org-sync-gitlab-send-buglist))
  "Gitlab backend.")

(defvar org-sync-gitlab-domain
  "gitlab.com"
  "The domain for gitlab.")

;; override
(defun org-sync-gitlab-base-url (url)
  "Return base URL."
  url)

(defun org-sync-gitlab-api-url ()
  "Gets the api url from the base-url"
  (let ((url org-sync-base-url))
    (string-match (concat  org-sync-gitlab-domain "/\\([^/]+\\)/\\([^/]+\\)/?$")  url)
    (concat "https://" org-sync-gitlab-domain "/api/v4/projects/"
	    (match-string 1 url) "%2F" (match-string 2 url) "/" )))

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
  "Send a  BUGLIST to the bugtracker and return new bugs"
  ;;TODO impliment org-sync-gitlab-send-buglist
  nil)

(defun org-sync-gitlab-json-to-bug (data)
  "Convert the provided Json DATA into a bug."
  `(
    :id ,(assoc-default `id  data)
	:title ,(assoc-default `title data)
	:status, (if (string= (assoc-default `state data) "opened") 'open 'closed)
	:date-creation ,(org-sync-parse-date (assoc-default 'created_at data))
	:deadline ,(org-sync-parse-date (assoc-default 'due_date data))
	:date-modification ,(org-sync-parse-date (assoc-default 'updated_at data))
	:web-url ,(assoc-default 'web_url data)
	:desc, (assoc-default `description data)))


(defun org-sync-gitlab-request (method url &optional data)
  "Sends HTTP request at URL using METHOD with DATA
Return a JSON response"
  (let* ((url-request-method method)
         (url-request-data data)
	 (url-request-extra-headers `(( "Private-Token".  ,(org-sync-gitlab-get-auth-token))))
	 )
    (message "%s %s %s" method url (prin1-to-string data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(defun org-sync-gitlab-get-auth-token ()
  "Gets the private-token."
  ;;TODO: prompt the user for auth token
  (unless  org-sync-gitlab-auth-token
    (error "Please set org-sync-gitlab-auth-token"))
  org-sync-gitlab-auth-token
  )

(provide 'org-sync-gitlab)
;;; org-sync-gitlab ends here



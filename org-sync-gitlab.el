;;; org-sync-gitlab.el --- GitLab backend for org-sync.
;;
;; Copyright (C) 2017 Toon Claes
;;
;; Author: Toon Claes <toon at iotcl dot com>
;; Keywords: org, gitlab, issues, merge-requests
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
;; This package implements a backend for org-sync to synchronize
;; issues from a GitLab issue tracker with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.
;;
;; This backend supports basic issue synchronization along with label creation.
;; If you add or change the labels of an issue to something that doesn't
;; exists, it will be created.
;;
;;; Code:

(require 'cl-lib)
(require 'url)
(require 'org-sync)
(require 'json)

(defvar org-sync-gitlab-backend
  '((base-url      . org-sync-base-project-api-url)
    (fetch-buglist . org-sync-gitlab-fetch-issues)
    (send-buglist  . org-sync-gitlab-send-issues))
  "GitLab backend.")

(defvar org-sync-gitlab-auth-token nil
  "GitLab Personal Access Token.
Visit `/profile/personal_access_tokens' on your GitLab instance
to generate a token.")

(defun org-sync-base-project-api-url (url)
  "The project API url, extracted from URL."
  (let* ((urlobj (url-generic-parse-url url))
         (encoded-project-name (browse-url-url-encode-chars
                                (substring (url-filename urlobj) 1) "/")))
    (setf (url-filename urlobj) (concat "/api/v4/projects/" encoded-project-name))
    (url-recreate-url urlobj)))

(defun org-sync-gitlab-fetch-labels ()
  "Return project labels from your GitLab instance."
  (let* ((url (concat org-sync-base-url "/labels"))
         (json (org-sync-gitlab-fetch-json url)))
    (mapcar (lambda (x)
              (cdr (assoc 'name x)))
            json)))

(defun org-sync-gitlab-random-color ()
  "Return a random hex color code 6 characters string without #."
  (random t)
  (format "%02X%02X%02X" (random 256) (random 256) (random 256)))

(defun org-sync-gitlab-color-p (color)
  "Return non-nil if COLOR is a valid color code."
  (and (stringp color) (string-match "^[0-9a-fA-F]\\{6\\}$" color)))

(defun org-sync-gitlab-create-label (label &optional color)
  "Create new LABEL with COLOR on your GitLab instance and return it.

LABEL must be a string.  COLOR must be a 6 characters string
containing a hex color code without the #.  Sets a random color
when not given or invalid."
  (let* ((url (concat org-sync-base-url "/labels"))
         (json (json-encode `((name . ,label)
                              (color . ,(if (org-sync-gitlab-color-p color)
                                            color
                                          (org-sync-gitlab-random-color)))))))
    (org-sync-gitlab-request "POST" url json)))

(defun org-sync-gitlab-handle-labels (issue existing-labels)
  "Create any label in ISSUE that is not in EXISTING-LABELS.

Append new labels in EXISTING-LABELS by side effects."
  (let* ((labels (org-sync-get-prop :labels issue)))
    (dolist (label labels)
      (when (org-sync-append! label existing-labels)
        (org-sync-gitlab-create-label label)))))

(defun org-sync-gitlab-time-to-string (time)
  "Return TIME as a full ISO 8601 date string, but without timezone adjustments (which GitLab doesn't support)."
  (format-time-string "%Y-%m-%dT%TZ" time t))

(defun org-sync-gitlab-fetch-issues (last-update)
  "Fetch and return issues from your GitLab instance."
  (let* ((created-after (when last-update
                          (format "&created_after=%s" (org-sync-gitlab-time-to-string last-update))))
         (url (concat org-sync-base-url "/issues?per_page=100" created-after))
         (json (vconcat (org-sync-gitlab-fetch-json url)
                        (org-sync-gitlab-fetch-json (concat url "&state=closed"))))
         (title (concat "Issues of " (org-sync-gitlab-project-name org-sync-base-url))))
    `(:title ,title
      :url ,org-sync-base-url ;; TODO the original URL
      :issues ,(mapcar 'org-sync-gitlab-json-to-issue json)
      :since ,last-update)))

(defun org-sync-gitlab-send-issues (issues)
  "Send ISSUES to your GitLab instance and return new created issues."
  (let* ((create-url (concat org-sync-base-url "/issues"))
         (existing-labels (org-sync-gitlab-fetch-labels)) ;; TODO fetch them EVERY TIME?
         (new-issues))
    (dolist (issue (org-sync-get-prop :issues issues))
      (let* ((sync (org-sync-get-prop :sync issue))
             (iid (org-sync-get-prop :iid issue))
             (data (org-sync-gitlab-issue-to-json issue))
             (update-url (format "%s/%d" create-url (or iid 0)))
             (result
              (cond
               ;; create new issue
               ((null iid)
                (org-sync-gitlab-handle-labels issue existing-labels)
                (push (org-sync-gitlab-json-to-issue
                       (org-sync-gitlab-request "POST" create-url data)) new-issues))

               ;; update issue
               (t
                (org-sync-gitlab-handle-labels b existing-labels)
                (org-sync-gitlab-request "PATCH" update-url data))))
             (err (cdr (assoc 'message result))))

        (when (stringp err)
          (error "GitLab: %s" err))))
    `(:issues ,new-issues)))

(defun org-sync-gitlab-fetch-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let (json)
    (while url
      (let* ((ret (org-sync-gitlab-fetch-json-page url))
             (data (car ret)))
        (setq url (cdr ret)
              json (vconcat json data))))))

(defvar url-http-extra-headers)

(defun org-sync-gitlab-url-retrieve-synchronously (url)
  "Retrieve from the specified URL using authentication data from `org-sync-gitlab-auth-token'."
  (let ((url-request-extra-headers '(("Authorization" . "None"))))
    (if org-sync-gitlab-auth-token
        (push (cons "PRIVATE-TOKEN" org-sync-gitlab-auth-token)
              url-request-extra-headers))
    (url-retrieve-synchronously url)))

(defun org-sync-gitlab-fetch-json-page (url)
  "Return a cons (JSON object from URL . next page url)."
  (let ((download-buffer (org-sync-gitlab-url-retrieve-synchronously url))
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
             "<\\(https?://.+?page=[0-9]+.*?\\)>; rel=\"next\""
             header-end t)
        (setq page-next (match-string 1)))

      (goto-char header-end)
      (setq ret (cons (json-read) page-next))
      (kill-buffer)
      ret)))

(defun org-sync-gitlab-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
Return the server decoded JSON response."
  (message "%s %s %s" method url (prin1-to-string data))
  (let* ((url-request-method method)
         (url-request-data data)
         (buf (org-sync-gitlab-url-retrieve-synchronously url)))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(defun org-sync-gitlab-project-name (url)
  "Return the name of the project at URL."
  (eww-decode-url-file-name (nth 4 (split-string (url-filename (url-generic-parse-url url)) "/"))))

(defun org-sync-gitlab-parse-desc (description)
  "Parse the DESCRIPTION to be human readable."
  (if description (progn
             (setq description (replace-regexp-in-string "\r\n" "\n" description))
             (setq description (replace-regexp-in-string "\\([^ \t\n]\\)[ \t\n]*\\'"
                                                         "\\1\n" description)))))

(defun org-sync-gitlab-json-to-issue (data)
  "Convert json DATA to an issue."
  (cl-flet* ((va (key alist) (cdr (assoc key alist)))
             (v (key) (va key data)))
    (let ((milestone-alist (v 'milestone)))
      `(:id ,(v 'id)
        :iid ,(v 'iid)
        :author ,(va 'username (v 'user))
        :assignee ,(va 'username (v 'assignee))  ;; TODO support for multiple assignees
        :status ,(if (string= (v 'state) "opened") 'opened 'closed)
        :title ,(v 'title)
        :desc ,(org-sync-gitlab-parse-desc (v 'body))
        :milestone ,(va 'title milestone-alist)
        :labels ,(mapcar (lambda (e) (va 'name e)) (v 'labels))
        :created-at ,(ctime (org-sync-parse-date (v 'created_at)))
        :updated-at ,(ctime (org-sync-parse-date (v 'updated_at)))))))

(defun org-sync-gitlab-issue-to-json (issue)
  "Return ISSUE as JSON."
  (let ((state (org-sync-get-prop :status issue)))
    (unless (member state '(opened closed))
      (error "GitLab: status invalid \"%s\"" (symbol-name state)))

  (json-encode
   `((title . ,(org-sync-get-prop :title issue))
     (description . ,(org-sync-get-prop :desc issue))
     ;(assignee . ,(org-sync-get-prop :assignee issue)) ;; TODO needs ids for this
     (state . ,(symbol-name (org-sync-get-prop :status issue)))
     (labels . [ ,@(org-sync-get-prop :labels issue) ])))))

(provide 'org-sync-gitlab)
;;; org-sync-gitlab.el ends here

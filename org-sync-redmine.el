;;; org-sync-rmine.el --- Redmine backend for org-sync.
;;
;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, redmine, synchronization
;; Homepage: https://github.com/arbox/org-sync
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

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
;; issues from a redmine repo with an org-mode buffer.  Read Org-sync
;; documentation for more information about it.
;;
;;; Code:

(require 'cl-lib)
(require 'org-sync)
(require 'url)
(require 'json)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar org-sync-rmine-backend
  '((base-url      . org-sync-rmine-base-url)
    (fetch-buglist . org-sync-rmine-fetch-buglist)
    (send-buglist  . org-sync-rmine-send-buglist))
  "Redmine backend.")

(defvar org-sync-rmine-auth nil
  "Redmine login (\"user\" . \"pwd\")")

(defvar org-sync-rmine-project-id nil
  "Project id of current buglist.")

(defconst org-sync-rmine-date-regex
  (rx
   (seq
    (group (repeat 4 digit)) "/"
    (group (repeat 2 digit)) "/"
    (group (repeat 2 digit))
    " "
    (group
     (repeat 2 digit) ":"
     (repeat 2 digit) ":"
     (repeat 2 digit))
    " "
    (group (or "+" "-")
           (repeat 2 digit)
           (repeat 2 digit))))
  "Regex to parse date returned by redmine.")

(defun org-sync-rmine-fetch-meta ()
  "Set `org-sync-rmine-project-id' for now."
  (let* ((res (org-sync-rmine-request "GET" (concat org-sync-base-url ".json")))
         (code (car res))
         (json (cdr res)))
    (when (/= code 200)
      (error "Can't fetch data from %s, wrong url?" org-sync-base-url))
    (setq org-sync-rmine-project-id (cdr (assoc 'id (cdr (assoc 'project json)))))))

(defun org-sync-rmine-parse-date (date)
  "Return time object of DATE."
  (when (string-match org-sync-rmine-date-regex date)
    (org-sync-parse-date (concat (match-string 1 date) "-"
                           (match-string 2 date) "-"
                           (match-string 3 date) "T"
                           (match-string 4 date)
                           (match-string 5 date)))))

(defun org-sync-rmine-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
AUTH is a cons (\"user\" . \"pwd\").  Return the server
decoded response in JSON."
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers
          (when data
            '(("Content-Type" . "application/json"))))
         (auth org-sync-rmine-auth)
         (buf))

    (when (stringp auth)
      (setq url (org-sync-url-param url `(("key" . ,auth)))))

    (message "%s %s %s" method url (prin1-to-string data))
    (setq buf (url-retrieve-synchronously url))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1
          (cons url-http-response-status (ignore-errors (json-read)))
        (kill-buffer)))))

;; override
(defun org-sync-rmine-base-url (url)
  "Return base URL."
  ;; if no url type, try http
  (when (not (string-match "^https?://" url))
    (setq url (concat "http://" url)))

  (let ((purl (url-generic-parse-url url)))
    (when (string-match "^.*/projects/\\([^/]+\\)" (url-filename purl))
      (concat (url-type purl) "://"
              (url-host purl)
              (match-string 0 (url-filename purl))))))

(defun org-sync-rmine-repo-name (url)
  "Return repo name at URL."
  (when (string-match "projects/\\([^/]+\\)" url)
    (match-string 1 url)))

(defun org-sync-rmine-json-to-bug (json)
  "Return JSON as a bug."
  (cl-flet* ((va (key alist) (cdr (assoc key alist)))
             (v (key) (va key json)))
    (let* ((id (v 'id))
           (author (va 'name (v 'author)))
           (txtstatus (va 'name (v 'status)))
           (status (if (or (string= txtstatus "Open")
                           (string= txtstatus "New"))
                       'open
                     'closed))
           (priority (va 'name (v 'priority)))
           (title (v 'subject))
           (desc (v 'description))
           (ctime (org-sync-rmine-parse-date (v 'created_on)))
           (mtime (org-sync-rmine-parse-date (v 'updated_on))))

      `(:id ,id
            :priority ,priority
            :status ,status
            :title ,title
            :desc ,desc
            :date-creation ,ctime
            :date-modification ,mtime))))

(defun org-sync-rmine-fetch-buglist (last-update)
  "Return the buglist at org-sync-base-url."
  (let* ((url (concat org-sync-base-url "/issues.json"))
         (res (org-sync-rmine-request "GET" url))
         (code (car res))
         (json (cdr res))
         (title (concat "Issues of " (org-sync-rmine-repo-name url))))

    `(:title ,title
             :url ,org-sync-base-url
             :bugs ,(mapcar 'org-sync-rmine-json-to-bug (cdr (assoc 'issues json))))))

(defun org-sync-rmine-bug-to-json (bug)
  (json-encode
   `((issue .
            ((subject     . ,(org-sync-get-prop :title bug))
             (description . ,(org-sync-get-prop :desc bug)))))))


;; (defun org-sync-rmine-code-success-p (code)
;;   "Return non-nil if HTTP CODE is a success code."
;;   (and (<= 200 code) (< code 300)))

(defun org-sync-rmine-send-buglist (buglist)
    "Send a BUGLIST on the bugtracker and return new bugs."
    (let* ((new-url (concat org-sync-base-url "/issues.json"))
           (root-url (replace-regexp-in-string "/projects/.+"
                                               "" org-sync-base-url))
           new-bugs)

      (org-sync-rmine-fetch-meta)

      (dolist (b (org-sync-get-prop :bugs buglist))
        (let* ((id (org-sync-get-prop :id b))
               (data (org-sync-rmine-bug-to-json b))
               (modif-url (format "%s/issues/%d.json" root-url (or id 0)))
               res)
          (cond
           ;; new bug
           ((null id)
            (setq res (org-sync-rmine-request "POST" new-url data))
            (when (/= (car res) 201)
              (error "Can't create new bug \"%s\"" (org-sync-get-prop :title b)))
            (push (org-sync-rmine-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs))

           ;; delete bug
           ((org-sync-get-prop :delete b)
            (setq res (org-sync-rmine-request "DELETE" modif-url))
            (when (not (member (car res) '(404 204)))
              (error "Can't delete bug #%d" id)))

           ;; update bug
           (t
            (setq res (org-sync-rmine-request "PUT" modif-url data))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            ;; fetch the new version since redmine doesn't send it
            (setq res (org-sync-rmine-request "GET" modif-url))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            (push (org-sync-rmine-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs)))))
      `(:bugs ,new-bugs)))

(provide 'org-sync-redmine)
;;; org-sync-redmine.el ends here

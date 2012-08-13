;;; os-rmine.el --- Redmine backend for org-sync.

;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, redmine, synchronization
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
;; issues from a redmine repo with an org-mode buffer.  Read Org-sync
;; documentation for more information about it.

;;; Code:

(eval-when-compile (require 'cl))
(require 'os)
(require 'url)
(require 'json)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar os-rmine-backend
  '((base-url      . os-rmine-base-url)
    (fetch-buglist . os-rmine-fetch-buglist)
    (send-buglist  . os-rmine-send-buglist))
  "Redmine backend.")

(defvar os-rmine-auth nil
  "Redmine login (\"user\" . \"pwd\")")

(defvar os-rmine-project-id nil
  "Project id of current buglist.")

(defconst os-rmine-date-regex
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

(defun os-rmine-fetch-meta ()
  "Set `os-rmine-project-id' for now."
  (let* ((res (os-rmine-request "GET" (concat os-base-url ".json")))
         (code (car res))
         (json (cdr res)))
    (when (/= code 200)
      (error "Can't fetch data from %s, wrong url?" os-base-url))
    (setq os-rmine-project-id (cdr (assoc 'id (cdr (assoc 'project json)))))))

(defun os-rmine-parse-date (date)
  "Return time object of DATE."
  (when (string-match os-rmine-date-regex date)
    (os-parse-date (concat (match-string 1 date) "-"
                           (match-string 2 date) "-"
                           (match-string 3 date) "T"
                           (match-string 4 date)
                           (match-string 5 date)))))

(defun os-rmine-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
AUTH is a cons (\"user\" . \"pwd\").  Return the server
decoded response in JSON."
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers
          (when data
            '(("Content-Type" . "application/json"))))
         (auth os-rmine-auth)
         (buf))

    (when (stringp auth)
      (setq url (os-url-param url `(("key" . ,auth)))))

    (message "%s %s %s" method url (prin1-to-string data))
    (setq buf (url-retrieve-synchronously url))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1
          (cons url-http-response-status (ignore-errors (json-read)))
        (kill-buffer)))))

;; override
(defun os-rmine-base-url (url)
  "Return base URL."
  ;; if no url type, try http
  (when (not (string-match "^https?://" url))
    (setq url (concat "http://" url)))

  (let ((purl (url-generic-parse-url url)))
    (when (string-match "^.*/projects/\\([^/]+\\)" (url-filename purl))
      (concat (url-type purl) "://"
              (url-host purl)
              (match-string 0 (url-filename purl))))))

(defun os-rmine-repo-name (url)
  "Return repo name at URL."
  (when (string-match "projects/\\([^/]+\\)" url)
    (match-string 1 url)))

(defun os-rmine-json-to-bug (json)
  "Return JSON as a bug."
  (flet ((va (key alist) (cdr (assoc key alist)))
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
           (ctime (os-rmine-parse-date (v 'created_on)))
           (mtime (os-rmine-parse-date (v 'updated_on))))

      `(:id ,id
            :priority ,priority
            :status ,status
            :title ,title
            :desc ,desc
            :date-creation ,ctime
            :date-modification ,mtime))))

(defun os-rmine-fetch-buglist (last-update)
  "Return the buglist at os-base-url."
  (let* ((url (concat os-base-url "/issues.json"))
         (res (os-rmine-request "GET" url))
         (code (car res))
         (json (cdr res))
         (title (concat "Bugs of " (os-rmine-repo-name url))))

    `(:title ,title
             :url ,os-base-url
             :bugs ,(mapcar 'os-rmine-json-to-bug (cdr (assoc 'issues json))))))

(defun os-rmine-bug-to-json (bug)
  (json-encode
   `((issue .
            ((subject     . ,(os-get-prop :title bug))
             (description . ,(os-get-prop :desc bug)))))))


;; (defun os-rmine-code-success-p (code)
;;   "Return non-nil if HTTP CODE is a success code."
;;   (and (<= 200 code) (< code 300)))

(defun os-rmine-send-buglist (buglist)
    "Send a BUGLIST on the bugtracker and return new bugs."
    (let* ((new-url (concat os-base-url "/issues.json"))
           (root-url (replace-regexp-in-string "/projects/.+"
                                               "" os-base-url))
           new-bugs)

      (os-rmine-fetch-meta)

      (dolist (b (os-get-prop :bugs buglist))
        (let* ((id (os-get-prop :id b))
               (data (os-rmine-bug-to-json b))
               (modif-url (format "%s/issues/%d.json" root-url (or id 0)))
               res)
          (cond
           ;; new bug
           ((null id)
            (setq res (os-rmine-request "POST" new-url data))
            (when (/= (car res) 201)
              (error "Can't create new bug \"%s\"" (os-get-prop :title b)))
            (push (os-rmine-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs))

           ;; delete bug
           ((os-get-prop :delete b)
            (setq res (os-rmine-request "DELETE" modif-url))
            (when (not (member (car res) '(404 204)))
              (error "Can't delete bug #%d" id)))

           ;; update bug
           (t
            (setq res (os-rmine-request "PUT" modif-url data))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            ;; fetch the new version since redmine doesn't send it
            (setq res (os-rmine-request "GET" modif-url))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            (push (os-rmine-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs)))))
      `(:bugs ,new-bugs)))
;;; os-rmine.el ends here

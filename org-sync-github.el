;;; org-sync-github.el --- Github backend for org-sync.
;;
;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, github, synchronization
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
;; issues from a github tracker with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.
;;
;; This backend supports basic bug synching along with tag creation.
;; If you add or change the tags of an issue to something that doesn't
;; exists, it will be created.
;;
;;; Code:

(require 'cl-lib)
(require 'url)
(require 'org-sync)
(require 'json)

(defvar org-sync-github-backend
  '((base-url      . org-sync-github-base-url)
    (fetch-buglist . org-sync-github-fetch-buglist)
    (send-buglist  . org-sync-github-send-buglist))
  "Github backend.")

(defvar url-http-end-of-headers)

(defvar org-sync-github-auth nil
  "Github login (\"user\" . \"pwd\")")

(defun org-sync-github-fetch-labels ()
  "Return list of labels at org-sync-base-url."
  (let* ((url (concat org-sync-base-url "/labels"))
         (json (org-sync-github-fetch-json url)))
    (mapcar (lambda (x)
              (cdr (assoc 'name x)))
            json)))

(defun org-sync-github-random-color ()
  "Return a random hex color code 6 characters string without #."
  (random t)
  (format "%02X%02X%02X" (random 256) (random 256) (random 256)))

(defun org-sync-github-color-p (color)
  "Return non-nil if COLOR is a valid color code."
  (and (stringp color) (string-match "^[0-9a-fA-F]\\{6\\}$" color)))

(defun org-sync-github-create-label (label &optional color)
  "Create new COLOR LABEL at org-sync-base-url and return it.

LABEL must be a string.  COLOR must be a 6 characters string
containing a hex color code without the #.  Take a random color
when not given."
  (let* ((url (concat org-sync-base-url "/labels"))
         (json (json-encode `((name . ,label)
                              (color . ,(if (org-sync-github-color-p color)
                                            color
                                          (org-sync-github-random-color)))))))
    (org-sync-github-request "POST" url json)))

(defun org-sync-github-handle-tags (bug existing-tags)
  "Create any label in BUG that is not in EXISTING-TAGS.

Append new tags in EXISTING-TAGS by side effects."
  (let* ((tags (org-sync-get-prop :tags bug)))
    (dolist (tag tags)
      (when (org-sync-append! tag existing-tags)
        (org-sync-github-create-label tag)))))

(defun org-sync-github-time-to-string (time)
  "Return TIME as a full ISO 8601 date string, but without timezone adjustments (which github doesn't support"
  (format-time-string "%Y-%m-%dT%TZ" time t))

;; override
(defun org-sync-github-fetch-buglist (last-update)
  "Return the buglist at org-sync-base-url."
  (let* ((since (when last-update
                  (format "&since=%s" (org-sync-github-time-to-string last-update))))
         (url (concat org-sync-base-url "/issues?per_page=100" since))
         (json (vconcat (org-sync-github-fetch-json url)
                        (org-sync-github-fetch-json (concat url "&state=closed"))))
         (title (concat "Issues of " (org-sync-github-repo-name url))))

    `(:title ,title
             :url ,org-sync-base-url
             :bugs ,(mapcar 'org-sync-github-json-to-bug json)
             :since ,last-update)))

;; override
(defun org-sync-github-base-url (url)
  "Return base url."
  (when (string-match "github.com/\\(?:repos/\\)?\\([^/]+\\)/\\([^/]+\\)" url)
    (let ((user (match-string 1 url))
          (repo (match-string 2 url)))
    (concat "https://api.github.com/repos/" user "/" repo ""))))

;; override
(defun org-sync-github-send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return new bugs."
  (let* ((new-url (concat org-sync-base-url "/issues"))
         (existing-tags (org-sync-github-fetch-labels))
         (newbugs))
    (dolist (b (org-sync-get-prop :bugs buglist))
      (let* ((sync (org-sync-get-prop :sync b))
             (id (org-sync-get-prop :id b))
             (data (org-sync-github-bug-to-json b))
             (modif-url (format "%s/%d" new-url (or id 0)))
             (result
              (cond
               ;; new bug
               ((null id)
                (org-sync-github-handle-tags b existing-tags)
                (push (org-sync-github-json-to-bug
                       (org-sync-github-request "POST" new-url data)) newbugs))

               ;; update bug
               (t
                (org-sync-github-handle-tags b existing-tags)
                (org-sync-github-request "PATCH" modif-url data))))
             (err (cdr (assoc 'message result))))

        (when (stringp err)
          (error "Github: %s" err))))
    `(:bugs ,newbugs)))

(defun org-sync-github-fetch-json (url)
  "Return a parsed JSON object of all the pages of URL."
  (let* ((ret (org-sync-github-fetch-json-page url))
         (data (car ret))
         (url (cdr ret))
         (json data))

    (while url
      (setq ret (org-sync-github-fetch-json-page url))
      (setq data (car ret))
      (setq url (cdr ret))
      (setq json (vconcat json data)))

    json))

(defun org-sync-github-url-retrieve-synchronously (url)
  "Retrieve the specified url using authentication data from
org-sync-github-auth. AUTH is a cons (\"user\" . \"pwd\")."
  (let ((auth org-sync-github-auth))
    (if (consp auth)
        ;; dynamically bind auth related vars
        (let* ((str (concat (car auth) ":" (cdr auth)))
               (encoded (base64-encode-string str))
               (login `(("api.github.com:443" ("Github API" . ,encoded))))
               (url-basic-auth-storage 'login))
          (url-retrieve-synchronously url))
      ;; nothing more to bind
      (url-retrieve-synchronously url))))

(defun org-sync-github-fetch-json-page (url)
  "Return a cons (JSON object from URL . next page url)."
  (let ((download-buffer (org-sync-github-url-retrieve-synchronously url))
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

(defun org-sync-github-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
Return the server decoded JSON response."
  (message "%s %s %s" method url (prin1-to-string data))
  (let* ((url-request-method method)
         (url-request-data data)
         (buf (org-sync-github-url-retrieve-synchronously url)))

    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1 (json-read) (kill-buffer)))))

(defun org-sync-github-repo-name (url)
  "Return the name of the repo at URL."
  (if (string-match "github.com/repos/[^/]+/\\([^/]+\\)" url)
      (match-string 1 url)
    "<project name>"))

;; XXX: we need an actual markdown parser here...
(defun org-sync-github-filter-desc (desc)
  "Return a filtered description of a GitHub description."
  (if desc (progn
             (setq desc (replace-regexp-in-string "\r\n" "\n" desc))
             (setq desc (replace-regexp-in-string "\\([^ \t\n]\\)[ \t\n]*\\'"
                                                  "\\1\n" desc)))))

(defun org-sync-github-json-to-bug (data)
  "Return DATA (in json) converted to a bug."
  (cl-flet* ((va (key alist) (cdr (assoc key alist)))
             (v (key) (va key data)))
    (let* ((id (v 'number))
           (stat (if (string= (v 'state) "open") 'open 'closed))
           (title (v 'title))
           (desc  (org-sync-github-filter-desc (v 'body)))
           (author (va 'login (v 'user)))
           (assignee (va 'login (v 'assignee)))
           (milestone-alist (v 'milestone))
           (milestone (va 'title milestone-alist))
           (ctime (org-sync-parse-date (v 'created_at)))
           (dtime (org-sync-parse-date (va 'due_on milestone-alist)))
           (mtime (org-sync-parse-date (v 'updated_at)))
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

(defun org-sync-github-bug-to-json (bug)
  "Return BUG as JSON."
  (let ((state (org-sync-get-prop :status bug)))
    (unless (member state '(open closed))
      (error "Github: unsupported state \"%s\"" (symbol-name state)))

  (json-encode
   `((title . ,(org-sync-get-prop :title bug))
     (body . ,(org-sync-get-prop :desc bug))
     (assignee . ,(org-sync-get-prop :assignee bug))
     (state . ,(symbol-name (org-sync-get-prop :status bug)))
     (labels . [ ,@(org-sync-get-prop :tags bug) ])))))

(provide 'org-sync-github)
;;; org-sync-github.el ends here

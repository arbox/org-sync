;;; org-sync-rtm.el --- Remember The Milk backend for org-sync.
;;
;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, rtm, synchronization
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
;; issues from a remember the milk repo with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.
;;
;;; Code:

(require 'cl-lib)
(require 'org-sync)
(require 'json)
(require 'url)

(defvar org-sync-rtm-api-key "e9b28a9ac67f1bffc3dab1bd94dab722")
(defvar org-sync-rtm-shared-secret "caef7e509a8dcd82")
(defvar org-sync-rtm-token nil)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun org-sync-rtm-call (method &rest args)
  "Call API METHOD and return result."
  (let* ((param `(("method" . ,method)
                  ,@args)))
    (org-sync-rtm-request "GET" "http://api.rememberthemilk.com/services/rest/" param nil 'sign)))

(defvar org-sync-rtm-backend
  '((base-url      . org-sync-rtm-base-url)
    (fetch-buglist . org-sync-rtm-fetch-buglist)
    (send-buglist  . org-sync-rtm-send-buglist))
  "Bitbucket backend.")

(defun org-sync-rtm-base-url (url)
  "Return base URL. Not used with RTM."
  url)

(defun org-sync-rtm-filter-tasks (response)
  "Return all the real task from RTM rtm.tasks.getList RESPONSE."
  (let (final)
    (mapc (lambda (e)
            (when (assoc 'taskseries e)
              (mapc (lambda (task-series)
                      (push task-series final))
                    (org-sync-getalist e 'taskseries))))
          (org-sync-getalist (cdr response) 'rsp 'tasks 'list))
    final))

(defun org-sync-rtm-fetch-buglist (last-update)
  (unless org-sync-rtm-token
    (org-sync-rtm-auth))
  (let ((bl
         (mapcar 'org-sync-rtm-task-to-bug
                 (org-sync-rtm-filter-tasks (org-sync-rtm-call "rtm.tasks.getList")))))
    `(:title "Tasks"
             :url ,org-sync-base-url
             :bugs ,bl)))

(defun org-sync-rtm-task-to-bug (task)
  "Return TASK as a bug."
  (cl-flet ((v (&rest key) (apply 'org-sync-getalist task key)))
    (let* ((id (string-to-number (v 'id)))
           (title (v 'name))
           (status (if (string= (v 'task 'completed) "")
                       'open
                     'closed))
           (priority (v 'task 'priority))
           (ctime (org-sync-parse-date (v 'created)))
           (mtime (org-sync-parse-date (v 'modified)))
           (dtime (org-sync-parse-date (v 'task 'due))))
      `(:id ,id
            :title ,title
            :status ,status
            :priority ,priority
            :date-creation ,ctime
            :date-modification ,mtime
            :date-deadline ,dtime))))


(defun org-sync-rtm-request (method url &optional param data sign)
  "Send HTTP request at URL using METHOD with DATA."

  (unless  (string-match "/auth/" url)
    (push (cons "format" "json") param))

  (when org-sync-rtm-token
    (push (cons "auth_token" org-sync-rtm-token) param))

  (push `("api_key" . ,org-sync-rtm-api-key) param)

  (when sign
    (push `("api_sig" . ,(org-sync-rtm-sign param)) param))

  (setq url (org-sync-url-param url param))

  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers
          (when data
            '(("Content-Type" . "application/x-www-form-urlencoded"))))
         buf)

    (message "%s %s %s" method url (prin1-to-string data))
    (setq buf (url-retrieve-synchronously url))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (message "%s" (buffer-substring (point) (point-max)))
      (prog1
          (cons url-http-response-status (ignore-errors (json-read)))
        (kill-buffer)))))

(defun org-sync-rtm-auth ()
  "Return the URL to grant access to the user account."
  ;; http://www.rememberthemilk.com/services/auth/?api_key=abc123&perms=delete

  (let* ((res (org-sync-rtm-call "rtm.auth.getFrob"))
         (frob (cdr (assoc 'frob (cl-cdadr res))))
         (param `(("api_key" . ,org-sync-rtm-api-key)
                  ("perms"   . "delete")
                  ("frob"    . ,frob)))
         url)

    ;; add signature
    (push `("api_sig" . ,(org-sync-rtm-sign param)) param)
    (setq url (org-sync-url-param "http://www.rememberthemilk.com/services/auth/" param))
    (browse-url url)
    (when (yes-or-no-p "Application accepted? ")
      (setq
       org-sync-rtm-token
       (org-sync-getalist
        (cdr (org-sync-rtm-call "rtm.auth.getToken" `("frob" . ,frob)))
        'rsp 'auth 'token)))))

(defun org-sync-rtm-sign (param-alist)
  "Return the signature for the PARAM-ALIST request."
  (let ((param (copy-tree param-alist))
        sign)

    ;; sort by key
    (setq param (sort param (lambda (a b)
                            (string< (car a) (car b)))))

    ;; sign = md5(shared_secret . k1 . v1 . k2 . v2...)
    (md5
     (message
      (concat
       org-sync-rtm-shared-secret
       ;; concat key&value
       (mapconcat (lambda (x)
                    (concat (car x) (cdr x)))
                  param ""))

      nil nil 'utf-8))))

(provide 'org-sync-rtm)
;;; org-sync-rtm.el ends here

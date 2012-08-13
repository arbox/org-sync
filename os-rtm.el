;;; os-rtm.el --- Remember The Milk backend for org-sync.

;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, rtm, synchronization
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
;; issues from a remember the milk repo with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.

;;; Code:

(eval-when-compile (require 'cl))
(require 'os)
(require 'json)
(require 'url)

(defvar os-rtm-api-key "e9b28a9ac67f1bffc3dab1bd94dab722")
(defvar os-rtm-shared-secret "caef7e509a8dcd82")
(defvar os-rtm-token nil)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun os-rtm-call (method &rest args)
  "Call API METHOD and return result."
  (let* ((param `(("method" . ,method)
                  ,@args)))
    (os-rtm-request "GET" "http://api.rememberthemilk.com/services/rest/" param nil 'sign)))

(defvar os-rtm-backend
  '((base-url      . os-rtm-base-url)
    (fetch-buglist . os-rtm-fetch-buglist)
    (send-buglist  . os-rtm-send-buglist))
  "Bitbucket backend.")

(defun os-rtm-base-url (url)
  "Return base URL. Not used with RTM."
  url)

(defun os-rtm-filter-tasks (response)
  "Return all the real task from RTM rtm.tasks.getList RESPONSE."
  (let (final)
    (mapc (lambda (e)
            (when (assoc 'taskseries e)
              (mapc (lambda (task-series)
                      (push task-series final))
                    (os-getalist e 'taskseries))))
          (os-getalist (cdr response) 'rsp 'tasks 'list))
    final))

(defun os-rtm-fetch-buglist (last-update)
  (unless os-rtm-token
    (os-rtm-auth))
  (let ((bl
         (mapcar 'os-rtm-task-to-bug
                 (os-rtm-filter-tasks (os-rtm-call "rtm.tasks.getList")))))
    `(:title "Tasks"
             :url ,os-base-url
             :bugs ,bl)))

(defun os-rtm-task-to-bug (task)
  "Return TASK as a bug."
  (flet ((v (&rest key) (apply 'os-getalist task key)))
    (let* ((id (string-to-number (v 'id)))
           (title (v 'name))
           (status (if (string= (v 'task 'completed) "")
                       'open
                     'closed))
           (priority (v 'task 'priority))
           (ctime (os-parse-date (v 'created)))
           (mtime (os-parse-date (v 'modified)))
           (dtime (os-parse-date (v 'task 'due))))
      `(:id ,id
            :title ,title
            :status ,status
            :priority ,priority
            :date-creation ,ctime
            :date-modification ,mtime
            :date-deadline ,dtime))))


(defun os-rtm-request (method url &optional param data sign)
  "Send HTTP request at URL using METHOD with DATA."

  (unless  (string-match "/auth/" url)
    (push (cons "format" "json") param))

  (when os-rtm-token
    (push (cons "auth_token" os-rtm-token) param))

  (push `("api_key" . ,os-rtm-api-key) param)

  (when sign
    (push `("api_sig" . ,(os-rtm-sign param)) param))

  (setq url (os-url-param url param))

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

(defun os-rtm-auth ()
  "Return the URL to grant access to the user account."
  ;; http://www.rememberthemilk.com/services/auth/?api_key=abc123&perms=delete

  (let* ((res (os-rtm-call "rtm.auth.getFrob"))
         (frob (cdr (assoc 'frob (cdadr res))))
         (param `(("api_key" . ,os-rtm-api-key)
                  ("perms"   . "delete")
                  ("frob"    . ,frob)))
         url)

    ;; add signature
    (push `("api_sig" . ,(os-rtm-sign param)) param)
    (setq url (os-url-param "http://www.rememberthemilk.com/services/auth/" param))
    (browse-url url)
    (when (yes-or-no-p "Application accepted? ")
      (setq
       os-rtm-token
       (os-getalist
        (cdr (os-rtm-call "rtm.auth.getToken" `("frob" . ,frob)))
        'rsp 'auth 'token)))))

(defun os-rtm-sign (param-alist)
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
       os-rtm-shared-secret
       ;; concat key&value
       (mapconcat (lambda (x)
                    (concat (car x) (cdr x)))
                  param ""))

      nil nil 'utf-8))))
;;; os-rtm.el ends here

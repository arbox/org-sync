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
(require 'org-sync)
(require 'json)
(require 'url)

(defvar os-rtm-api-key "e9b28a9ac67f1bffc3dab1bd94dab722")
(defvar os-rtm-shared-secret "caef7e509a8dcd82")
(defvar os-rtm-frob)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun os-rtm-call (method &optional args)
  "Call API METHOD and return result."
  (let* ((param `(("method" . ,method)
                  ,@args)))
    (os-rtm-request "GET" "http://api.rememberthemilk.com/services/rest/" param nil 'sign)))

(defun os-rtm-request (method url &optional param data sign)
  "Send HTTP request at URL using METHOD with DATA."

  (unless  (string-match "/auth/" url)
    (push '("format" . "json") param))

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
      (os-rtm-call "rtm.auth.getToken" `(("frob" . ,frob))))))

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

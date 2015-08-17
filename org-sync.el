;;; org-sync.el --- Synchronize Org documents with External Issue Trackers

;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Maintainer: Andrei Beliankou <arbox@yandex.ru>
;; Created: 2012-05-01
;; Keywords: org, synchronization, issue tracking, GitHub, Redmine
;; Homepage: https://github.com/arbox/org-sync
;; Package-Requires: ((cl-lib "0.5") (org "8.2") (emacs "24"))
;; Version: 0.3.0

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

;; This package implements an extension to org-mode that synchronizes
;; org document with external services.  It provides an interface that
;; can be implemented in backends.  The current focus is on
;; bugtrackers services.

;; The entry points are `org-sync-import', `org-sync' and `org-sync-or-import'.
;; The first one prompts for an URL to import, the second one pulls, merges and
;; pushes every buglist in the current buffer and the third one
;; combines the others in one function: if nothing in the buffer can
;; be synchronized, ask for an URL to import.

;; The usual workflow is to first import your buglist with
;; `org-sync-import', modify it or add a bug and run `org-sync'.

;; A buglist is a top-level headline which has a :url: in its
;; PROPERTIES block.  This headline is composed of a list of
;; subheadlines which correspond to bugs.  The requirement for a bug
;; is to have a state, a title and an id.  If you add a new bug, it
;; wont have an id but it will get one once you sync.  If you omit the
;; status, OPEN is chose.

;; The status is an org TODO state.  It can either be OPEN or CLOSED.
;; The title is just the title of the headline.  The id is a number in
;; the PROPERTIES block of the headline.

;; Org DEADLINE timestamp are also handled and can be inserted in a
;; bug headline which can then be used by the backend if it supports
;; it.

;; Paragraphs under bug-headlines are considered as their description.
;; Additional data used by the backend are in the PROPERTIES block of
;; the bug.

;; To add a bug, just insert a new headline under the buglist you want
;; to modify e.g.:
;;     ** OPEN my new bug
;; Then simply call `org-sync'.

;;; Code:

;; The data structures used to represent bugs and buglists are simple
;; plists.  It is what backend have to handle, process or return.

;; Buglist example:

;; '(:title "My buglist"
;;   :url "http://github.com/repos/octocat/Hello-World"
;;   :bugs (BUGS...))

;; Bug example:

;; '(:id 3
;;   :status 'open or 'closed
;;   :sync 'conflict-local or 'conflict-remote
;;   :title "foo"
;;   :desc "blah"
;;   :priority "major"
;;   :tags ("a" "b" "c")
;;   :author "Aurélien"
;;   :assignee "Foo"
;;   :milestone "foo"

;;   ;; dates are regular emacs time object
;;   :date-deadline ...
;;   :date-creation ...
;;   :date-modification ...

;;   ;; backend-specific properties
;;   ;; ...
;;   )

;; Some accessors are available for both structure.  See `org-sync-set-prop',
;; and `org-sync-get-prop'.


;; When importing an URL, Org-sync matches the URL against the
;; variable `org-sync-backend-alist' which maps regexps to backend symbols.
;; The backend symbol is then used to call the backend functions.
;; When these functions are called, the variable `org-sync-backend' and
;; `org-sync-base-url' are dynamically bound to respectively the backend
;; symbol and the canonical URL for the thing you are syncing with.

;; The symbol part in a `org-sync-backend-alist' pair must be a variable
;; defined in the backend.  It is an alist that maps verb to function
;; symbol.  Each backend must implement at least 3 verbs:

;; * base-url (param: URL)

;; Given the user URL, returns the canonical URL to represent it.
;; This URL will be available dynamically to all of your backend
;; function through the `org-sync-base-url' variable.

;; * fetch-buglist (param: LAST-FETCH-TIME)

;; Fetch the buglist at `org-sync-base-url'.  If LAST-FETCH-TIME is non-nil,
;; and you only fetched things modified since it, you are expected to
;; set the property :since to it in the buglist you return.  You can
;; add whatever properties you want in a bug.  The lisp printer is
;; used to persist them in the buffer.

;; * send-buglist (param: BUGLIST)

;; Send BUGLIST to the repo at `org-sync-base-url' and return the new bugs
;; created that way. A bug without an id in BUGLIST is a new bug, the
;; rest are modified bug.


;; When synchronizing, Org-sync parses the current buffer using
;; org-element and convert any found buglist headline to a buglist
;; data structure.  See `org-sync-headline-to-buglist',
;; `org-sync-headline-to-bug'.

;; When writing buglists back to the document, Org-sync converts them
;; to elements -- the data structure used by org-element -- which are
;; then interpreted by `org-element-interpret-data'.  The resulting
;; string is then inserted in the buffer.  See `org-sync-buglist-to-element'
;; and `org-sync-bug-to-element'.

;;; Change Log:
;; Version 0.0.1

;;; Code:
(require 'cl-lib)
(require 'org)
(require 'org-element)

(defvar org-sync-backend nil
  "Org-sync current backend.")

(defvar org-sync-base-url nil
  "Org-sync current base url.")

(defvar org-sync-backend-alist
  '(("github.com/\\(?:repos/\\)?[^/]+/[^/]+" . org-sync-github-backend)
    ("bitbucket.org/[^/]+/[^/]+"             . org-sync-bb-backend)
    ("/projects/[^/]+"                       . org-sync-rmine-backend)
    ("rememberthemilk.com"                   . org-sync-rtm-backend))
  "Alist of url patterns and corresponding org-sync backends.")

(defvar org-sync-cache-file (concat user-emacs-directory "org-sync-cache")
  "Path to Org-sync cache file.")

(defvar org-sync-cache-alist nil
  "Org-sync cache for buglists.
Maps URLs to buglist cache.")

(defvar org-sync-conflict-buffer "*Org-sync conflict*"
  "Name of the conflict buffer.")

(defvar org-sync-props nil
  "List of property to sync or nil to sync everything.")

(defun org-sync-action-fun (action)
  "Return current backend ACTION function or nil."
  (unless (or (null action) (null org-sync-backend))
    (let ((fsym (assoc-default action (eval org-sync-backend))))
      (when (fboundp fsym)
        fsym))))

(defun org-sync-get-backend (url)
  "Return backend symbol matching URL from `org-sync-backend-alist'."
  (assoc-default url org-sync-backend-alist 'string-match))

(defmacro org-sync-with-backend (backend &rest body)
  "Eval BODY with org-sync-backend set to corresponding BACKEND.

If BACKEND evals to a string it is passed to org-sync-get-backend, the
resulting symbol is dynamically assigned to org-sync-backend.  The url
is passed to org-sync--base-url and dynamically assigned to
org-sync-base-url.

Else BACKEND should be a backend symbol.  It is
assigned to org-sync-backend."
  (declare (indent 1) (debug t))
  (let ((res (cl-gensym))
        (url (cl-gensym)))

    `(let* ((,res ,backend)
            (,url))
       (when (stringp ,res)
         (setq ,url ,res)
         (setq ,res (org-sync-get-backend ,url)))
       (unless (symbolp ,res)
         (error "Backend %s does not evaluate to a symbol."
                (prin1-to-string ',backend)))
       (let* ((org-sync-backend ,res)
              (org-sync-base-url (org-sync--base-url ,url)))
         ,@body))))

(defun org-sync-set-cache (url buglist)
  "Update URL to BUGLIST in `org-sync-cache-alist'."
  (let ((cell (assoc url org-sync-cache-alist)))
    (if cell
        (setcdr cell buglist)
      (push (cons url buglist) org-sync-cache-alist))))

(defun org-sync-get-cache (url)
  "Return the buglist at URL in cache or nil."
    (cdr (assoc url org-sync-cache-alist)))

(defun org-sync-write-cache ()
  "Write Org-sync cache to `org-sync-cache-file'."
  (with-temp-file org-sync-cache-file
    (prin1 `(setq org-sync-cache-alist ',org-sync-cache-alist) (current-buffer))))

(defun org-sync-load-cache ()
  "Load Org-sync cache from `org-sync-cache-file'."
  (load org-sync-cache-file 'noerror nil))

(defun org-sync-plist-to-alist (plist)
  "Return PLIST as an association list."
  (let* (alist cell q (p plist))
    (while p
      (setq cell (cons (car p) (cadr p)))
      (if alist
          (progn
            (setcdr q (cons cell nil))
            (setq q (cdr q)))
        (setq alist (cons cell nil))
        (setq q alist))
      (setq p (cddr p)))
    alist))

(defun org-sync-propertize (sym)
  "Return sym as a property i.e. prefixed with :."
  (intern (concat ":" (if (symbolp sym)
                          (symbol-name sym)
                        sym))))

(defun org-sync-get-prop (key b)
  "Return value of the property KEY in buglist or bug B."
  (plist-get b key))

(defun org-sync-set-prop (key val b)
  "Set KEY to VAL in buglist or bug B."
  (plist-put b key val))

(defun org-sync-append! (elem list)
  "Add ELEM at the end of LIST by side effect if it isn't present.

Return ELEM if it was added, nil otherwise."
  (catch :exit
    (let ((p list))
      (while (cdr p)
        (when (equal (car p) elem)
          (throw :exit nil))
        (setq p (cdr p)))
      (setcdr p (cons elem nil))
      elem)))

(defun org-sync--send-buglist (buglist)
  "Send a BUGLIST on the bugtracker."
  (let ((f (org-sync-action-fun 'send-buglist)))
        (if f
            (funcall f buglist)
          (error "No send backend available."))))

(defun org-sync--fetch-buglist (last-update)
  "Return the buglist at url REPO."
  (let ((f (org-sync-action-fun 'fetch-buglist)))
        (if f
            (funcall f last-update)
          (error "No fetch backend available."))))


(defun org-sync--base-url (url)
  "Return the base url of URL."
  (let ((f (org-sync-action-fun 'base-url)))
        (if f
            (funcall f url)
          (error "No base-url backend available."))))


(defun org-sync-url-param (url param)
  "Return URL with PARAM alist appended."
  (let* ((split (split-string url "\\?" t))
         (base (car split))
         (rest (cadr split))
         (final))

    ;; read all param
    (when rest
      (mapc
       (lambda (s)
         (let* ((split (split-string s "=" t))
                (var (car split))
                (val (cadr split))
                (cell (assoc var final)))
           (if cell
               (setcdr cell val)
             (push (cons var val) final))))
       (split-string rest "&" t)))

    ;; add params from arg
    (mapc (lambda (p)
            (let* ((var (car p))
                   (val (cdr p))
                   (cell (assoc var final)))
              (if cell
                  (setcdr cell val)
                (push p final))))
          param)

    ;; output new url
    (concat
     base
     "?"
     (mapconcat (lambda (p)
                  (concat
                   (url-hexify-string (car p))
                   "="
                   (url-hexify-string (cdr p))))
                final "&"))))

;; OPEN bugs sorted by mod time then CLOSED bugs sorted by mod time
(defun org-sync-bug-sort (a b)
  "Return non-nil if bug A should appear before bug B."
  (cl-flet ((time-less-safe (a b)
                            (if (and a b)
                                (time-less-p a b)
                              (or a b))))
    (let* ((ao (eq 'open (org-sync-get-prop :status a)))
           (bc (not (eq 'open (org-sync-get-prop :status b))))
           (am (time-less-safe
                (org-sync-get-prop :date-modification b)
                (org-sync-get-prop :date-modification a))))
      (or
       (and ao am)
       (and bc am)
       (and ao bc)))))

(defun org-sync-buglist-to-element (bl)
  "Return buglist BL as an element."
  (let* ((skip '(:title :bugs :date-cache))
         (sorted (sort (org-sync-get-prop :bugs bl) 'org-sync-bug-sort))
         (elist (delq nil (mapcar 'org-sync-bug-to-element sorted)))
         (title (org-sync-get-prop :title bl))
         (url (org-sync-get-prop :url bl))
         (props (sort (mapcar
                       ;; stringify prop name
                       (lambda (x)
                         (cons (substring (symbol-name (car x)) 1) (cdr x)))
                       ;; remove skipped prop
                       (cl-remove-if (lambda (x)
                                       (memq (car x) skip))
                                     (org-sync-plist-to-alist bl)))
                      ;; sort prop by key
                      (lambda (a b)
                        (string< (car a) (car b))))))

    (org-sync-set-prop :bugs sorted bl)
    `(headline
      (:level 1 :title (,title))
      (section
       nil
       ,(org-sync-alist-to-property-drawer props))
      ,@elist)))

(defun org-sync-filter-list (list minus)
  "Return a copy of LIST without elements in MINUS."
  (let ((final (cl-copy-seq list)))
    (mapc (lambda (x)
            (delq x final)) minus)
    final))

(defun org-sync-bug-to-element (b)
  "Return bug B as a TODO element if it is visible or nil."
  ;; not in PROPERTIES block
  (let* ((skip '(:title :status :desc :old-bug
                        :date-deadline :date-creation :date-modification))
         (title (org-sync-get-prop :title b))
         (dtime (org-sync-get-prop :date-deadline b))
         (ctime (org-sync-get-prop :date-creation b))
         (mtime (org-sync-get-prop :date-modification b))
         (prop-alist (cl-loop for (a b) on b by #'cddr
                              if (and b (not (memq a skip)))
                              collect (cons (substring (symbol-name a) 1)
                                            (prin1-to-string b)))))
    (unless (org-sync-get-prop :delete b)
      ;; add date-xxx props manually in a human readable way.
      (push (cons
             "date-creation"
             (org-sync-time-to-string ctime)) prop-alist)
      (push (cons
             "date-modification"
             (org-sync-time-to-string mtime)) prop-alist)

      ;; sort PROPERTIES by property name
      (setq prop-alist (sort prop-alist
                             (lambda (a b)
                               (string< (car b) (car a)))))

      `(headline
        (:title ,(concat
                  title
                  (when dtime
                    (concat
                     " DEADLINE: "
                     (format-time-string (org-time-stamp-format) dtime))))
                :level 2
                :todo-type todo
                :todo-keyword ,(upcase (symbol-name (org-sync-get-prop :status b))))
        (section
         nil
         ,(org-sync-alist-to-property-drawer prop-alist)
         (fixed-width (:value ,(org-sync-get-prop :desc b))))))))

(defun org-sync-headline-url (e)
  "Returns the url of the buglist in headline E."
  (cdr (assoc "url"
              (org-sync-property-drawer-to-alist
               (car (org-element-contents
                     (car (org-element-contents e))))))))

(defun org-sync-buglist-headline-p (elem)
  "Return t if ELEM is a buglist headline."
  (and
   (eq (org-element-type elem) 'headline)
   (stringp (org-sync-headline-url elem))))

(defun org-sync-property-drawer-to-alist (drawer)
  "Return the alist of all key value pairs."
  (org-element-map drawer
                   'node-property
                   (lambda (x) (cons (org-element-property :key x)
                                (org-element-property :value x)))))

(defun org-sync-alist-to-property-drawer (alist)
  "Return the property drawer corresponding to an alist of key
  value pairs"
  `(property-drawer nil
                    ,(mapcar
                      (lambda (x) `(node-property (:key ,(car x) :value ,(cdr x))))
                      alist)))

(defun org-sync-headline-to-buglist (h)
  "Return headline H as a buglist."
  (let* ((skip '(:url))
         (alist (org-sync-property-drawer-to-alist
                 (car (org-element-contents
                       (car (org-element-contents h))))))
         (title (car (org-element-property :title h)))
         (url (cdr (assoc "url" alist)))
         (bugs (mapcar
                'org-sync-headline-to-bug
                (nthcdr 1 (org-element-contents h))))
         (bl `(:title ,title
                      :url ,url
                      :bugs ,bugs)))

    ;; add all other properties
    (mapc (lambda (x)
            (let ((k (org-sync-propertize (car x)))
                  (v (cdr x)))
              (unless (memq k skip)
                (org-sync-set-prop k v bl))))
          alist)

    bl))

(defun org-sync-headline-to-bug (h)
  "Return headline H as a bug."
  (let* ((todo-keyword (org-element-property :todo-keyword h))
         ;; properties to skip when looking at the PROPERTIES block
         (skip '(:status :title :desc :date-deadline :date-creation :date-modification))
         (status (intern (downcase (or todo-keyword "open"))))
         (dtime (org-sync-parse-date (org-element-property :deadline h)))
         (title (car (org-element-property :title h)))
         (section (org-element-contents (car (org-element-contents h))))
         (headline-alist (org-sync-property-drawer-to-alist
                          (car
                           (org-element-contents
                            (car (org-element-contents h))))))
         (ctime (org-sync-parse-date (cdr (assoc "date-creation" headline-alist))))
         (mtime (org-sync-parse-date (cdr (assoc "date-modification" headline-alist))))
         desc
         bug)

    (dolist (e section)
      (let ((type (org-element-type e))
            (content (org-element-contents e)))
        (cond
         ;; interpret quote block as actual text
         ((eq type 'fixed-width)
          (setq desc (concat desc (org-element-property :value e))))

         ;; ignore these
         ((or (eq type 'property-drawer)
              (eq type 'planning)
              (and (eq type 'paragraph)
                   (string-match "^ *DEADLINE: " (car content))))
          nil)

         ;; else, interpret via org-element
         (t
          (setq desc (concat desc (org-element-interpret-data e)))))))

    ;; deadlines can be either on the same line as the headline or
    ;; on the next one.  org-element doesn't parse it the same way
    ;; when on the same line, remove DEADLINE tag from title
    ;; else ignore DEADLINE tag in paragraph
    (when dtime
      (setq title (replace-regexp-in-string " DEADLINE: " "" title)))

    (setq bug (list
               :status status
               :title title
               :desc desc
               :date-deadline dtime
               :date-creation ctime
               :date-modification mtime))

    ;; add all properties
    (mapc (lambda (x)
            (let ((k (org-sync-propertize (car x)))
                  (v (when (and (cdr x) (not (equal (cdr x) "")))
                       (read (cdr x)))))
              (unless (memq k skip)
                (setq bug (cons k (cons v bug)))))) headline-alist)
    bug))

(defun org-sync-find-buglists (elem)
  "Return every buglist headlines in ELEM."
  (let ((type (org-element-type elem))
        (contents (org-element-contents elem)))
    (cond
     ;; if it's a buglist, return it
     ((org-sync-buglist-headline-p elem)
      elem)
     ;; else if it contains elements, look recursively in it
     ((or (eq type 'org-data) (memq type org-element-greater-elements))
      (let (buglist)
        (mapc (lambda (e)
                (let ((h (org-sync-find-buglists e)))
                  (when h
                    (setq buglist (cons h buglist)))))
              contents)
        buglist))
     ;; terminal case
     (t
      nil))))

(defun org-sync-add-keyword (tree key val)
  "Add KEY:VAL as a header in TREE by side-effects and return TREE.
If KEY is already equal to VAL, no change is made."
  (catch :exit
    (let* ((section (cl-first (org-element-contents tree))))
      (when (and (eq 'org-data (org-element-type tree))
                 (eq 'section  (org-element-type section)))

        (dolist (e (org-element-contents section))
          (let* ((type (org-element-type e))
                 (ekey (org-element-property :key e))
                 (eval (org-element-property :value e)))

            (when (and (eq 'keyword type)
                       (string= ekey key)
                       (string= eval val))
              (throw :exit nil))))

        (setf (nthcdr 2 section)
              (cons
               `(keyword (:key ,key :value ,val))
               (org-element-contents section))))))
  tree)

(defun org-sync-org-reparse ()
  "Reparse current buffer."
  ;; from org-ctrl-c-ctrl-c, thanks to vsync in #org-mode
  (let ((org-inhibit-startup-visibility-stuff t)
        (org-startup-align-all-tables nil))
    (when (boundp 'org-table-coordinate-overlays)
      (mapc 'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (org-save-outline-visibility 'use-markers (org-mode-restart))))

(defun org-sync-import (url)
  "Fetch and insert at point bugs from URL."
  (interactive "sURL: ")
  (org-sync-with-backend url
   (let* ((buglist (org-sync--fetch-buglist nil))
          (elem (org-sync-buglist-to-element buglist))
          (bug-keyword '(sequence "OPEN" "|" "CLOSED")))

     ;; we add the buglist to the cache
     (org-sync-set-prop :date-cache (current-time) buglist)
     (org-sync-set-cache org-sync-base-url buglist)

     (save-excursion
       (insert (org-element-interpret-data
                `(org-data nil ,elem)))

       (unless (member bug-keyword org-todo-keywords)
         (goto-char (point-min))
         (insert "#+TODO: OPEN | CLOSED\n")
         (add-to-list 'org-todo-keywords bug-keyword)

         ;; the buffer has to be reparsed in order to have the new
         ;; keyword taken into account
         (org-sync-org-reparse)))))
  (message "Import complete."))

(defun org-sync-get-bug-id (buglist id)
  "Return bug ID from BUGLIST."
  (when id
      (catch :exit
        (mapc (lambda (x)
                (let ((current-id (org-sync-get-prop :id x)))
                  (when (and (numberp current-id) (= current-id id))
                    (throw :exit x))))
              (org-sync-get-prop :bugs buglist))
        nil)))

(defun org-sync-buglist-dups (buglist)
  "Return non-nil if BUGLIST contains bugs with the same id.
The value returned is a list of duplicated ids."
  (let ((hash (make-hash-table))
        (dups))
    (mapc (lambda (x)
            (let ((id (org-sync-get-prop :id x)))
              (puthash id (1+ (gethash id hash 0)) hash)))
          (org-sync-get-prop :bugs buglist))
    (maphash (lambda (id nb)
               (when (> nb 1)
                 (push id dups))) hash)
    dups))

(defun org-sync-time-max (&rest timelist)
  "Return the largest time in TIMELIST."
  (cl-reduce (lambda (a b)
               (if (and a b)
                   (if (time-less-p a b) b a))
               (or a b))
             timelist))

(defun org-sync-buglist-last-update (buglist)
  "Return the most recent creation/modi date in BUGLIST."
  (apply 'org-sync-time-max (cl-loop for x in (org-sync-get-prop :bugs buglist)
                               collect (org-sync-get-prop :date-creation x) and
                               collect (org-sync-get-prop :date-modification x))))

(defun org-sync-set-equal (a b)
  "Return t if list A and B have the same elements, no matter the order."
  (catch :exit
    (mapc (lambda (e)
            (unless (member e b)
              (throw :exit nil)))
          a)
    (mapc (lambda (e)
            (unless (member e a)
              (throw :exit nil)))
          b)
    t))

(defun org-sync-parse-date (date)
  "Parse and return DATE as a time or nil."
  (when (and (stringp date) (not (string= date "")))
    (date-to-time date)))

(defun org-sync-time-to-string (time)
  "Return TIME as a full ISO 8601 date string."
  (format-time-string "%Y-%m-%dT%T%z" time))

(defun org-sync-bug-diff (a b)
  "Return an alist of properties that differs in A and B or nil if A = B.
The form of the alist is ((:property . (valueA valueB)...)"
  (let ((diff)
        (props-list
         (append
          (cl-loop for (akey aval) on a by #'cddr collect akey)
          (cl-loop for (bkey bval) on b by #'cddr collect bkey))))
    (delete-dups props-list)
    (dolist (key props-list diff)
      (let ((va (org-sync-get-prop key a))
            (vb (org-sync-get-prop key b)))
        (unless (equal va vb)
          (setq diff (cons `(,key . (,va ,vb)) diff)))))))

(defun org-sync-bug-prop-equalp (prop a b)
  "Return t if bug A PROP = bug B PROP, nil otherwise."
  (equal (org-sync-get-prop prop a) (org-sync-get-prop prop b)))

(defun org-sync-buglist-diff (a b)
  "Return a diff buglist which turns buglist A to B when applied.
This function makes the assumption that A ⊂ B."
  (let (diff)
    (dolist (bbug (org-sync-get-prop :bugs b))
      (let ((abug (org-sync-get-bug-id a (org-sync-get-prop :id bbug))))
        (when (or (null abug) (org-sync-bug-diff abug bbug))
          (push bbug diff))))
    `(:bugs ,diff)))

(defun org-sync-merge-diff (local remote)
  "Return the merge of LOCAL diff and REMOTE diff.
The merge is the union of the diff.  Conflicting bugs are tagged
with :sync conflict-local or conflict-remote."
  (let ((added (make-hash-table))
        merge)
    ;; add all local bugs
    (dolist (lbug (org-sync-get-prop :bugs local))
      (let* ((id (org-sync-get-prop :id lbug))
             (rbug (org-sync-get-bug-id remote id))
            rnew lnew)

        ;; if there's a remote bug with the same id, we have a
        ;; conflict

        ;; if the local bug has a sync prop, it was merged by the
        ;; user, so we keep the local one (which might be the
        ;; remote from a previous sync)
        (if (and rbug (null (org-sync-get-prop :sync lbug)) (org-sync-bug-diff lbug rbug))
            (progn
              (setq lnew (copy-tree lbug))
              (org-sync-set-prop :sync 'conflict-local lnew)
              (setq rnew (copy-tree rbug))
              (org-sync-set-prop :sync 'conflict-remote rnew)
              (push rnew merge)
              (push lnew merge))
          (progn
            (push lbug merge)))

        ;; mark it
        (puthash id t added)))

    ;; add new remote bug which are the unmarked bugs in remote
    (dolist (rbug (org-sync-get-prop :bugs remote))
      (unless (gethash (org-sync-get-prop :id rbug) added)
        (push rbug merge)))

    `(:bugs ,merge)))

(defun org-sync-update-buglist (base diff)
  "Apply buglist DIFF to buglist BASE and return the result.
This is done according to `org-sync-props'."
  (let ((added (make-hash-table))
        new)
    (dolist (bug (org-sync-get-prop :bugs base))
      (let* ((id (org-sync-get-prop :id bug))
             (diff-bug (org-sync-get-bug-id diff id))
             new-bug)

        (if (and org-sync-props diff-bug)
            (progn
              (setq new-bug bug)
              (mapc (lambda (p)
                      (org-sync-set-prop p (org-sync-get-prop p diff-bug) new-bug))
                    org-sync-props))
          (setq new-bug (or diff-bug bug)))

        (push new-bug new)
        (puthash id t added)))

    (dolist (bug (org-sync-get-prop :bugs diff))
      (let ((id (org-sync-get-prop :id bug)))
        (when (or (null id) (null (gethash id added)))
          (push bug new))))

    (let ((new-buglist (cl-copy-list base)))
      (org-sync-set-prop :bugs new new-buglist)
      new-buglist)))

(defun org-sync-remove-unidentified-bug (buglist)
  "Remove bugs without id from BUGLIST."
  (let ((new-bugs))
    (dolist (b (org-sync-get-prop :bugs buglist))
      (when (org-sync-get-prop :id b)
        (push b new-bugs)))
    (org-sync-set-prop :bugs new-bugs buglist)
    buglist))

(defun org-sync-replace-headline-by-buglist (headline buglist)
  "Replace HEADLINE by BUGLIST by side effects."
  (let ((new-headline (org-sync-buglist-to-element buglist)))
    (setf (car headline) (car new-headline)
          (cdr headline) (cdr new-headline))))

(defun org-sync-show-conflict (buglist url)
  "Show conflict in BUGLIST at URL in conflict window."
  (let ((buf (get-buffer-create org-sync-conflict-buffer)))
    (with-help-window buf
      (with-current-buffer buf
        (erase-buffer)
        (org-mode)
        (insert "There were some conflicts while merging.  Here
are the problematic items.  Look at the :sync property to know
their origin. Copy what you want to keep in your org buffer and
sync again.\n\n")
        (dolist (b (org-sync-get-prop :bugs buglist))
          (when (and b (org-sync-get-prop :sync b))
            (insert (org-element-interpret-data (org-sync-bug-to-element b))
                    "\n")))))))

(defun org-sync-getalist (obj &rest keys)
  "Apply assoc in nested alist OBJ with KEYS."
  (let ((p obj))
    (dolist (k keys p)
      (setq p (cdr (assoc k p))))))

(defun org-sync-filter-bug (bug)
  "Filter BUG according to `org-sync-props'."
  (if org-sync-props
      (let ((new-bug `(:id ,(org-sync-get-prop :id bug))))
        (mapc (lambda (x)
                (org-sync-set-prop x (org-sync-get-prop x bug) new-bug))
              org-sync-props)
        new-bug)
    bug))

(defun org-sync-filter-diff (diff)
  "Filter DIFF according to `org-sync-props'."
  (when org-sync-props
    (let (final)
      (dolist (b (org-sync-get-prop :bugs diff))
        (let ((id (org-sync-get-prop :id b)))
          ;; drop new bugs
          (when id
            (push (org-sync-filter-bug b) final))))
      (org-sync-set-prop :bugs final diff)))
  diff)

(defun org-sync ()
  "Update buglists in current buffer."
  (interactive)
  (ignore-errors (kill-buffer org-sync-conflict-buffer))

  ;; parse the buffer and find the buglist-looking headlines
  (let* ((local-doc (org-element-parse-buffer))
         (local-headlines (org-sync-find-buglists local-doc)))

    ;; for each of these headlines, convert it to buglist
    (dolist (headline local-headlines)
      (let* ((local (org-sync-headline-to-buglist headline))
             (url (org-sync-get-prop :url local)))

        ;; if it has several bug with the same id, stop
        (when (org-sync-buglist-dups local)
          (error
           "Buglist \"%s\" contains unmerged bugs."
           (org-sync-get-prop :title local)))

        ;; local          cache          remote
        ;;    \          /    \          /
        ;;    parse    load   load     fetch
        ;;      \      /        \      /
        ;;     local-diff       remote-diff
        ;;             \        /
        ;;              \      /
        ;;             merged-diff --------send-------->
        ;;                                              (...)
        ;;               local   <--recv-updated-diff---
        ;;                 v
        ;;              merged
        ;;                 v
        ;;        new cache/local/remote

        ;; handle buglist with the approriate backend
        (org-sync-with-backend url
         (let* ((cache (org-sync-get-cache org-sync-base-url))
                (last-fetch (org-sync-get-prop :date-cache cache))
                (local-diff (org-sync-buglist-diff cache local))
                remote remote-diff merged merged-diff)

           ;; fetch remote buglist
           (if last-fetch
               ;; make a partial fetch and apply it to cache if the backend
               ;; supports it
               (let* ((partial-fetch (org-sync--fetch-buglist last-fetch)))
                 (if (org-sync-get-prop :since partial-fetch)
                     (setq remote (org-sync-update-buglist cache partial-fetch))
                   (setq remote partial-fetch)))
             (setq remote (org-sync--fetch-buglist nil)))
           ;; at this point remote is the full remote buglist

           (setq remote-diff (org-sync-buglist-diff cache remote))
           (setq merged-diff (org-sync-merge-diff local-diff remote-diff))

           ;; filter according to org-sync-props
           (org-sync-filter-diff merged-diff)

           (setq merged (org-sync-update-buglist local merged-diff))

           ;; if merged-diff has duplicate bugs, there's a conflict
           (let ((dups (org-sync-buglist-dups merged-diff)))
             (if dups
                 (progn
                   (message "Synchronization failed, manual merge needed.")
                   (org-sync-show-conflict merged-diff org-sync-base-url))

               ;; else update buffer and cache
               (setq merged
                     (org-sync-remove-unidentified-bug
                      (org-sync-update-buglist merged (org-sync--send-buglist merged-diff))))
               (org-sync-set-prop :date-cache (current-time) merged)
               (org-sync-set-cache org-sync-base-url merged)
               (message "Synchronization complete.")))

           ;; replace headlines in local-doc
           (org-sync-replace-headline-by-buglist headline merged)))))

    (org-sync-add-keyword local-doc "TODO" "OPEN | CLOSED")

    ;; since we replace the whole buffer, save-excusion doesn't work so
    ;; we manually (re)store the point
    (let ((oldpoint (point)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (org-element-interpret-data local-doc))
      (goto-char oldpoint))))

(defun org-sync-or-import ()
  "Synchronize current buffer or import an external document.

If no Org-sync elements are present in the buffer, ask for a URL
to import otherwise synchronize the buffer."
  (interactive)
  (let* ((local-doc (org-element-parse-buffer)))
    (if (org-sync-find-buglists local-doc)
        (org-sync)
      (call-interactively 'org-sync-import))))

(provide 'org-sync)
;;; org-sync.el ends here

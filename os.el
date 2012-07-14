;;; os.el --- Synchronize Org documents with external services

;; Copyright (C) 2012  Aurelien Aptel
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Keywords: org, synchronization
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

;; This package implements an extension to org-mode that synchnonizes
;; org document with external services. It provides an interface that
;; can be implemented in backends. The current focus is on bugtrackers
;; services.

;; The entry points are `os-import' and `os-sync'. The former prompts
;; for a URL to import, the latter pulls, merges and pushes every
;; buglists in the current buffer.

;; The usual workflow is first to import your buglist with
;; `os-import', modify it or add a bug and run `os-sync'.

;; A buglist is a top-level headline which has a :url: in its
;; PROPERTIES block. This headline is composed of a list of
;; subheadlines which corresponds to bugs. The requirement for a bug
;; is to have a state, a title and an id. If you add a new bug, it
;; wont have an id but it will get one once you sync.

;; The state is an org TODO state. It can be either OPEN or CLOSED.
;; The title is just the title of the headline.
;; The id is a number in the PROPERTIES block of the headline.

;; Org DEADLINE timestamp are also handled and can be inserted in a
;; bug headline which can then be used by the backend if it supports
;; it.

;; Paragraphs under bug-headlines are considered as their description.
;; Additionnal data used by the backend are in the PROPERTIES block of
;; the bug.

;; To add a bug, just insert a new headline under the buglist you want
;; to modify e.g.:
;;     ** OPEN my new bug
;; Then simply call `os-sync'.

;;; Code:

;; The data structures used to represent bugs and buglists are simple plists.
;; It is what backend have to handle, process or return.

;; Buglist example:

;; '(:title "My buglist"
;;   :url "http://github.com/repos/octocat/Hello-World"
;;   :bugs (BUGS...))

;; Bug example:

;; '(:id 3
;;   :status 'open ;; or 'closed
;;   :sync 'delete ;; or 'change 'conflict-local 'conflict-remote 'same (= nil)
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

;; Some accesors are available for both structure. See `os-set-prop',
;; and `os-get-prop'.


;; When importing an URL, Org-sync matches the URL against the
;; variable `os-backend-alist' which maps regexps to backend
;; symbols.  The backend symbol is then used to call the backend
;; functions.  When these functions are called, the variable
;; `os-backend' and `os-base-url' are dynamically bound to
;; respectively the backend symbol and the cannonical URL for the
;; thing you are synching with.

;; The symbol part in a `os-backend-alist' pair must be a variable
;; defined in the backend. It is an alist that maps verb to function
;; symbol. Each backend must implement at least 3 verbs:

;; * base-url (param: URL)

;; Given the user URL, returns the cannonical URL
;; to represent it. This URL will be available dynamically to all of
;; your backend function through the `os-base-url' variable.

;; * fetch-buglist (param: LAST-FETCH-TIME)

;; Fetch the buglist at `os-base-url'. If LAST-FETCH-TIME is non-nil,
;; and you only fetched things modified since it, you are expected to
;; set the property :since to it in the buglist you return. You can
;; add whatever properties you want in a bug. The lisp printer is used
;; to persist them in the buffer.

;; * send-buglist (param: BUGLIST)

;; Send BUGLIST to `os-base-url' and return the final, updated buglist
;; available on the remote end.


;; When synchronizing, Org-sync parses the current buffer using
;; org-element and convert any found buglist headline to a buglist
;; data structure. See `os-headline-to-buglist', `os-headline-to-bug'.

;; When writing buglists back to the document, Org-sync converts them
;; to elements -- the data structure used by org-element -- which are
;; then interpreted by `org-element-interpret-data'. The resulting
;; string is then inserted in the buffer. See `os-buglist-to-element'
;; and `os-bug-to-element'.

(eval-when-compile (require 'cl))
(require 'org)
(require 'org-element)

(defvar os-backend nil
  "Org-sync current backend.")

(defvar os-base-url nil
  "Org-sync current base url.")

(defvar os-backend-alist
  '(("github.com/\\(?:repos/\\)?[^/]+/[^/]+" . os-github-backend)
    ("bitbucket.org/[^/]+/[^/]+"             . os-bb-backend))
  "Alist of url patterns vs corresponding org-sync backend.")

(defvar os-cache-file (concat user-emacs-directory "org-sync-cache")
  "Path to Org-sync cache file.")

(defvar os-cache-alist nil
  "Org-sync cache for buglists.
Maps URLs to buglist cache.")

(defun os-action-fun (action)
  "Return current backend ACTION function or nil."
  (unless (or (null action) (null os-backend))
    (let ((fsym (assoc-default action (eval os-backend))))
      (when (fboundp fsym)
        fsym))))

(defun os-get-backend (url)
  "Return backend symbol matching URL from os-backend-alist or nil."
  (assoc-default url os-backend-alist 'string-match))

(defmacro os-with-backend (backend &rest body)
  "Eval BODY with os-backend set to corresponding BACKEND.

If BACKEND evals to a string it is passed to os-get-backend, the
resulting symbol is dynamically assigned to os-backend. The url
is passed to os--base-url and dynamically assigned to
os-base-url.

Else BACKEND should be a backend symbol. It is
assigned to os-backend."
  (declare (indent 1) (debug t))
  (let ((res (gensym))
        (url (gensym)))

    `(let* ((,res ,backend)
            (,url))
       (when (stringp ,res)
         (setq ,url ,res)
         (setq ,res (os-get-backend ,url)))
       (unless (symbolp ,res)
         (error "Backend %s does not evaluate to a symbol."
                (prin1-to-string ',backend)))
       (let* ((os-backend ,res)
              (os-base-url (os--base-url ,url)))
         ,@body))))

(defun os-set-cache (url buglist)
  "Update URL to BUGLIST in `os-cache-alist'."
  (let ((cell (assoc url os-cache-alist)))
    (if cell
        (setcdr cell buglist)
      (push (cons url buglist) os-cache-alist))))

(defun os-get-cache (url)
  "Return the buglist at URL in cache or nil."
    (cdr (assoc url os-cache-alist)))

(defun os-write-cache ()
  (with-temp-file os-cache-file
    (prin1 `(setq os-cache-alist ',os-cache-alist) (current-buffer))))

(defun os-load-cache ()
  (load os-cache-file 'noerror nil))

(defun os-propertize (sym)
  "Return sym as a property i.e. prefixed with :."
  (intern (concat ":" (symbol-name sym))))

(defun os-get-prop (key b)
  "Return value of the property KEY in buglist or bug B."
  (plist-get b key))

(defun os-set-prop (key val b)
  "Set KEY to VAL in buglist or bug B."
  (plist-put b key val))

(defun os-append! (elem list)
  "Add ELEM at the end of LIST by side effect if it is not
already present.

Return ELEM if it was added, nil otherwise."
  (catch :exit
    (let ((p list))
      (while (cdr p)
        (when (equal (car p) elem)
          (throw :exit nil))
        (setq p (cdr p)))
      (setcdr p (cons elem nil))
      elem)))

(defun os--send-buglist (buglist)
  "Send a BUGLIST on the bugtracker and return an updated buglist."
  (let ((f (os-action-fun 'send-buglist)))
        (if f
            (funcall f buglist)
          (error "No send backend available."))))

(defun os--fetch-buglist (last-update)
  "Return the buglist at url REPO."
  (let ((f (os-action-fun 'fetch-buglist)))
        (if f
            (funcall f last-update)
          (error "No fetch backend available."))))


(defun os--base-url (url)
  "Return the base url of URL."
  (let ((f (os-action-fun 'base-url)))
        (if f
            (funcall f url)
          (error "No base-url backend available."))))


(defun os-buglist-to-element (bl)
  "Return buglist BL as an element."
  (let* ((elist (delq nil (mapcar 'os-bug-to-element (os-get-prop :bugs bl))))
         (title (os-get-prop :title bl))
         (url (os-get-prop :url bl)))
    `(headline
      (:level 1 :title (,title))
      (section
       nil
       (property-drawer (:properties (("url" . ,url)))))
      ,@elist)))

(defun os-filter-list (list minus)
  "Return a copy of LIST without elements in MINUS."
  (let ((final (copy-seq list)))
    (mapc (lambda (x)
            (delq x final)) minus)
    final))

(defun os-bug-to-element (b)
  "Return bug B as a TODO element if it is visible, nil otherwise."
  ;; not in PROPERTIES block
  (let* ((skip '(:title :status :desc :old-bug :date-deadline))
         (title (os-get-prop :title b))
         (deadline (os-get-prop :date-deadline b))
         (prop-alist (loop for (a b) on b by #'cddr
                           if (and b (not (memq a skip)))
                           collect (cons (substring (symbol-name a) 1)
                                         (prin1-to-string b)))))
    (unless (eq 'delete (os-get-prop :sync b))
      ;; sort PROPERTIES by property name
      (setq prop-alist (sort prop-alist
                             (lambda (a b)
                               (string< (car b) (car a)))))


      `(headline
        (:title ,(if deadline
                     (list
                      (format "%s DEADLINE: " title)
                      `(timestamp
                        (:value
                         ,(format-time-string (org-time-stamp-format)))))
                   title)
                :level 2
                :todo-type todo
                :todo-keyword ,(upcase (symbol-name (os-get-prop :status b))))
        (section
         nil
         (property-drawer
          (:properties ,prop-alist))
         (paragraph nil ,(os-get-prop :desc b)))))))

(defun os-headline-url (e)
  "Returns the url of the buglist in headline E."
  (cdr (assoc "url"
              (org-element-property
               :properties
               (car (org-element-contents
                     (car (org-element-contents e))))))))

(defun os-buglist-headline-p (elem)
  "Return t if ELEM is a buglist headline."
  (and
   (eq (org-element-type elem) 'headline)
   (stringp (os-headline-url elem))))

(defun os-headline-to-buglist (h)
  "Return headline H as a buglist."
  (let* ((alist (org-element-property
                 :properties
                 (car (org-element-contents
                       (car (org-element-contents h))))))
         (title (car (org-element-property :title h)))
         (url (cdr (assoc "url" alist))))
    `(:title ,title
             :url ,url
             :bugs ,(mapcar
                     'os-headline-to-bug
                     (nthcdr 1 (org-element-contents h))))))


(defun os-headline-to-bug (h)
  "Return headline H as a bug."
  (let* ((todo-keyword (org-element-property :todo-keyword h))
         ;; properties to skip when looking at the PROPERTIES block
         (skip '(:status :title :sync :desc :date-deadline))
         (status (if (string= "OPEN" todo-keyword) 'open 'closed))
         (deadline (os-parse-date (org-element-property :deadline h)))
         (title (car (org-element-property :title h)))
         (sync (when (string= "DELETE" todo-keyword) 'delete))
         (section (org-element-contents (car (org-element-contents h))))
         (headline-alist (org-element-property
                          :properties
                          (car
                           (org-element-contents
                            (car (org-element-contents h))))))
         (desc (org-element-interpret-data
                (remove-if
                 (lambda (e)
                   (let ((type (org-element-type e))
                         (content (org-element-contents e)))
                     (or (eq type 'property-drawer)
                         (and (eq type 'paragraph)
                              (string-match "^ *DEADLINE: " (car content))))))
                 section)))

         bug)

    ;; deadlines can be either on the same line as the headline or
    ;; on the next one. org-element doesn't parse it the same way
    ;; when on the same line, remove DEADLINE tag from title
    ;; else ignore DEADLINE tag in paragraph
    (when deadline
      (setq title (replace-regexp-in-string " DEADLINE: " "" title)))

    (setq bug (list
               :status status
               :title title
               :sync sync
               :desc desc
               :date-deadline deadline))


    (mapc (lambda (x)
            (let ((k (intern (concat ":" (car x))))
                  (v (when (and (cdr x) (not (equal (cdr x) "")))
                       (read (cdr x)))))
              (unless (memq k skip)
                (setq bug (cons k (cons v bug)))))) headline-alist)
    bug))

(defun os-find-buglists (elem)
  "Return every buglist headlines in ELEM."
  (let ((type (org-element-type elem))
        (contents (org-element-contents elem)))
    (cond
     ;; if it's a buglist, return it
     ((os-buglist-headline-p elem)
      elem)
     ;; else if it contains elements, look recursively in it
     ((or (eq type 'org-data) (memq type org-element-greater-elements))
      (let (buglist)
        (mapc (lambda (e)
                (let ((h (os-find-buglists e)))
                  (when h
                    (setq buglist (cons h buglist)))))
              contents)
        buglist))
     ;; terminal case
     (t
      nil))))

(defun os-add-keyword (tree key val)
  "Add KEY:VAL as a header in TREE by side-effects and return TREE.
If KEY is already equal to VAL, no change is made."
  (catch :exit
    (let* ((section (first (org-element-contents tree))))
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

(defun os-org-reparse ()
  "Reparse current buffer."
  ;; from org-ctrl-c-ctrl-c, thanks to vsync in #org-mode
  (let ((org-inhibit-startup-visibility-stuff t)
        (org-startup-align-all-tables nil))
    (when (boundp 'org-table-coordinate-overlays)
      (mapc 'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (org-save-outline-visibility 'use-markers (org-mode-restart))))

(defun os-import (url)
  "Fetch and insert at point bugs from URL."
  (interactive "sURL: ")
  (os-with-backend url
   (let* ((buglist (os--fetch-buglist nil))
          (elem (os-buglist-to-element buglist))
          (bug-keyword '(sequence "OPEN" "|" "CLOSED")))

     ;; we add the buglist to the cache
     (os-set-prop :date-cache (current-time) buglist)
     (os-set-cache os-base-url buglist)

     (save-excursion
       (insert (org-element-interpret-data
                `(org-data nil ,elem)))

       (unless (member bug-keyword org-todo-keywords)
         (goto-char (point-min))
         (insert "#+TODO: OPEN | CLOSED\n")
         (add-to-list 'org-todo-keywords bug-keyword)

         ;; the buffer has to be reparsed in order to have the new
         ;; keyword taken into account
         (os-org-reparse)))))
  (message "Import complete."))

(defun os-get-bug-id (buglist id)
  "Return bug ID from BUGLIST."
  (when id
      (catch :exit
        (mapc (lambda (x)
                (let ((current-id (os-get-prop :id x)))
                  (when (and (numberp current-id) (= current-id id))
                    (throw :exit x))))
              (os-get-prop :bugs buglist))
        nil)))

(defun os-buglist-dups (buglist)
  "Return non-nil if BUGLIST contains bugs with the same id.
The value returned is a list of duplicated ids."
  (let ((hash (make-hash-table))
        (dups))
    (mapc (lambda (x)
            (let ((id (os-get-prop :id x)))
              (puthash id (1+ (gethash id hash 0)) hash)))
          (os-get-prop :bugs buglist))
    (maphash (lambda (id nb)
               (when (> nb 1)
                 (push id dups))) hash)
    dups))

(defun os-time-max (&rest timelist)
  "Return the largest time in TIMELIST."
  (reduce (lambda (a b)
            (if (and a b)
                (if (time-less-p a b) b a))
            (or a b))
          timelist))

(defun os-buglist-last-update (buglist)
  "Return the most recent creation/modi date in BUGLIST."
  (apply 'os-time-max (loop for x in (os-get-prop :bugs buglist)
                            collect (os-get-prop :date-creation x) and
                            collect (os-get-prop :date-modification x))))

(defun os-merge-buglist (local remote)
  "Return a buglist merged from buglist LOCAL & REMOTE."
  (let* ((since (os-get-prop :since remote))
         (local-bugs (os-get-prop :bugs local))
         (remote-bugs (os-get-prop :bugs remote))
         (merged-bugs))

    ;; A. handle local bugs and fill the merged-bugs list
    (dolist (loc local-bugs)
      (let* ((id (os-get-prop :id loc))
             (rem (os-get-bug-id remote id)))

        (cond
         ;; if the bug doesn't exist in remote:
         ((null rem)
          (cond

           ;; if local id is invalid, push local as a new bug on
           ;; remote
           ((not (and (numberp id) (>= id 0)))
            (os-set-prop :sync 'new loc)
            (setq merged-bugs (append merged-bugs (list loc))))

           ;; if local id is valid & fetch was partial, local didn't
           ;; change: keep it
           (since
            (os-set-prop :sync 'same loc)
            (setq merged-bugs (append merged-bugs (list loc))))

           ;; else, the bug was deleted on remote: don't append it to
           ;; the merged bugs list
           (t nil)))

         ;; if the bug was marked to be deleted, insert it but don't
         ;; display it
         ((eq 'delete (os-get-prop :sync loc))
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; if local bug = remote bug, nothing to sync
         ((not (os-bug-diff loc rem))
          (os-set-prop :sync 'same loc)
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; if local != remote but the modification date is equal, we
         ;; keep the local and push it. we keep a ref to the remote to
         ;; compute a diff when we push
         ((os-bug-prop-equalp :date-modification loc rem)
          (os-set-prop :sync 'change loc)
          (os-set-prop :old-bug rem loc)
          (setq merged-bugs (append merged-bugs (list loc))))

         ;; local and remote differ, insert both and let the user merge
         (t
          (os-set-prop :sync 'conflict-local  loc)
          (os-set-prop :sync 'conflict-remote rem)
          (setq merged-bugs (append merged-bugs (list loc rem)))))))

    ;; B. handle remote bugs and complete the merged-bugs list
    (dolist (rem remote-bugs)
      (let* ((id (os-get-prop :id rem))
             (loc (os-get-bug-id local id)))

        (cond
         ;; if local bug = remote bug => already handled in step A.
         ;; local and remote differ   => already handled in step A.

         ;; if the bug doesn't exist in local, it's a new one
         ;; don't resend it, and insert it
         ((null loc)
          (os-set-prop :sync 'same rem)
          (setq merged-bugs (append merged-bugs (list rem)))))))

    ;; return new merged buglist
    `(:title ,(os-get-prop :title local) ;; always keep local title
             :url ,(os-get-prop :url local)
             :bugs ,merged-bugs)))

(defun os-set-equal (a b)
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

(defun os-parse-date (date)
  "Parse and return DATE as a time or nil."
  (when date
    (date-to-time date)))

(defun os-time-to-string (time)
  "Return TIME as a full ISO 8601 date string."
  (format-time-string "%Y-%m-%dT%T%z" time))

(defun os-bug-diff (a b)
  "Return an alist of properties that differs in A and B or nil if A = B.
The form of the alist is ((:property . (valueA valueB)...)"
  (let ((diff)
        (props-list
         (append
          (loop for (akey aval) on a by #'cddr collect akey)
          (loop for (bkey bval) on b by #'cddr collect bkey))))
    (delete-dups props-list)
    (dolist (key props-list diff)
      (let ((va (os-get-prop key a))
            (vb (os-get-prop key b)))
        (unless (equal va vb)
          (setq diff (cons `(,key . (,va ,vb)) diff)))))))

(defun os-bug-prop-equalp (prop a b)
  "Return t if bug A PROP = bug B PROP, nil otherwise."
  (equal (os-get-prop prop a) (os-get-prop prop b)))

(defun os-buglist-diff (a b)
  "Return a diff buglist which turns buglist A to B when applied.
This function makes the assumption that A ⊂ B."
  (let (diff)
    (dolist (bbug (os-get-prop :bugs b))
      (let ((abug (os-get-bug-id a (os-get-prop :id bbug))))
        (when (or (null abug) (os-bug-diff abug bbug))
          (push bbug diff))))
    `(:bugs ,diff)))

(defun os-merge-diff (local remote)
  "Return the merge of LOCAL diff and REMOTE diff.
The merge is the union of the diff. Conflicting bugs are tagged
with :sync conflict-local or conflict-remote."
  (let ((added (make-hash-table))
        merge)
    ;; add all local bugs
    (dolist (lbug (os-get-prop :bugs local))
      (let* ((id (os-get-prop :id lbug))
             (rbug (os-get-bug-id remote id))
            rnew lnew)
        ;; add both remote and local with special tags when there's a
        ;; conflict
        (if rbug ;; TODO: maybe test diff between rbug lbug
            (progn
              (setq lnew (copy-tree lbug))
              (os-set-prop :sync 'conflict-local lnew)
              (setq rnew (copy-tree rbug))
              (os-set-prop :sync 'conflict-remote rnew)
              (push rnew merge)
              (push lnew merge))
          (push lbug merge))
        (puthash id t added)))

    (dolist (rbug (os-get-prop :bugs remote))
      (unless (gethash (os-get-prop :id rbug) added)
        (push rbug merge)))

    `(:bugs ,merge)))

(defun os-update-buglist (base diff)
  "Apply buglist DIFF to buglist BASE and return the result."
  (let ((added (make-hash-table))
        new)
    (dolist (bug (os-get-prop :bugs base))
      (let* ((id (os-get-prop :id bug))
             (diff-bug (os-get-bug-id diff id))
             (new-bug (or diff-bug bug)))
        (push new-bug new)
        (puthash id t added)))

    (dolist (bug (os-get-prop :bugs diff))
      (let ((id (os-get-prop :id bug)))
        (when (or (null id) (null (gethash id added)))
          (push bug new))))

    (let ((new-buglist (copy-list base)))
      (os-set-prop :bugs new new-buglist)
      new-buglist)))


;; TODO: add sync tags to make backends work again.
(defun os-sync ()
  "Update buglists in current buffer."
  (interactive)
  (let* ((local-doc (org-element-parse-buffer))
         (local-headlines (os-find-buglists local-doc)))

    (dolist (headline local-headlines)
      (let* ((local (os-headline-to-buglist headline))
             (url (os-get-prop :url local)))

        (when (os-buglist-dups local)
          (error
           "Buglist \"%s\" contains unmerged bugs."
           (os-get-prop :title local)))

        (os-with-backend url
          (let* ((cache (os-get-cache os-base-url))
                 (last-fetch (os-get-prop :date-cache cache))
                 (local-diff (os-buglist-diff cache local))
                 remote remote-diff merged merged-diff)

            ;; fetch remote buglist
            (if last-fetch
                ;; make a partial fetch and apply it to cache if the backend
                ;; supports it
                (let* ((partial-fetch (os--fetch-buglist last-fetch)))
                  (if (os-get-prop :since partial-fetch)
                      (setq remote (os-update-buglist cache partial-fetch))
                    (setq remote partial-fetch)))
              (setq remote (os--fetch-buglist nil)))
            ;; at this point remote is the full remote buglist

            (setq remote-diff (os-buglist-diff cache remote))
            (setq merged-diff (os-merge-diff local-diff remote-diff))
            (setq merged (os-update-buglist remote merged-diff))

            (pp merged-diff)

            (unless (os-buglist-dups merged-diff)
              (os--send-buglist merged-diff)
              (os-set-prop :date-cache (current-time) merged)
              (os-set-cache os-base-url merged))

            ;; replace headlines in local-doc
            (let ((new-headline (os-buglist-to-element merged)))
              (setf (car headline) (car new-headline)
                    (cdr headline) (cdr new-headline)))))))

    (os-add-keyword local-doc "TODO" "OPEN | CLOSED")

    ;; since we replace the whole buffer, save-excusion doesn't work so
    ;; we manually (re)store the point
    (let ((oldpoint (point)))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (org-element-interpret-data local-doc))
      (goto-char oldpoint))

    (message "Synchronization complete.")))

(provide 'org-sync)
;;; os.el ends here

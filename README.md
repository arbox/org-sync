[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/org-sync-badge.svg)](http://melpa.org/#/org-sync)
[![MELPA Stable](http://stable.melpa.org/packages/org-sync-badge.svg)](http://stable.melpa.org/#/org-sync)

# Org-sync: Synchronize Org-mode Files with Bug Tracking systems

Org-sync is a tool to synchronize online bugtrackers with org documents.
It is made for relatively small/medium projects: I find Org documents are not
really suited for handling large bug lists. The official homepage of the project
is http://orgmode.org/worg/org-contrib/gsoc2012/student-projects/org-sync/
on Worg. You can find the official git repo and contact informations there.

## Installation

### melpa

```
M-x package-install RET org-sync RET
```

### from source

Put the org-sync directory in your load-path and load the org-sync backend you
need. You can add this to your .emacs:

``` emacs-lisp
(add-to-list 'load-path "path/to/org-sync")
(mapc 'load
      '("org-sync" "org-sync-bb" "org-sync-github" "org-sync-rmine"))
```

Make sure you have `org-element.el` (it's part of recent org-mode >=). If you
don't have it you can download a recent version in the org-sync directory:

``` sh
wget -O org-element.el 'http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=lisp/org-element.el'
```

## Tutorial

Next, open a new org-mode buffer and run `M-x org-sync-import`.  It prompts you for
an URL.  You can try my Github test repo: `github.com/arbox/org-sync-test`.
Org-sync should import the issues from the repo. *Note*: This is just
a test repo, do not use it to report actual bugs.

Now, let's try to add a new issue.  First you have to set a
user/password to be able to modify the issue remotely.

Set the variable org-sync-github-auth to like so:
`(setq org-sync-github-auth '("ostesting" . "thisisostesting42"))`

Try to add another issue e.g. insert `** OPEN my test issue`.  You can
type a description under it if you want.

The next step is simple, just run `M-x org-sync`.  It synchronizes all
the buglists in the document.

## How to write a new backend

Writing a new backend is easy.  If something is not clear, try to read
the header in `org-sync.el` or one of the existing backend.

``` emacs-lisp
;; backend symbol/name: demo
;; the symbol is used to find and call your backend functions (for now)

;; what kind of urls does you backend handle?
;; add it to org-sync-backend-alist in org-sync.el:

(defvar org-sync-backend-alist
  '(("github.com/\\(?:repos/\\)?[^/]+/[^/]+"  . org-sync-github-backend)
    ("bitbucket.org/[^/]+/[^/]+"              . org-sync-bb-backend)
    ("demo.com"                               . org-sync-demo-backend)))

;; if you have already loaded org-sync.el, you'll have to add it
;; manually in that case just eval this in *scratch*
(add-to-list 'org-sync-backend-alist (cons "demo.com" 'org-sync-demo-backend))

;; now, in its own file org-sync-demo.el:

(require 'org-sync)

;; this is the variable used in org-sync-backend-alist
(defvar org-sync-demo-backend
  '((base-url      . org-sync-demo-base-url)
    (fetch-buglist . org-sync-demo-fetch-buglist)
    (send-buglist  . org-sync-demo-send-buglist))
  "Demo backend.")


;; this overrides org-sync--base-url.
;; the argument is the url the user gave.
;; it must return a cannonical version of the url that will be
;; available to your backend function in the org-sync-base-url variable.

;; In the github backend, it returns API base url
;; ie. https://api.github/reposa/<user>/<repo>

(defun org-sync-demo-base-url (url)
  "Return proper URL."
  "http://api.demo.com/foo")

;; this overrides org-sync--fetch-buglist
;; you can use the variable org-sync-base-url
(defun org-sync-demo-fetch-buglist (last-update)
  "Fetch buglist from demo.com (anything that happened after LAST-UPDATE)"
  ;; a buglist is just a plist
  `(:title "Stuff at demo.com"
           :url ,org-sync-base-url

           ;; add a :since property set to last-update if you return
           ;; only the bugs updated since it.  omit it or set it to
           ;; nil if you ignore last-update and fetch all the bugs of
           ;; the repo.

           ;; bugs contains a list of bugs
           ;; a bug is a plist too
           :bugs ((:id 1 :title "Foo" :status open :desc "bar."))))

;; this overrides org-sync--send-buglist
(defun org-sync-demo-send-buglist (buglist)
  "Send BUGLIST to demo.com and return updated buglist"
  ;; here you should loop over :bugs in buglist
  (dolist (b (org-sync-get-prop :bugs buglist))
    (cond
      ;; new bug (no id)
      ((null (org-sync-get-prop :id b)
        '(do-stuff)))

      ;; delete bug
      ((org-sync-get-prop :delete b)
        '(do-stuff))

      ;; else, modified bug
      (t
        '(do-stuff))))

  ;; return any bug that has changed (modification date, new bugs,
  ;; etc).  they will overwrite/be added in the buglist in org-sync.el

  ;; we return the same thing for the demo.
  ;; :bugs is the only property used from this function in org-sync.el
  `(:bugs ((:id 1 :title "Foo" :status open :desc "bar."))))
```

That's it.  A bug has to have at least an id, title and status properties.
Other recognized but optionnal properties are `:date-deadline`,
`:date-creation`, `:date-modification`, `:desc`. Any other properties are
automatically added in the `PROPERTIES` block of the bug via `prin1-to-string`
and are `read` back by org-sync.  All the dates are regular emacs time object.
For more details you can look at the github backend in `org-sync-github.el`.

## More information

You can find more in the `org-sync.el` commentary headers.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg

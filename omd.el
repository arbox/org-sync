;; omd.el --- org-merge-driver document generator

(defconst omd-bullet-type '("+" "-" "num"))

(defun omd-rand (min max)
  "Return random integer in [MIN;MAX[."
  (if (< max min)
      (rotatef min max))
  (let ((d (- max min)))
    (+ min (random d))))

(defun omd-random-word (&optional length)
  "Return random word."
  (unless length
    (setq length (omd-rand 2 7)))

  (let (chars)
    (apply 'string
           (dotimes (i length chars)
             (push (omd-rand 97 123) chars)))))

(defun omd-random-text (&optional lines length prefix)
  "Return random text.
The text has LINES lines and each line is approximately LENGTH
characters."
  (unless prefix
    (setq prefix ""))
  (unless length
    (setq length 70))
  (unless lines
    (setq lines 1))

  (let (text)
    (dotimes (n lines text)
      (if (/= n 0)
          (setq text (concat text "\n" prefix)))

      (let (line)
        (while (< (length line) length)
          (let ((w (omd-random-word)))
            (setq line (if line (concat line " " w) w))))
        (setq text (concat text line))))))


(defun omd-random-paragraph (&optional text)
  (unless text
    (setq text (omd-random-text (omd-rand 2 5))))
  `(paragraph nil ,text))

(defun omd-random-headline (&rest contents)
  (let ((title (omd-random-text 1 20)))
  `(headline (:title ,title) ,@contents)))

(defun omd-pick-random-element (list)
  (let ((len (length list)))
    (nth (omd-rand 0 len) list)))

(defun omd-random-list (&optional nitems bullet)
  (unless nitems
    (setq nitems (omd-rand 2 5)))
  (unless bullet
    (setq bullet (omd-pick-random-element omd-bullet-type)))

  (let* (items)
    (dotimes (i nitems)
      (push (omd-random-text (omd-rand 2 5) 30) items))

    `(list (:bullet ,bullet) ,@items)))


(defun omd-set-contents (elem contents)
  (setf (nthcdr 2 elem) contents))

(defun omd-get-contents (elem)
  (nthcdr 2 elem))

(defun omd-add-contents (elem &rest contents)
  (setcdr (last elem) contents))

(defun omd-get-prop (prop elem)
  (plist-get (nth 1 elem) prop))

(defun omd-set-prop (prop val elem)
  (setcar (cdr elem) (plist-put (nth 1 elem) prop val)))

(defalias 'omd-copy 'copy-tree)

(defun omd-new-doc (&rest contents)
  `(doc () ,@contents))

(defun omd-to-string (elem &optional level)
  (unless level
    (setq level 1))
  (let* ((type (nth 0 elem))
         (prop (nth 1 elem))
         (cont (nthcdr 2 elem)))

    (cond
     ((eq 'doc type)
      (mapconcat 'omd-to-string cont ""))

     ((eq 'headline type)
      (apply 'concat
             (make-string level ?*)
             " "
             (omd-get-prop :title elem)
             "\n"
             (mapcar (lambda (e)
                       (omd-to-string e (1+ level)))
                     cont)))

     ((eq 'list type)
      (let ((n 0)
            (bullet (omd-get-prop :bullet elem)))
        (apply 'concat
              (mapcar (lambda (item)
                        (incf n)
                        (let* ((prefix (if (string= "num" bullet)
                                           (format "%d. " n)
                                         (concat bullet " ")))
                               (space (make-string (length prefix) ?\ ))
                               (replace (concat "\n" space "\\1")))
                          (concat
                           prefix
                           (replace-regexp-in-string "\n\\(.\\)" replace item)
                           "\n")))
                      cont))))

      ((eq 'paragraph type)
       (apply 'concat cont)))))

(defun omd-write-to-file (elem file)
  (with-temp-file file
    (insert (omd-to-string string))))

(defun omd-random-insert (elem list)
  "Insert ELEM in LIST at a random position."
  (let* ((pos (omd-rand 0 (length list))))
    (if (= pos 0)
        (cons elem list)
      (let ((cell (nthcdr (1- pos) list)))
        (setcdr cell
                (cons elem (cdr cell))))
      list)))

(defun omd-mutate-elem-list (elem &optional nb)
  "Append NB items at random positions in every list of ELEM."
  (unless nb
    (setq nb 1))
  (let* ((type (nth 0 elem))
         (cont (nthcdr 2 elem)))
    (cond
     ((eq 'list type)
      (omd-set-contents
       elem
       (dotimes (i nb cont)
         (setq cont
               (omd-random-insert (omd-random-text (omd-rand 1 3) 30)
                                  cont)))))

     ((member type '(headline doc))
      (dolist (e cont)
        (omd-mutate-doc-list e nb)))))
  elem)


(defun omd-shuffle-elem (elem &optional recurse)
  "Shuffle the order of the contents of ELEM."
  (when (listp elem)
    (let ((cont
           (map 'list 'identity
                (shuffle-vector
                 (map 'vector 'identity (omd-get-contents elem))))))
      (omd-set-contents elem cont)
      (when recurse
        (dolist (e cont)
          (omd-shuffle-elem e)))
      elem)))

(defun omd-test ()
  ;; original doc is 2 headlines with a list
  (let* ((doc-orig (omd-new-doc
                    (omd-random-headline
                     (omd-random-list))
                    (omd-random-headline)))

         (doc-a (omd-copy doc-orig))
         (doc-b (omd-copy doc-orig)))

    ;; doc A adds 2 items to the list
    (omd-add-contents
     (car (omd-get-contents (car (omd-get-contents doc-a))))
     "new item 1"
     "new item 2")

    ;; doc B adds a new subheadline with a list
    (omd-add-contents
     (second (omd-get-contents doc-b))
     (omd-random-headline
      (omd-random-list)))

    (with-current-buffer (get-buffer-create "omd test")
      (erase-buffer)
      (insert
       (omd-to-string doc-orig)
       "\n\n"
       (omd-to-string doc-a)
       "\n\n"
       (omd-to-string doc-b)))))

(require 'org-sync)

(ert-deftest test-key-to-comparator ()
  (defun my-key (s)
    ; sort by decreasing length and then lexicographically
    `(
      (,(length s) . (=       . >))
      (,s          . (string= . string<))))

  (defun predicate (a b)
    (funcall (key-to-comparator 'my-key) a b))

  (should (equal '("loooooong" "emacs" "hello" "a")
                 (sort '("a" "emacs" "loooooong" "hello") 'predicate))))

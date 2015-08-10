;;; omd-test.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine Romain Dumont

;; Author: Antoine Romain Dumont <antoine.romain.dumont@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash-functional)

(ert-deftest test-omd-rand ()
  (should
   (let ((min 1)
         (max 100))
     (->> (number-sequence 1 10)
          (-map (lambda (_) (omd-rand min max)))
          (--every? (and (<= min it) (< it max)))))))

(ert-deftest test-omd-random-word ()
  (let ((seq-length (number-sequence 1 10)))
    (->> seq-length
         (-map (-compose 'length 'omd-random-word))
         (equal seq-length))))

;; ...

(provide 'omd-test)
;;; omd-test.el ends here

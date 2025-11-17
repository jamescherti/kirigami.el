;;; kirigami.el --- A unified method to fold and unfold text -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/kirigami.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A unified method to fold and unfold text.

;;; Code:

(defgroup kirigami nil
  "A unified method to fold and unfold text"
  :group 'kirigami
  :prefix "kirigami-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/kirigami.el"))

(defcustom kirigami-verbose nil
  "Enable displaying verbose messages."
  :type 'boolean
  :group 'kirigami)

(defun kirigami--message (&rest args)
  "Display a message with the same ARGS arguments as `message'."
  (apply #'message (concat "[kirigami] " (car args)) (cdr args)))

(defmacro kirigami--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  (declare (indent 0) (debug t))
  `(progn
     (when kirigami-verbose
       (kirigami--message
        (concat ,(car args)) ,@(cdr args)))))

;;;###autoload
(define-minor-mode kirigami-mode
  "Toggle `kirigami-mode'."
  :global t
  :lighter " kirigami"
  :group 'kirigami
  (if kirigami-mode
      t
    t))

(provide 'kirigami)
;;; kirigami.el ends here
